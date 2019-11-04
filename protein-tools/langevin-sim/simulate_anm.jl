import BioStructures
import Bio
import Bio.Structure
using LinearAlgebra
using Printf
using Serialization
using Distributed
using Statistics
using StatsBase
using Plots
using LsqFit
const BS = BioStructures   # Name aliases!

#############

const k_B = 1.0   # Boltzmann Constant

"""Encapsulates parameters of the simulation"""
struct SimParameters
    temp::Float64   # The temperature of the system
    ts::Float64     # The timestep of the system
    damp::Float64   # The level of damping in the system
    num_ts::Int     # The total number of timesteps to take
end

"""Return the "default" parameters for a simulation"""
function gen_default_parameters()
    return SimParameters(100.0, 0.0001, 0.95, 1_000_000)
end

"""Encapsulates state of the simulation"""
mutable struct SimState
    N::Int               # The number of atoms
    positions::Matrix{Float64}   # The initial positions of the atoms
    restlens::UpperTriangular{Float64,Matrix{Float64}}  # Restlength of bonds
    coeffs::UpperTriangular{Float64,Matrix{Float64}}    # Spring coefficients
end

# A lookup table used to determine the mass of a given amino acid. Numbers are
# taken from http://www.matrixscience.com/help/aa_help.html, units in amu
const AMINO_MASS = Dict{String,Float64}("ALA" => 71.0779,
  "ARG" => 156.1857,
  "ASN" => 114.1026,
  "ASP" => 115.0874,
  "ASX" => 114.595,  # For ASX, average ASN + ASP (since we don't know which)
  "CYS" => 103.1429,
  "GLU" => 129.114,
  "GLN" => 128.1292,
  "GLX" => 128.6216, # For GLX, average GLN + GLU (since we don't know which)
  "GLY" => 57.0513,
  "HIS" => 137.1393,
  "ILE" => 113.1576,
  "LEU" => 113.1576,
  "LYS" => 128.1723,
  "MET" => 131.1961,
  "PHE" => 147.1739,
  "PRO" => 97.1152,
  "SER" => 87.0773,
  "THR" => 101.1039,
  "SEC" => 150.0379,
  "TRP" => 186.2099,
  "TYR" => 163.1733,
  "VAL" => 99.1311)

"""A simple helper that computes the Index Into Packed 3-vector: assume that a
3xN set of coordinates (indexed from 1) is packed into a 3N-vector with the
first column being 1:3, second being 4:6, etc. This computes the correct
range to get the i-th 3-vector"""
function iip3(i::Int)
    first = 3 * (i - 1) + 1
    last = first + 2
    return first:last
end

"""Read file and return list of calpha atoms and masses of residues.

Returns a tuple (positions,masses), where positions is a 3xN matrix with positions
of calpha atoms in the columns, and masses is an N-vector with the mass of residue
i at the i-th index.
"""
function get_pdb_calpha(struc)
    residues = Bio.Structure.collectresidues(struc)

    # I'm not sure what the cleanest way to build up a matrix in Julia is...for
    # now I'll just make an empty matrix and hcat to it.
    positions = zeros(3, 0)
    masses = Vector{Float64}()
    for res in residues
        c_alpha_set = BS.collectatoms(res, BS.calphaselector)
        if isempty(c_alpha_set)
            # println(BS.resname(res))  # If we're worried about the ignored res
            continue # Not really a residue we care about
        end

        @assert(length(c_alpha_set) == 1, "A single residue has more than one c-alpha atom!")
        ca_atom = c_alpha_set[1]    # The only atom in the set
        atom_pos = BS.coords(ca_atom)
        mass = get(AMINO_MASS, BS.resname(res), 0)
        if mass == 0
            @printf("WARNING: Unknown residue: %s\n", BS.resname(res))
            @printf("Setting mass to a default value\n")
            mass = 100.0
        end

        positions = hcat(positions, atom_pos)
        push!(masses, mass)
    end
    (positions, masses)
end

# The default c-alpha ProDy potential follows a very simple rule: if two calpha
# atoms are within a cutoff distance of each other, they're connected by a spring
# with strength gamma. Otherwise, there is no connection. Units are Angstroms.
"""Create the restlength and spring constants for the ENM"""
function generate_simstate(positions, cutoff = 15.0, gamma = 1.0)
    @assert(cutoff > 4.0, "cutoff is too low for ANM!")
    (_, natoms) = size(positions)

    # Determine the restlengths and connectivity of each pair of atoms
    restlen = UpperTriangular(zeros(natoms, natoms))
    for j in 1:natoms
        for i in 1:j
            pos_i = positions[:,i]
            pos_j = positions[:,j]
            restlen[i,j] = norm(pos_i - pos_j, 2)
        end
    end

    connectivity = restlen .< cutoff
    coeffs = UpperTriangular(connectivity * gamma)
    simstate = SimState(natoms, positions, restlen, coeffs)
end

# Currently used only for plots
"""Compute the energy of a state """
function compute_energy(state::SimState)
    numAtoms = state.N
    config = state.positions
    restdist = state.restlens
    coeffs = state.coeffs

    energy = 0.0

    @inbounds for i in 1:numAtoms
        @inbounds for j in i + 1:numAtoms
            vec_ij  = config[:,j] - config[:,i]
            dist = norm(vec_ij)
            coeff = coeffs[i,j]
            restlen = restdist[i,j]
            energy += 0.5 * coeff * (dist - restlen)^2
        end
    end
    return energy
end

"""Compute the energy of a positional trace. Useful for computing the energies 
of a simulation result. positions is a 3xNxtimesteps 
"""
function compute_energy(positions, state::SimState)
    energy = Vector{Float64}()
    # It's cheaper to change the state when computing energy than cloning a new
    # state...but let's make sure to restore the original state
    for x in eachslice(positions; dims = 3)
        state.positions = x
        push!(energy, compute_energy(state))
    end
    # Restore the orginal position in case the caller cares about it
    state.positions = positions[:,:,1]
    return energy
end

"""Compute the forces on a given protein configuration, given the current
configuration and the spring constants
"""
function compute_forces(state::SimState)
    numAtoms = state.N
    config = state.positions
    restdist = state.restlens
    coeffs = state.coeffs

    forces = zeros(size(config))

    @inbounds for i in 1:numAtoms
        @inbounds for j in i + 1:numAtoms
            vec_ij  = config[:,j] - config[:,i]
            dist_ij = norm(vec_ij)
            # If this vector is zero, we can't normalize it. Fortunately, we can
            # just say the force is zero, since stochastic updates will push it
            # out of the zero condition on the next timestep
            if dist_ij == 0.0
                vec_ij_unit = zeros(size(vec_ij))
            else
                vec_ij_unit = vec_ij / dist_ij
                @assert(norm(vec_ij_unit) - 1.0 < 0.0001, "Normalized vector is not unit!")
            end

            forces[:,i] += coeffs[i,j] * (dist_ij - restdist[i,j]) * vec_ij_unit
            forces[:,j] -= coeffs[i,j] * (dist_ij - restdist[i,j]) * vec_ij_unit
        end
    end
    return forces
end

"""A utility function to generate random 3-d unit vectors"""
function rand_3unit(N)
    output = Matrix{Float64}(undef, 3, N)
    for i in 1:N
        u1 = rand()
        u2 = rand()
        phi = 2 * pi * u2
        lam = acos(2 * u1 - 1.0) - pi / 2
        vec = [ cos(lam) * sin(phi), cos(lam) * cos(phi), sin(lam)]
        output[:,i] = vec

        # It's hard to check for uniformity in angular distribution, but uniformity
        # in length had better be a given.
        @assert(abs(norm(vec) - 1.0) < 1e-4)
    end
    return output
end

"""Take a timestep with explicit Euler

positions = 3xN matrix of point positions
restdist = NxN UpTriMat of rest positions between points
coeffs = NxN UpTriMat of spring coefficients between points
timestep = float, specifies size of timestep
damping = float, specifies damping (gamma in langevin equations)
T = float, specifies temperature
"""
function take_timestep(state::SimState, params::SimParameters)
    timestep = params.ts
    damping = params.damp
    T = params.temp

    # The explicit euler update for forces in the overdamped regime
    natoms = state.N
    update = compute_forces(state) .* timestep ./ damping

    # The stochastic update
    D = k_B * T / damping
    update += sqrt(2 * D * timestep) * randn(size(update))
    state.positions += update
end

"""Run a simulation on the specified PDB file"""
function run_sim(pdbname; params = nothing, temp = -1.0)

    if(length(pdbname) != 4)
        println(pdbname * " does not appear to be a valid PDB name, skipping")
        return nothing
    end

    # We don't trust system PDBs because Maestro can't output valid ones: grab 
    # them from the interweb instead
    main_dir = dirname(@__DIR__())
    download_dir = joinpath(main_dir, "inputs")
    output_dir = joinpath(main_dir, "outputs")

    # Downloads and parses structure in a single line, equivalent to downloadpdb + read
    # println("Reading PDB structure for " * pdbname)
    struc = BS.retrievepdb(pdbname; pdb_dir = download_dir)

    # println("Generating C-Alpha structure for " * pdbname)
    (positions, masses) = get_pdb_calpha(struc)
    simstate = generate_simstate(positions)

    # Add a random perturbation to the initial positions
    simstate.positions = simstate.positions + rand_3unit(simstate.N)

    # If we don't have user-supplied parameters, use the default
    if params === nothing
        params = gen_default_parameters()
    end

    if temp >= 0.0
        params = SimParameters(temp, params.ts, params.damp, params.num_ts)
    end

    @printf("Running %s with %d atoms, T = %f, %d timesteps of size %f \n"
            ,pdbname, simstate.N, params.temp, params.num_ts, params.ts)

    solution = Array{Float64,3}(undef, 3, simstate.N, params.num_ts)
    for t in 1:params.num_ts
        solution[:,:,t] = simstate.positions
        take_timestep(simstate, params)
    end

    # Serialize the outputs, ding 
    return (solution, simstate)
end

""" Creates a simple test which can be checked by hand for correctness.

Test: two points on the z-axis, initially at z = +1 and z=-1. They are perturbed
to +/-1.5 and then allowed to evolve under the system.

With basic overdamped Langevin (no random force), we expect to see them drift
towards each other slowly, until the force becomes almost zero and they stop
moving."""
function simple_test()
    # Points across the Z axis
    positions = [0 0; 0 0; 0 0]
    state = generate_simstate(positions)
    natoms = state.N

    # Initial perturbation
    state.positions += [0 0; 0 0; 0.5 -0.5]

    params = SimParameters(100.0, 0.001, 0.8, 10_000_000)

    solution = zeros(3, state.N, params.num_ts)
    for t in 1:params.num_ts
        solution[:,:,t] = state.positions
        take_timestep(state, params)
    end
    return (solution, state)
end

function compute_histogram(simoutput; name = nothing)
    binct = 1000 # Doesn't matter what it is, as long as we have the same # bins for both
    (sim, state) = simoutput
    energy = compute_energy(sim, state)
    hist = fit(Histogram, energy; nbins = binct)

    # Normalize Histogram
    edges = collect(hist.edges[1])[1:end - 1]
    weights = hist.weights / sum(hist.weights)

    plot(edges, weights)
    #histogram!(energy; normalize = :probability, bins = binct)   # As a sanity check

    @. model(x, p) = p[1] * exp(-x * p[2])
    f = curve_fit(model, edges, weights, [0.01,0.01])
    plot!(edges, model(edges, f.param))
    if name !== nothing
        savefig(name)
    else
    gui()
    end
end

function get_pdbnames(filename)
    names = Vector{String}()
    open(filename) do file
        for name in eachline(file)
            push!(names, name)
        end
    end
    return names
end

