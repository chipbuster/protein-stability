using LinearAlgebra
using Printf
using Serialization
using Statistics
using StatsBase
using Base.Threads
using HDF5

#############

const k_B = 1.0   # Boltzmann Constant

"""Encapsulates parameters of the simulation"""
struct SimParameters
    temp::Float64   # The temperature of the system
    ts::Float64     # The timestep of the system
    damp::Float64   # The level of damping in the system
    num_ts::Int     # The total number of timesteps to take
end


"""Encapsulates state of the simulation"""
mutable struct SimState
    N::Int               # The number of atoms
    positions::Matrix{Float64}   # The initial positions of the atoms
    restlens::UpperTriangular{Float64,Matrix{Float64}}  # Restlength of bonds
    coeffs::UpperTriangular{Float64,Matrix{Float64}}    # Spring coefficients
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

"""Run a simulation on the specified initial simulation state.

    Stores output to an HDF dataset provided by `hdf_fname` and `datapath`
    Only stores every `skipn` data steps.
"""
function run_sim(simstate::SimState, hdf_fname::String, datapath::String; params::SimParameters, skipn::Int = 1)
    @assert skipn >= 1 "Trying to skip non-positive number of frames"
    simfile = SimData.InputData(hdf_fname, datapath)
    data = simfile.data
    # Add a random perturbation to the initial positions
    simstate.positions = simstate.positions + rand_3unit(simstate.N)

    @printf("Running with %d atoms, T = %f, %d timesteps of size %f \n",
            simstate.N, params.temp, params.num_ts, params.ts)

    attrs(data)["source"] = "langevin"
    attrs(data)["timestep"] = params.ts
    attrs(data)["temp"] = params.temp
    attrs(data)["damp"] = params.damp

    if skipn > 1
       attrs(data)["skip"] = skipn
    end

    for t in 1:params.num_ts
        # Only record every skipn counts
        if t % skipn == 0
            data[:,:,t] = simstate.positions
        end
        take_timestep(simstate, params)
    end
end

############################
# END SIMULATION FUNCTIONS #
# BEGIN UTILITIY FUNCTIONS #
############################

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

"""Get a list of pdbnames from a file"""
function get_pdbnames(filename)
    names = Vector{String}()
    open(filename) do file
        for name in eachline(file)
            push!(names, name)
        end
    end
    return names
end

"""Parse the input name, possibly splitting it for subselection"""
function parse_pdb_subrange(name)
    # Base PDBs have no characters
    if length(name) == 4
        return (name, nothing)
    end
    
    # We have a subset range specified in the name. Split on the allowed chars
    toks = split(name, ['-','_'])
    if length(toks) != 3
        println("PDB name is not 4 chars, but has wrong form for subrange!")
        exit(1)
    end

    (name, start, fini) = toks
    s = parse(Int, start)
    e = parse(Int, fini)
    (name, (s,e))
end
