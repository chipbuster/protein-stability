using BioStructures
using LinearAlgebra
using Printf
using DifferentialEquations
using Serialization
using Distributed
#using Filesystem

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

"""Read file and return list of calpha atoms and masses of residues.

Returns a tuple (positions,masses), where positions is a 3xN matrix with positions
of calpha atoms in the columns, and masses is an N-vector with the mass of residue
i at the i-th index.
"""
function get_pdb_calpha(struc)
    residues = collectresidues(struc)

    # I'm not sure what the cleanest way to build up a matrix in Julia is...for
    # now I'll just make an empty matrix and hcat to it.
    positions = zeros(3, 0)
    masses = Vector{Float64}()
    for res in residues
        c_alpha_set = collectatoms(res, calphaselector)
        if isempty(c_alpha_set)
#            println(resname(res))  # If we're worried about the ignored res
            continue # Not really a residue we care about
        end

        @assert(length(c_alpha_set) == 1, "A single residue has more than one c-alpha atom!")
        ca_atom = c_alpha_set[1]    # The only atom in the set
        atom_pos = coords(ca_atom)
        mass = get(AMINO_MASS, resname(res), 0)
        if mass == 0
            @printf("WARNING: Unknown residue: %s\n", resname(res))
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
"""Generate a Hamiltonian for a given protein.

This function proceeds in three stages:

    1. Determine connectivity of atoms in elastic model
    2. Create closures capturing the kinetic and potential energies
    3. Create and return closure corresponding to Hamiltonian
"""
@noinline function create_hamiltonian(positions, masses, cutoff = 15.0, gamma = 1.0)
    @assert(cutoff > 4.0, "cutoff is too low for ANM!")
    natoms = length(masses)

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

    # Create kinetic energy function, which takes in momentum and outputs K.E.
    function kinetic_energy(q)
        per_particle = reshape(q, 3, :)
        inv_mass_mat = Diagonal(1 ./ (2 .* masses))
        ke_mat = per_particle * inv_mass_mat * per_particle'
        return sum(diag(ke_mat))
    end

    # Create potential energy function, which takes in positions and outputs PE
    function potential_energy(p)
        atoms = reshape(p, 3, :)
        total_pe = 0.0
        for j in 1:natoms
            for i in 1:j
                # If atoms are connected *in the rest pose* (not in the current
                # configuration!), add their P.E. to the contribution
                if connectivity[i,j]
                    dist = norm(atoms[:,i] - atoms[:,j], 2)
                    energy = gamma * (dist - restlen[i,j])^2
                    total_pe += energy
                end
            end
        end
        return total_pe
    end

    # Create Hamiltonian
    function hamiltonian(p, q, params = [])
        ke = kinetic_energy(p)
        pe = potential_energy(q)
        return ke + pe
    end

    return hamiltonian
end

# https://math.stackexchange.com/a/1586185
"""A utility function to generate random 3-d unit vectors"""
function rand_3unit(N)
    output = zeros(3 * N)
    for i in 1:N
        j = ((i - 1) * 3 + 1)
        u1 = rand()
        u2 = rand()
        phi = 2 * pi * u2
        lam = acos(2 * u1 - 1.0) - pi / 2
        vec = [ cos(lam) * sin(phi), cos(lam) * cos(phi), sin(lam)]

        # It's hard to check for uniformity in angular distribution, but uniformity
        # in length had better be a given.
        @assert(abs(norm(vec) - 1.0) < 1e-4)
        output[j:j + 2] = vec
    end
    return output
end

"""Given a Hamiltonian solution, gets the positions as a 3xNxTimesteps matrix

result[:,:,i] is the 3xN result of the i-th timestep
"""
function extract_positions(hamiltonian_sol)
    (numel, numtimesteps) = size(hamiltonian_sol)
    # Size must be divisible by 2 (to get momentum and positions components) and then
    # each solution must have size divisible by 3 (for 3 coordinates per element)
    @assert(numel % 6 == 0, "Wrong number of elements for a 3D Hamiltonian Solution")
    numcoords = numel ÷ 2
    numatoms = numcoords ÷ 3
    result = Array{Float64,3}(undef, 3, numatoms, numtimesteps)
    for i in 1:numtimesteps
        # Extract the positions, which are in the second half of the solution vector
        data = hamiltonian_sol[numcoords + 1:end, i]
        result[:,:,i] = reshape(data, (3, numatoms))
    end
    return result
end

"""Run a simulation on the specified PDB file"""
function run_sim(pdbname)

    if(length(pdbname) != 4)
        println(pdbname * " does not appear to be a valid PDB name, skipping")
        return nothing
    end

    # We don't trust system PDBs because Maestro can't output valid ones: grab 
    # them from the interweb instead
    main_dir = dirname(@__DIR__())
    download_dir = joinpath( main_dir,"inputs" )
    output_dir = joinpath( main_dir,"outputs" )

    # Downloads and parses structure in a single line, equivalent to downloadpdb + read
    println("Reading PDB structure for " * pdbname)
    struc = retrievepdb(pdbname; pdb_dir=download_dir)

    println("Generating C-Alpha structure for " * pdbname)
    (positions, masses) = get_pdb_calpha(struc)
    natoms = length(masses)
    println("Creating Hamiltonian operator for " * pdbname * " ANM model")
    ham_op = create_hamiltonian(positions, masses)

    ## Next we do some (quick) sanity checks on the resultant hamiltonian.
    # Test: zero displacement and momentum should give us zero energy
    println("Testing sanity of Hamiltonian for " * pdbname)
    energy = ham_op(zeros(size(positions)), positions)
    @assert(energy == 0.0, "Energy in rest position with no motion is nonzero!")
    # Test: zero displacement and unit momentum should give us energy proportional
    # to the reciprocal sum of the masses
    unit_randoms = rand_3unit(natoms)
    energy = ham_op(unit_randoms, positions)
    inv_sum_masses = sum(1.0 ./ (2.0 .* masses))
    @assert(abs(energy - inv_sum_masses) < 0.001 * inv_sum_masses,
              "Energy with unit momenta does not appear to be correct!")

    # Let's build a dynamical system using semi-randomized momenta and positions
    initial_momentum = rand_3unit(natoms)      # Randomize initial momenta
    positions = positions + reshape(rand_3unit(natoms), size(positions)) # Randomize initial positions as well
    @printf("Setting up and solving problem for %s with %d atoms\n", pdbname, natoms)
    prob = HamiltonianProblem(ham_op, initial_momentum, positions, (0.0, 10000))
    sol = solve(prob, Tsit5(), saveat = 1.0; progressbar=true)

    # Serialize the solution in two ways: with DiffEQ and as a simple matrix
    # The former contains all the information, but requires 
    matrixsol = extract_positions(sol)
    serialize(joinpath(output_dir, "diffeqout", pdbname * ".serial"), sol)
    serialize(joinpath(output_dir, "matrixout", pdbname * ".serial"), matrixsol)
    return 1
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