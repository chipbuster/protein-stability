if length(ARGS) < 2
    println("Usage: $(PROGRAM_FILE) <hdf5_file> <datapath> <pdbfile> [opts]\n")
    println("")
    println("hdf5_file: path to the HDF5 file where results are stored")
    println("datapath:  path within HDF5 file to dataset")
    println("pdbfile:   location of PDB to base ANM off of")
    println("")
    println("[opts] are in key-value form, separated by a ':'")
    println("Valid [opts]:")
    println("	k:<val>")
    println("	dt:<val>")
    println("	xi:<val>")
    println("	damp:<val>")
    println("	cutoff:<val>")
    println("	bond_mod:<val>")
    println("	nframes:<val>")
    println("	nskip:<val>")
    exit(1)
end

qq = dirname(abspath(PROGRAM_FILE))
include(joinpath(qq, "simulate_anm.jl"))

using BioStructures;
import .SimData

struct SimArgs
    hdf5_filepath::AbstractString
    hdf5_datapath::AbstractString
    pdb_filepath::AbstractString
    bond_cutoff::Float64
    bond_modifier::Float64
    nframes::Integer
    nskip::Integer
    params::SimParameters
end

# The default c-alpha ProDy potential follows a very simple rule: if two calpha
# atoms are within a cutoff distance of each other, they're connected by a spring
# with strength gamma. Otherwise, there is no connection. Units are Angstroms.
"""Create the restlength and spring constants for the ENM"""
function generate_anm_simstate(positions, cutoff=15.0, gamma=1.0)
    @assert(cutoff > 4.0, "cutoff is too low for ANM!")
    (_, natoms) = size(positions)

    # Determine the restlengths and connectivity of each pair of atoms
    restlen = UpperTriangular(zeros(natoms, natoms))
    for j in 1:natoms
        for i in 1:j
            restlen[i,j] = @views norm(pos[:,i] - pos[:,j], 2)
        end
    end

    connectivity = restlen .< cutoff
    coeffs = UpperTriangular(connectivity * gamma)
    simstate = SimState(positions, restlen, coeffs)
end

function parse_args(args::AbstractVector{T})::SimArgs where T <: AbstractString
    # Some default values in case keys are not provided. Since this is a protein
    # simulation, L in eqn of motion is implicitly set at 1 angstrom
    k = 10
    δt = 0.01
    γ = 0.95
    T = 1.0
    bond_mod = 1.0
    cutoff = 15.0
    nframes = 10_000_000
    nskip = 1000
    for arg in args[4:end]
        (key, val) = split(arg, ':')
        if key == "bond_modifier"
            bond_mod = parse(Float64, val)
        elseif key == "bond_cutoff"
            cutoff = parse(Float64, val)
        elseif key == "nskip"
            nskip = parse(Int, val)
        elseif key == "nframes"
            nframes = parse(Int, val)
        elseif key == "bond_strength"
            k = parse(Float64, val)
        elseif key == "temp"
            T = parse(Float64, val)
        elseif key == "timestep"
            δt = parse(Float64, val)
        elseif key == "damping"
            γ = parse(Float64, val)
        else
            error("Unknown key $(key)")
        end
    end

    params = SimParameters(T, δt, γ, k)
    SimArgs(args[1], args[2], args[3], cutoff, bond_mod, nframes, nskip, params)
end

function get_pdb_Cα(filename)
    pdbstruct = read(filename, PDB; remove_disorder=true, read_het_atoms=false)
    residues = collectresidues(pdbstruct)
    Cαs = Matrix{Float64}(undef, 3, length(residues))
    for (i, res) in enumerate(residues)
        Cαs[:,i] = coords(res)
    end
    Cαs
end

function default_anm_from_pdbfile(filename)
    Cαs = get_pdb_Cα(filename)
    generate_anm_simstate(Cαs)
end

function main()
    simargs = parse_args(ARGS)

    Cαs = get_pdb_Cα(simargs.pdb_filepath)
    natoms = size(Cαs, 2)

    # Create datasets needed for simulation
    h5open(simargs.hdf5_filepath, "cw") do file
        d_create(file, simargs.hdf5_datapath * "/pdb_ca", datatype(Float64), dataspace(3, natoms))
        d_create(file, simargs.hdf5_datapath * "/anm_trace", datatype(Float64), dataspace(3, natoms, simargs.nframes))
    end

    hdataset = h5open(simargs.hdf5_datapath, "w")

    hpdb_data = hdataset["/pdb_ca"]
    htrace_data = hdataset["/anm_trace"]

    # Tag the datasets with attributes that are needed to recreate the simulation
    attrs(hpdb_data)["pdbid"] = split(basename(simargs.pdb_filepath), '.')[1]
    attrs(htrace_data)["k"] = simargs.params.k
    attrs(htrace_data)["dt"] = simargs.params.δt
    attrs(htrace_data)["temp"] = simargs.params.T
    attrs(htrace_data)["damp"] = simargs.params.γ
    attrs(htrace_data)["cutoff"] = simargs.bond_cutoff
    attrs(htrace_data)["bondmod"] = simargs.bond_modifier
    attrs(htrace_data)["skip"] = simargs.nskip

    simstate = generate_anm_simstate(Cαs, simargs.bond_cutoff, simargs.bond_modifier)
    hpdb_data = Cαs

    # run_sim(simstate, hdf_fname, datapath; params = params, skipn = skipn)
end
