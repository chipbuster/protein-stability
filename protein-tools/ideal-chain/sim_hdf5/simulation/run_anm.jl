const DEFAULT_C1 = 0.45
const DEFAULT_C2 = 0.05
const DEFAULT_NUM_TS = 1_000_000
const DEFAULT_RESTLEN = 1.0
const DEFAULT_SKIPN = 1000

if length(ARGS) < 2
    @printf("Usage: %s <hdf5_file> <datapath> [opts]\n", PROGRAM_FILE)
    println("")
    println("hdf5_file: path to the HDF5 file where results are stored")
    println("datapath:  path within HDF5 file to dataset")
    println("")
    println("[opts] are in key-value form, separated by a ':'")
    println("Valid [opts]:")
    println("	c1:<val>")
    println("	c2:<val>")
    println("	nframes:<val>")
    println("	restlen:<val>")
    println("	nskip:<val>")
    exit(1)
end

qq = dirname(abspath(PROGRAM_FILE))
include(joinpath(qq,"simulate_anm.jl"))
include(joinpath(dirname(qq), "core/simdata.jl"))

import .SimData

# The default c-alpha ProDy potential follows a very simple rule: if two calpha
# atoms are within a cutoff distance of each other, they're connected by a spring
# with strength gamma. Otherwise, there is no connection. Units are Angstroms.
"""Create the restlength and spring constants for the ENM"""
function generate_anm_simstate(positions, cutoff=15.0, gamma=1.0, params::SimParameters)
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

c1 = DEFAULT_C1
c2 = DEFAULT_C2
n = DEFAULT_NUM_TS
rlen = DEFAULT_RESTLEN
skipn = DEFAULT_SKIPN

## Argument parsing block
hdf_fname = ARGS[1]
datapath = ARGS[2]
simtype = lowercase(ARGS[3])
natoms = parse(Int, ARGS[4])
for kvpair in ARGS[5:end]
    (key,val) = split(kvpair,":")
    if key == "c1"
        global c1 = parse(Float64, val)
    elseif key == "c2"
        global c2 = parse(Float64, val)
    elseif key == "nframes"
        global n = parse(Int, val)
    elseif key == "restlen"
        global rlen = parse(Float64, val)
    elseif key == "nskip"
        global skipn = parse(Int, val)
    else
        @printf("Unknown key-value pair \t %s", kvpair)
        exit(1)
    end
end

## Generate initial data
params = SimParameters(t,ts,d,n)
println(params)
## We need to preallocate the array in the HDF5 file
h5open(hdf_fname,"cw") do file
    # Create a 3 X N X TS dataset, chunking into the appropriate timesteps
    d_create(file, datapath * "/inputdata", datatype(Float64), dataspace(3,natoms,n))
end

if simtype == "ring"
    simstate = generate_simstate(natoms, rlen, d, true)
elseif simtype == "chain"
    simstate = generate_simstate(natoms, rlen, d, false)
else
    @printf("Unrecognized sim type: %s\n",simtype)
end

run_sim(simstate, hdf_fname, datapath; params = params, skipn = skipn)
