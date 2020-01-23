using Printf;

if length(ARGS) < 4
    @printf("Usage: %s <hdf5_file> <datapath> <type> <natoms> [opts]\n", PROGRAM_FILE)
    println("")
    println("hdf5_file: path to the HDF5 file where results are stored")
    println("datapath:  path within HDF5 file to dataset")
    println("type:      either CHAIN or RING")
    println("natoms:      number of atoms in system")
    println("")
    println("[opts] are in key-value form, separated by a ':'")
    println("Valid [opts]:")
    println("	temp:<val>")
    println("	timestep:<val>")
    println("	damp:<val>")
    println("	num_ts:<val>")
    println("	restlen:<val>")
    exit(1)
end

qq = dirname(abspath(PROGRAM_FILE))
include(joinpath(qq,"simulate_anm.jl"))
include(joinpath(dirname(qq), "core/simdata.jl"))

import .SimData

"""Create a chain at rest, folded in half. Note: may create funky results for chainlength < 6"""
function generate_simstate(chainlength::Int, restlen::Float64, gamma::Float64, ring::Bool = false)
    positions = zeros(3, chainlength)
    positions[:,1] = [0;0;0]

    offset = [0;0;restlen]

    # Generate chain by walking out and back
    for i in 2:chainlength
        if i > chainlength ÷ 2
            positions[:,i] = positions[:,i - 1] + offset
        else
            positions[:,i] = positions[:,i - 1] - offset
        end
    end

    # Determine the restlengths and connectivity of each pair of atoms
    restlens = UpperTriangular(zeros(chainlength, chainlength))
    for j in 1:chainlength - 1
            restlens[j,j + 1] = restlen
    end
    if ring
        restlens[1,chainlength] = restlen
    end

    connectivity = restlens .!= 0.0
    coeffs = UpperTriangular(connectivity * gamma)
    simstate = SimState(chainlength, positions, restlens, coeffs)
end

const DEFAULT_TEMP = 10
const DEFAULT_TIMESTEP = 0.05
const DEFAULT_DAMP = 0.95
const DEFAULT_NUM_TS = 10_000_000
const DEFAULT_RESTLEN = 1.0

t = DEFAULT_TEMP
ts = DEFAULT_TIMESTEP
d = DEFAULT_DAMP
n = DEFAULT_NUM_TS
rlen = DEFAULT_RESTLEN

## Argument parsing block
hdf_fname = ARGS[1]
datapath = ARGS[2]
simtype = lowercase(ARGS[3])
natoms = parse(Int, ARGS[4])
for kvpair in ARGS[5:end]
    (key,val) = split(kvpair,":")
    if key == "temp"
        t = parse(Float64, val)
    elseif key == "timestep"
        ts = parse(Float64, val)
    elseif key == "damp"
        d = parse(Float64, val)
    elseif key == "num_ts"
        n = parse(Int, val)
    elseif key == "restlen"
        rlen = parse(Float64, val)
    else
        @printf("Unknown key-value pair \t %s", kvpair)
        exit(1)
    end
end

## Generate initial data
params = SimParameters(t,ts,d,n)
## We need to preallocate the array in the HDF5 file
h5open(hdf_fname,"cw") do file
    # Create a 3 X N X TS dataset, chunking into the appropriate timesteps
    d_create(file, datapath * "/inputdata", datatype(Float64), dataspace(3,natoms,n), "chunk", (3,natoms,1))
end

simfile = SimData.InputData(hdf_fname, datapath)
if simtype == "ring"
    simstate = generate_simstate(natoms, rlen, d, true)
elseif simtype == "chain"
    simstate = generate_simstate(natoms, rlen, d, false)
else
    @printf("Unrecognized sim type: %s\n",simtype)
end

run_sim(simstate, simfile.data; params = params)
