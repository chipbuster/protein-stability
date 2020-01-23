using Printf;
if length(ARGS) < 2
    @printf("Usage: %s <hdf5_file> <datapath>\n",PROGRAM_FILE)
    @printf("                      <datapath> can be --all\n")
    exit(1)
end

# Set up python environments
using PyCall;
using HDF5;

core_dir = joinpath(dirname(dirname(abspath(PROGRAM_FILE))), "core")
pushfirst!(PyVector(pyimport("sys")."path"), core_dir)

include(joinpath(core_dir, "simdata.jl"))
using .SimData

measure = pyimport("measure_ratio")

infile = ARGS[1]

datapaths = if ARGS[2] == "--all"
    h5open(infile,"r") do f
        find_all_dataset(f)
    end
    else
        [ARGS[2]]
    end

completed = []

for dp in datapaths
    last_dp = split(dp,"/")[end]
    if last_dp == "inputdata" || last_dp == "parameterized" || last_dp == "binned"
        # Strip off trailing component
        mdp = join(split(dp,"/")[1:end-1],"/")

        if mdp in completed
            continue
        end
    else
        # Assume that this is the dataset group, not a dataset directly
        mdp = dp
    end

    ratio = measure.compute_entropy(infile, mdp)
    push!(completed,mdp)
    @printf("%s computed entropy is %f\n",mdp,ratio)
end
