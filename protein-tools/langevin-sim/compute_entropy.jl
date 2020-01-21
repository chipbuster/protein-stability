# Set up python environments
using PyCall;
using Printf;
using HDF5;

core_dir = joinpath(dirname(dirname(abspath(PROGRAM_FILE))), "core")
pushfirst!(PyVector(pyimport("sys")."path"), core_dir)

include(joinpath(core_dir, "simdata.jl"))
using .SimData

measure = pyimport("measure_ratio")

if length(ARGS) < 2
    @printf("Usage: %s <hdf5_file> <datapath>\n",PROGRAM_FILE)
    @printf("                      <datapath> can be --all\n")
    exit(1)
end

infile = ARGS[1]

datapaths = if ARGS[2] == "--all"
    h5open(infile,"r") do f
        find_all_dataset(f)
    end
    else
        [ARGS[2]]
    end

for dp in datapaths
    if split(dp,"/")[end] == "binned"
        mdp = join(split(dp,"/")[1:end-1],"/")
        ratio = measure.compute_entropy(infile, mdp)
    @printf("%s computed entropy is %f\n",mdp,ratio)
else
    #@printf("Skipping %s\n",dp)
    #continue
end
end
