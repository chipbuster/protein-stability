if length(ARGS) < 2
    println("Usage: $(PROGRAM_FILE) <hdf5_file> <datapath> [additional args]")
    println("                      <datapath> can be --all\n")
    println("Additional args:")
    println("   skip:<int>  Additional frames to skip, default is 1")
    exit(1)
end

# Set up python environments
using PyCall;
using HDF5;
using Printf;

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

nskip = 1
if length(ARGS) == 3
    (tok, val) = split(strip(ARGS[3]),":")
    if tok == "skip"
        nskip = parse(Int, val)
    else
        println!("Unrecognized token $(tok), aborting")
    end
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

    ratio = measure.compute_entropy(infile, mdp, nskip)
    push!(completed,mdp)
    @printf("%s computed entropy is %f\n",mdp,ratio)
end
