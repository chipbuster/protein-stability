using Printf

if length(ARGS) != 3
    @printf("Usage: %s <hdf5_file> <datapath> <bincount>\n", PROGRAM_FILE)
    println("   Processes the dataset at <datapath>/inputdata to create both")
    println("   parameterized and binned data according to <bincount>")
    println("")
    @printf("Usage: %s <hdf5_file> --all <bincount>\n", PROGRAM_FILE)
    println("   Processes all datasets in <hdf5_file> that have path /inputdata")
    exit(2)
end

qq = dirname(dirname(abspath(PROGRAM_FILE)))
include(joinpath(qq, "core/convertdata.jl"))

hdf5_path = ARGS[1]
bincount = parse(Int, ARGS[3])

datapaths = if ARGS[2] == "--all"
    h5open(hdf5_path, "r") do f
        find_all_dataset(f)
    end
else
    datapaths = [ARGS[2]]
end

for dp in datapaths
    if split(dp, "/")[end] == "inputdata"
        @printf("Processing %s\n", dp)
        my_dp = dirname(dp)
        id = InputData(hdf5_path, my_dp)
        pd = xyz_to_dihedrals(id)
        bd = bin_dihedrals(pd, bincount)
    else
        @printf("Skipping %s\n", dp)
        continue
    end
end
