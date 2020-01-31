if length(ARGS) < 2
    println("Usage: $(PROGRAM_FILE) <input_fname> <datapath>")
    println("	<datapath> may be the special string '--all' in which case all")
    println("	datasets within the input file will be analyzed.")
    exit(1)
end

using StatsBase
using Printf
using Statistics
using LinearAlgebra
using HDF5
using Base.Threads
using Serialization

const PT_BASEDIR="/home/chipbuster/codes/protein-stability/protein-tools"

include(joinpath(PT_BASEDIR,"core/simdata.jl"))
using .SimData

include(joinpath(PT_BASEDIR,"langevin-sim/analysis_funcs/result.jl"))
include(joinpath(PT_BASEDIR,"langevin-sim/analysis_funcs/bondstats.jl"))
include(joinpath(PT_BASEDIR,"langevin-sim/analysis_funcs/endpoint_distrib.jl"))
include(joinpath(PT_BASEDIR,"langevin-sim/analysis_funcs/autocor.jl"))
include(joinpath(PT_BASEDIR,"langevin-sim/analysis_funcs/fit_gaussian.jl"))

# Process arguments
filepath = ARGS[1]
datapaths = if ARGS[2] == "--all"
    println("Acquiring all datasets in $(filepath)")
    h5open(filepath) do file
        fs = find_all_dataset(file)
        [ join(split(q,"/")[1:end-1], "/") for q in fs ]
    end
else
    [ARGS[2]]
end
outnames = Vector{AbstractString}()
for dp in datapaths                                              # Strip off leading /
    outname = joinpath(dirname(filepath),replace(dp, "/" => "_")[2:end]) * "_stats.serial"
    push!(outnames, outname)
end
    
function analyze_dataset(ds::InputData)::AnalysisResult
    println("====================================")
    @printf(" Analyzing %s \n", ds.datapath)
    println("====================================")

    println("Computing lengths and angles")
    lengths = bond_lengths(ds)
    angles = bond_angles(ds)

    println("Computing autocor")
    (ac_xs, autocor) = sim_autocor(ds)
    println("Computing end effectors")
    eeffs = end_effector_vec(ds)
    
    ident = (ds.filepath, ds.datapath)
    return AnalysisResult(ident, lengths, angles, ac_xs, autocor, eeffs)
end

function analyze_all(filepath, datapaths, outpaths)
    for (dp,op) in zip(datapaths,outpaths)
        println(filepath,dp)
        ds = SimData.InputData(filepath, dp)
        an = analyze_dataset(ds)
        serialize(op, an)
    end
end

analyze_all(filepath, datapaths, outnames)        # Uncomment to run full analysis
