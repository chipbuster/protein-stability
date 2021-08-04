using StatsBase
using Statistics
using LinearAlgebra
using HDF5
using Printf
using Base.Threads
using Serialization

include("/home/chipbuster/codes/protein-stability/protein-tools/core/simdata.jl")
using .SimData

include("endpoint_distrib.jl")
include("autocor.jl")

using Plots

filepath = ARGS[1]
file = h5open(filepath,"r")
paths = find_all_dataset(file)

# Create our own read-only datasets by using the default constructor
datasets = [filepath]

function last(path, delim="/")
    split(path,delim)[end]
end

struct AnalysisResult
    datapath::String
    autocor_xs::Vector{Float64}
    autocor_ys::Vector{Float64}
    eeffs::Matrix{Float64}
    bond_dirs::Array{Float64,3}
end

function analyze_dataset(ds::AbstractString)::AnalysisResult
    println("====================================")
    @printf(" Analyzing %s \n", ds)
    println("====================================")
    
    f = h5open(ds,"r")
    inp = f["/inputdata"]
    param = f["/parameterized"]

    println("Computing End Effectors")
    eeffs = end_effector_vec(inp)
    println("Computing bond directions")
    bdirs = bond_ends_vec(inp)
    println("Computing autocov, please be patient...")
    (ac_xs, autocor) = sim_autocor_2d(param)
    return AnalysisResult(ds, ac_xs, autocor, eeffs, bdirs)
end

function analyze_all()
    for ds in datasets
        outfilepath="/home/chipbuster/Spinny/Experiments/02-2020/avsim-2dchain/outputs/stats"
        outfilename=basename(filepath) * ".stats.serial"
        an = analyze_dataset(ds)
        out = joinpath(outfilepath,outfilename)
        println("Writing to $(out)")
        serialize(out, an)
    end
end

analyze_all()        # Uncomment to run full analysis
