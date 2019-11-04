using Distributed
using Serialization
using Base.Filesystem

@everywhere include("simulate_anm.jl")

# Custom version of this file which only runs one PDB per entry

name   = ARGS[1]
temp_s = ARGS[2]
dir    = ARGS[3]

temp = parse(Float64, temp_s)

println("Running Simulation for " * name)
(trace, sim) = run_sim(name; temp=temp)

tracedir = joinpath(dir, "traces", name * "-" * temp_s * ".trace.serial")
simdir = joinpath(dir, "simstate", name * "-" * temp_s * ".state.serial")

serialize(tracedir, trace)
serialize(simdir, sim)
