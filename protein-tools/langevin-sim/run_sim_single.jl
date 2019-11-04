using Serialization
using Base.Filesystem

include("simulate_anm.jl")

# Custom version of this file which only runs one PDB per entry
rawname = ARGS[1]
temp_s  = ARGS[2]
dir     = ARGS[3]

# Parse out arguments
temp = parse(Float64, temp_s)
(name, subrange) = parse_pdb_subrange(rawname)

println("Running Simulation for " * name)
(trace, sim) = run_sim(name; temp=temp, subrange=subrange)

tracefile = joinpath(dir, "traces", name * "-" * temp_s * ".trace.serial")
simfile = joinpath(dir, "simstate", name * "-" * temp_s * ".state.serial")

serialize(tracefile, trace)
serialize(simfile, sim)
