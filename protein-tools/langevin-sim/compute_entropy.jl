# Set up python environments
using PyCall;

core_dir = joinpath(dirname(dirname(abspath(PROGRAM_FILE))), "core")
pushfirst!(PyVector(pyimport("sys")."path"), core_dir)

measure = pyimport("measure_ratio")

ratio = measure.compute_entropy(ARGS[1], ARGS[2])
println(ratio)