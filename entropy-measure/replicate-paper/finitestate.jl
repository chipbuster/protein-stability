# Implement finite state sampling
using StatsBase

const energyMultiples = [1.,2.,70.,80.]
const kB = 1.0

"""
    genSamples(levels, temp, nsamples)

Generate samples from a fixed distribution with level being the energy levels
(multiples of some epsilon) and temp being the temperature.
"""
function genSamples(eMult::Vector{Float64}, T::AbstractFloat, n::Int)::Vector{Int8}
    # Calculate the partition function 
    probs = exp.(-energyMultiples / (kB * T))
    Z = sum(probs)

    # Compute probabilities
    vals = Vector{Int8}(1:4)
    pval = probs / Z
    w = Weights(pval)
    return sample(vals,w,n)
end

# Generate samples for simulation
ntemps = 500
results = Vector{Vector{Int8}}(undef,ntemps)
@inbounds Base.Threads.@threads for t = 1:ntemps
    results[t] = genSamples(energyMultiples, convert(Float64, t), Int(7e6))
end

include("writeoutputs.jl")

outputs = vcat(results...)