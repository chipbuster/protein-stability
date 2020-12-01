using StatsBase
using LsqFit
using LinearAlgebra

"""Fit a Gaussian function to the density of the given samples.

`nrm` determines whether to normalize the area of the gaussian to 1.

Returns an (a,b,c) tuple which can be used to obtain the actual Gaussian"""
function fit_gaussian(entries::Vector{T}, nrm::Bool = false, bins::Int=200) where T <: Number
    hist = fit(Histogram, entries; nbins = bins)
    if nrm
        hist = normalize(hist, :pdf)
    end
    edges = collect(hist.edges[1])[2:end] # Default is extra endpoint on the left
    freqs = hist.weights
    fit_gaussian(edges,freqs)
end

"""Fit a Gaussian to the given (x,y) pairs of the PDF.

The (x,y) pairs will usually be obtained by using fit(Histogram) on the data
and getting the resultant x,y data from the histogram object.

Returns an (a,bc) tuple which can be used to obtain the actual Gaussian"""
function fit_gaussian(values::Vector{T}, frequencies::Vector{S}) where T <: Number where S <: Number
    # Define a gaussian model where p[1] = a, p[2] = b, p[3] = c. For a normalized
    # gaussian, a and c are dependent, but we might not be normalized.
    @. model(x,p) = p[1] * exp(-(x - p[2])^2 / (2 * p[3]^2)) 

    # Get initial guess by looking at bin ranges
    qtr = length(values) ÷ 4
    a_init = sqrt(sum(frequencies))     # Guesstimate as overall mass of freqs
    b_init = values[2 * qtr]   # Take midpoint of bins as estimate of mean
    c_init = values[3 * qtr] - values[1 * qtr]  # Rough estimate of spread as bandwidth
    p_init = [a_init, b_init, c_init]

    fit = curve_fit(model, values, frequencies, p_init)
    coef(fit)
end

"""Given a set of xs, get the gaussian frequencies given by the parameters"""
function params_to_gaussian(params, xs::Vector{T} ) where T <: Number
    @. model(x,p) = p[1] * exp(-(x - p[2])^2 / (2 * p[3]^2)) 
    ret = Vector{Float64}(undef, length(xs))
    @inbounds for i = 1:length(xs)
        ret[i] = model(xs[i],params)
    end
    ret
end
