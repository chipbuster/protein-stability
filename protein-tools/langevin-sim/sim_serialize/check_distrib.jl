#if length(ARGS) < 2
#    println("Usage: $(PROGRAM_FILE) <input_file>")
#    exit(1)
#end

using Serialization
using StatsBase
using Statistics
using LinearAlgebra
using LsqFit
using Distances

"""Assuming the distribution coming from the histogram has a certain functional
form, fit the best function to the histogram.

func is the function to fit the density to. It must take in two arguments: the
first is the input data, and the second is the parameters. init_guess is the
initial guess for the parameters. See LsqFit docs for more details. An example
input is func = @. model(x, p) = p[1]*exp(-x*p[2]), with init_guess = [0.0,1.0]
"""
function fit_func_to_hist(data::Vector{Float64}, func, init_guess)
    rawhist = fit(Histogram, data, nbins=100)
    hist = normalize(rawhist, mode=:pdf)   # convert hist into a density function
    xdata = midpoints(hist.edges[1])   # Histogram edges have one more element than values
    ydata = hist.weights

    f = curve_fit(func, xdata, ydata, init_guess)
    return (xdata, ydata, coef(f))
end

"""Calculate the divergence between the actual histogram of a simulation and the
best-fit function for it"""
function calc_divergence(data, func, init_param_guess)
    (xpts, y_actual, coeff) = fit_func_to_hist(data, func, init_param_guess)
    y_expected = func(xpts, coeff)
    l2 = sqrt(sum((y_expected - y_actual).^2))
    kl = kl_divergence(y_actual, y_expected)
    return (l2,kl)
end

function get_frame_lengths(frame)
    (_, n) = size(frame)
    [ norm(frame[:,i+1] - frame[:,i]) for i in 1:n-1 ]
end

function get_bond_lengths(filename)
    data = deserialize(filename)
    (_, natom, nstep) = size(data)
    collect(Iterators.flatten([ get_frame_lengths(x) for x in eachslice(data;dims=3) ]))
end

bls = get_bond_lengths(ARGS[1])

# Do some sane parameter estimation
m = mean(bls)
v = var(bls)
prm = [ 1.0 / sqrt(v * 2 * pi) , 1.0 / v, m ]


@. gaus(x, p) = p[1]*x*exp(-(x - p[3])^2*p[2])
(l2, kl) = calc_divergence(bls, gaus, prm)
open(ARGS[1] * ".div","w") do io
    write(io,"l2Divergence $(basename(ARGS[1])) is $(l2)\n")
    write(io,"klDivergence $(basename(ARGS[1])) is $(kl)")
end
