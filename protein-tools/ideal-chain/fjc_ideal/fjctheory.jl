using QuadGK;
using SpecialFunctions;
using StaticArrays;
using LinearAlgebra;

#= 
Compute the distribution of the freely-jointed chain in 2D and 3D.

2D form taken from Dmitrii's notes
3D form taken from Kloczkowski and Kolinski (also not complete yet)
=#

"""Compute the probability that the end-to-end vector has scalar magnitude r."""
function p_fjc(r, nbonds; bondlen=1.0, dimension=2)
    if dimension != 2
        error("Other dimensions not yet implemented")
    end
end

"""Return an appropriate function representing the PDF of the chain.

This version returns a function that takes in a scalar r and outputs the
probability that the FJC has an end-to-end vector with |R| = r.
"""
function p_fjc_2d_rscalar(nbonds; bondlen=1.0)
    function pdf(r::Float64)
        function integrand(k::Float64)
            bcontrib = besselj0(k * bondlen)
            k * bcontrib^nbonds * besselj0(k * r)
        end

        pRvec, err = quadgk(integrand, 0, 1000, rtol=1e-8)
        2 * pi * r * pRvec
    end
    norm, err2 = quadgk(pdf, 0, nbonds-1, rtol=1e-8)
    
    normpdf(r) = pdf(r) / norm
    return normpdf
end
