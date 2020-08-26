using Libdl;
using LinearAlgebra;

function get_compressor_function()
    libpath = joinpath(@__DIR__, "simplelzma.so")
    lib = try
        dlopen(libpath)
    catch e
        oldwd = pwd()
        cd(@__DIR__)
        println(pwd())
        run(`make -C $(pwd())`)
        cd(oldwd)
        lib = dlopen(libpath)
    end
    return (dlsym(lib, :get_compressed_size), lib)
end

function compressed_size(data::Vector{UInt8})::Int
    len = length(data)
    (func, lib) = get_compressor_function()
    csize = ccall(func, Csize_t, (Ptr{UInt8}, Csize_t), data, len)
    dlclose(lib)
    val = convert(Int, csize)
end

# An overload which allows us to cache the compression of random data if needed.
function compression_eta(data::Vector{UInt8}, C_0::T, C_1::T) where T <: Integer
    """Computes the compression η of the given data using the given C_d.

    Allows us to cache the results of compressing random data. C_0 should be
    the size of compressed zero data, and C_d the size of compressed random data."""

    C_d = compressed_size(data)
    (C_d - C_0) / (C_1 - C_0)
end
    

function compression_eta(data::Vector{UInt8}, random::Union{Vector{UInt8},Nothing}=nothing)
    """Computes the compression η of the given data.
    
    If random is `nothing` (the default), then the "default" random distribution
    of uniform on UInt8 is used. Otherwise, specified distribution is used."""
    numel = length(data)
    if random === nothing
        random = rand(UInt8, numel)
    else
        if (length(random) != numel)
            printstyled("[WARN]: Random/input differ in size! Rand: $(size(random)), Data: $(size(data))\n", 
                         bold=true, color=:yellow)
        end
    end
    z = zeros(UInt8, numel)
    C_0 = compressed_size(z)
    C_1 = compressed_size(random)
    compression_eta(data, C_0, C_1)
end
