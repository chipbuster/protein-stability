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

"""Compute the compressed size by calling an external binary. Has some downsides (incl. mem
usage) but allows for multithreading whereas libLZMA/libXZ are not safe to call multithreaded."""
function compressed_size_external(data::Vector{UInt8}; squelch_err=false)
    dataname = tempname()
    outname = dataname * ".xzraw"
    open(dataname,"w") do f
        write(f, data)
    end

    # Compress the written file to stdout, then read the result to get
    # the # of bytes. Can run out of memory if many large files are
    # being run at once, but even 1GB is pretty extreme right now.
    io = IOBuffer()
    errdest = squelch_err ? devnull : stderr
    run(pipeline(`xz -9e --keep --format=raw --stdout $(dataname)`,stderr=errdest,stdout=io))
    comp_data = take!(io)
    length(comp_data)
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
