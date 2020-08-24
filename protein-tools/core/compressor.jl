using Libdl;
using LinearAlgebra;

function get_compressor_function()
    libpath = joinpath(@__DIR__, "simplelzma.so")
    lib = try
        dlopen(libpath)
    catch e
        oldwd = pwd()
        cd(@__DIR__)
        run(`make`)
        cd(oldwd)
        lib = dlopen(libpath)
    end
    dlsym(lib, :get_compressed_size)
end

function compressed_size(data::Vector{UInt8})
    len = length(data)
    func = get_compressor_function()
    csize = ccall(func, Csize_t, (Ptr{UInt8}, Csize_t), data, len)
    convert(Int, csize)
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
            printstyled("[WARN]: Random/input data are not the same length!\n", 
                         bold=true, color=:yellow)
        end
    end
    z = zeros(UInt8, numel)
    C_0 = compressed_size(z)
    C_d = compressed_size(data)
end