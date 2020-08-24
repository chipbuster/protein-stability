using Libdl;

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

function compress_data(data::Vector{UInt8})
    len = length(data)
    func = get_compressor_function()
    csize = ccall(func, Csize_t, (Ptr{UInt8}, Csize_t), data, len)
    convert(Int, csize)
end