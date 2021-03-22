include("../lib/mod.jl")

using Base.Threads

if ARGS[2] == "gzip"
    protos = []
    errs = []
    for file in readdir(ARGS[1], join=true)
        println("Reading $(file)")
        push!(protos, read_gz_proto(file))
    end

    println("Found $(length(protos)) protos")
    l = SpinLock()
    for proto in protos
        e = validate(proto)
        if !isempty(e)
            lock(l)
            push!(errs, e)
            unlock(l)
        end
    end
        
    if isempty(errs)
        println("No errors found")
    end
    for e in errs
        println(e)
    end
elseif ARGS[2] == "lz77"
    protos = []
    errs = []
    for file in readdir(ARGS[1], join=true)
        println("Reading $(file)")
        push!(protos, read_lz_proto(file))
    end

    println("Found $(length(protos)) protos")
    l = SpinLock()
    for proto in protos
        e = validate(proto)
        if !isempty(e)
            lock(l)
            push!(errs, e)
            unlock(l)
        end
    end
        
    if isempty(errs)
        println("No errors found")
    end
    for e in errs
        println(e)
    end
else
    error("Please provide one of \"gzip\" or \"lz77\"")
end

