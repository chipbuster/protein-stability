using Base: Int64
struct MCRunInfo
    lo::Float64
    hi::Float64
    gaussWidth::Float64
    numAngles::Int64
    numSteps::Int64
    skipPerStep::Int64
    accept::Int64
    reject::Int64
end

function write_info(io, info)
    write(io, info.lo)
    write(io, info.hi)
    write(io, info.gaussWidth)
    write(io, info.numAngles)
    write(io, info.numSteps)
    write(io, info.skipPerStep)
    write(io, info.accept)
    write(io, info.reject)
end

function read_mc_sim(filename)
    open(filename, "r") do f
        lo = read(f, Float64)
        hi = read(f, Float64)
        gw = read(f, Float64)
        nA = read(f, Int64)
        nS = read(f, Int64)
        skip = read(f, Int64)
        acc = read(f, Int64)
        rej = read(f, Int64)

        println(nA, " ", nS)

        data = Matrix{Float64}(undef, nA, nS)
        read!(f, data)

        set = MCRunInfo(lo, hi, gw, nA, nS, skip, acc, rej)
        (set, data)
    end
end
