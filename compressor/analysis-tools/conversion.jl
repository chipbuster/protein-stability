struct JitterParams
    natoms::Int
    ext_frac::Float64
    njitter::Int
    jtype::String
end

function get_params_from_fn(filename)
    fn = basename(filename)
    parts = split(fn,'_')
    @assert(parts[1] == "output")
    natoms = parse(Int, parts[2])
    ext_frac = parse(Float64, parts[3])
    njitter = parse(Float64, parts[4])
    jtype = parts[5]
    JitterParams(natoms, ext_frac, njitter, jtype)
end

function get_fn_stem(j::JitterParams, suffix)
    out = ""
    out *= "output_"
    out *= "$(j.natoms)_"
    out *= "$(j.ext_frac)_"
    out *= "$(j.njitter)_"
    out *= "$(j.jtype)_"
    out *= suffix
    out
end

basedir = ARGS[1]
owtdir = ARGS[2]
for type in ("cluster", "spread")
    dir1 = "jitter_$(type)"
    for dir2 in ("gzip", "rawbins", "serial")
        outdir = mkpath(joinpath(owtdir, dir1, dir2))
        jitterns = readdir(joinpath(basedir, dir1, dir2, "nvals"))
        for jn in jitterns
            files = readdir(joinpath(basedir, dir1, dir2, "nvals",jn))
            for fn in files
                parts = split(fn, '_')
                n = parse(Int,parts[2])
                f = parse(Float64, parts[3])
                suffix = parts[4]
                jp = JitterParams(n, f, parse(Int, jn), type)
                outfn = joinpath(outdir, get_fn_stem(jp, suffix))
                oldfn = joinpath(basedir, dir1, dir2, "nvals",jn, fn)
                println("Copying $(oldfn) to $(outfn)")
                cp(oldfn, outfn)
            end
        end
    end
end