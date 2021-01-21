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