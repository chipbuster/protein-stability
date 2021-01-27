struct JitterParams
    natoms::Int
    ext_frac::Float64
    njitter::Int
    sorted::String
    aatype::Int
end

function get_params_from_fn(filename)
    fn = basename(filename)
    parts = split(fn, '_')
    @assert(parts[1] == "output")
    natoms = parse(Int, parts[2])
    ext_frac = parse(Float64, parts[3])
    njitter = parse(Float64, parts[4])
    sorted = parts[5]
    aatype = if parts[6] == "2AA"
        2
    elseif parts[6] == "3AA"
        3
    else
        error("Unknown Angle Type")
    end
    JitterParams(natoms, ext_frac, njitter, sorted, aatype)
end

function get_fn_stem(j::JitterParams, suffix)
    out = ""
    out *= "output_"
    out *= "$(j.natoms)_"
    out *= "$(j.ext_frac)_"
    out *= "$(j.njitter)_"
    out *= "$(j.sorted)_"
    out *= "$(j.aatype)AA_"
    out *= suffix
    out
end