## A class to extract simulation parameter information from a filename.
#  Note that this is a skeleton function and will likely be edited to
#  fit the file that's being worked on.

struct FNameParam
    natoms::Int
    ext_frac::Float64
    njit::Int
    sorted::Bool
    aatype::Int
    eedist::Float64
end

FNameParam(filename) = begin
    fn = basename(filename)
    parts = split(fn, '_')
    natoms = parse(parts[2], Int)
    ext_frac = parse(parts[3], Float64)
    njit = parse(parts[4], Int)
    sorted = parts[5] == "sorted"
    aatype = parse(parts[6][1], Int)
    eedist = (natoms - 1) * ext_frac
    FNameParam(natoms, ext_frac, njit, sorted, aatype, eedist)
end