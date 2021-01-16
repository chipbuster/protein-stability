qq = dirname(dirname(abspath(PROGRAM_FILE)))
include(joinpath(qq, "core/convertdata.jl"))

using Printf

if length(ARGS) < 2
    @printf("Usage: %s <infile> <skipnum> [outfile]", PROGRAM_FILE)
    exit(1)
end

outf = if length(ARGS) == 3
    ARGS[3]
else
    ""
end

skip = parse(Int,ARGS[2])
infile = ARGS[1]

create_dataskip_file(infile, skip, outf)
