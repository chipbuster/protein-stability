include("size-analysis/read_input/lzinput.jl")
include("size-analysis/lzutils.jl")


test1_dat = [Literal(1), Literal(2), Literal(3), Literal(4), Backreference(2, 3)]
z = expand_lzelem(test1_dat)
display(z)


(info, dat) = read_lz_proto(ARGS[1])
lzelems = get_lzstream(dat.blocks)
uints = expand_lzelem(lzelems)

println(String(uints))