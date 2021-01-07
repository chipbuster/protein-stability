include("lztypes.jl")

function codepoint_for_len(x)
    for cp in LENLIT_CODEPOINT_INFO
        (lo, hi) = cp.val_range
        if lo <= x <= hi
            return cp
        end
    end
    error("No len/lit codepoint found for value $(x)")
end

function codepoint_for_dist(x)
    for cp in DIST_CODEPOINT_INFO
        (lo, hi) = cp.val_range
        if lo <= x <= hi
            return cp
        end
    end
    error("No dist codepoint found for value $(x)")
end

function size_len(sym::Backreference, lenlit_code)
    val = sym.len
    cp = codepoint_for_len(val)
    length(lenlit_code[cp.code]) + cp.extrabits
end

function size_dist(sym::Backreference, dist_code)
    val = sym.dist
    cp = codepoint_for_dist(val)
    length(dist_code[cp.code]) + cp.extrabits
end

"""Compute the number of bits used to express a literal"""
function size_lzelem(sym::Literal, codes::BlockCode)
    length(codes.lenlit_code[sym.value])
end

function size_lzelem(sym::Backreference, codes::BlockCode)
    display(sym)
    size_len(sym, codes.lenlit_code) + size_dist(sym, codes.dist_code)
end