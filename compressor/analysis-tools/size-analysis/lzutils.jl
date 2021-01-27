using Base.Iterators;

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
    size_len(sym, codes.lenlit_code) + size_dist(sym, codes.dist_code)
end

function get_lzstream(blocks)
    out = Vector{LZElem}()
    for block in blocks
        append!(out, block.data)
    end
    out
end

function expand_backref(output_so_far, elem)
    l = elem.len
    d = elem.dist
    last_index = length(output_so_far)

    if d > last_index
        display(output_so_far)
        display(elem)
        error("Reference past start: dist $(d) on length-$(last_index)")
    end

    begin_index = last_index - d + 1
    end_index = min(last_index, begin_index + l - 1)
    repeated = output_so_far[begin_index:end_index]

    if d > l
        repeated
    else
        take(cycle(repeated), l)
    end
end

function expand_lzelem(dat::Vector{LZElem})
    output = Vector{UInt8}()
    for input_elem in dat
        if isa(input_elem, Literal)
            push!(output, input_elem.value)
        elseif isa(input_elem, Backreference)
            new = expand_backref(output, input_elem)
            @assert(length(new) == input_elem.len, "Got $(length(new)) elements on a backref of length $(input_elem.len), dist $(input_elem.dist)")
            append!(output, new)
        elseif isa(input_elem, OffsetBackref)
            new = expand_backref(output, input_elem)
            append!(output, new .+ input_elem.offset)
        else
            error("Unknown LZElem type")
        end
    end
    output
end