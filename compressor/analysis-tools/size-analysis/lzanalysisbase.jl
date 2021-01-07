## The main suite of LZ analysis tools 

include("lztypes.jl")
include("lzutils.jl")
using JSON;

function get_codelengths(code::HuffCode)
    Dict(k => length(c) for (k, c) in code)
end

function inc_codepoint(x::Literal, lenlits, dists)
    push!(lenlits, x.value)
end

function inc_codepoint(x::Backreference, lenlits, dists)
    push!(lenlits, x.len)
    push!(dists, x.dist)
end

function list_used_codepoints(block::Block)
    lenlits = Set{Int16}()
    dists = Set{Int16}()
    for elem in block.data
        inc_codepoint(elem, lenlits, dists)
    end
    (lenlits, dists)
end


"""Check that block meets validity condition to be analyzed"""
function validate_block(block::Block)
    # Currently just checks that no offsetbackrefs were encountered
    for sym in block.data
        if isa(sym, OffsetBackref)
            return false
        end
    end
    return true
end

# function symbol_frequencies()

"""Strip unused symbols out of the codes"""
function process_block_unused(block::Block)
    lenlits,dists = list_used_codepoints(block)
    for k in keys(block.lenlit_code)
        if k ∉ lenlits
            delete!(block.lenlit_code, k)
        end
    end
    for k in keys(block.dist_code)
        if k ∉ dists
            delete!(block.dist_code, k)
        end
    end
end

function compute_compressed_size(blocks::Vector{Block})
    sum(compute_compressed_size(b) for b in blocks)
end

function compute_compressed_size(block::Block)
    if !validate_block(block)
        return 0
    end
    nbits = 3 # 3 bits for type, ignore hufftree encoding sizes
    for elem in block.data
        nbits += size_lzelem(elem, block.codetrees)
    end
    nbits
end
