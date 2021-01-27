## The main suite of LZ analysis tools 

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

"""Strip unused symbols out of the codes. Useful for interpreting Fix blocks"""
function process_block_unused(block::Block)
    lenlits, dists = list_used_codepoints(block)
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


"""Compute the number of bits in the compressed data (minus GZIP + block headers)"""
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

function compute_compressed_size(blocks::Vector{Block})
    sum(compute_compressed_size(b) for b in blocks)
end

"""Gather the distribution of backreference distances in a block"""
function backreference_distances(block::Block)
    backrefs = filter(x -> isa(x, Backreference), block.data)
    [ b.dist for b in backrefs ]
end

function backreference_distances(blocks::Vector{Block})
    vcat((backreference_distances(b) for b in blocks)...)
end

"""Gather the distribution of backreference lengths in a block"""
function backreference_lengths(block::Block)
    backrefs = filter(x -> isa(x, Backreference), block.data)
    [ b.len for b in backrefs ]
end

function backreference_lengths(blocks::Vector{Block})
    vcat((backreference_lengths(b) for b in blocks)...)
end

"""Compute how many bytes the uncompressed data is."""
function num_bytes_uncompressed(block::Block)
    bytes = 0
    for sym in block.data
        if isa(sym, Literal)
            bytes += 1
        else
            bytes += sym.len
        end
    end
    bytes
    end

function num_bytes_uncompressed(blocks::Vector{Block})
    sum(num_bytes_uncompressed(b) for b in blocks)
end

"""Determine how many compressed bits are used by literals"""
function literal_bits_compressed(block::Block)
    lits = filter(x -> isa(x, Literal), block.data)
    sum(size_lzelem(l, block.codetrees) for l in lits)
end

function literal_bits_compressed(blocks::Vector{Block})
    sum(literal_bits_compressed(b) for b in blocks)
end

"""Determine how many compressed bits are used by backreferences"""
function backreference_bits_compressed(block::Block)
    brefs = filter(x -> isa(x, Backreference), block.data)
    sum(size_lzelem(l, block.codetrees) for l in brefs)
end

function backreference_bits_compressed(blocks::Vector{Block})
    sum(backreference_bits_compressed(b) for b in blocks)
end

"""Compute how much space savings is gained through backreference use"""
function backreference_bits_saved(block::Block)
    bits_saved = 0
    brefs = filter(x -> isa(x, Backreference), block.data)
    for bref in brefs
        uncompressed_bits = bref.len * 8
        compressed_bits = size_lzelem(bref, block.codetrees)
        bits_saved += uncompressed_bits - compressed_bits
    end
    bits_saved
end

function backreference_bits_saved(blocks::Vector{Block})
    sum(backreference_bits_saved(b) for b in blocks)
end