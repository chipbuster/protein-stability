using Base.Iterators

function literal_values(block::UncompressedBlock)
    [ Literal(x) for x in block.bytes ]
end

function literal_values(block::CompressedBlock)
    [ s.value for s in block.syms if isa(s, Literal) ]
end

function literal_values(stream::GZIPStream)
    flatten([literal_values(b) for b in stream.blocks])
end

function literal_values(stream::LZ77Stream)
    [ s.value for s in stream.syms if isa(s, Literal) ]
end

function backref_dists(block::CompressedBlock)
    z = filter(x -> isa(x, Backreference) || isa(x, OffsetBackreference), block.syms)
    [ s.dist for s in z ]
end

# TODO: Implement backref dists + lens for GZIP and LZ77 Streams
function backref_dists

function backref_lengths(block::CompressedBlock)
    z = filter(x -> isa(x, Backreference) || isa(x, OffsetBackreference), block.syms)
    [ s.dist for s in z ]
end
