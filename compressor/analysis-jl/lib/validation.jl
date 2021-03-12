function validate(sym::Literal)
    return ""
end

function validate(sym::Backreference)
    errs = ""
    if sym.length > 258 || sym.length < 3
        errs += "Symbol length $(sym.length) is out of range"
    end
    if sym.distance > 32768 || sym.length < 1
        errs += "Symbol distance $(sym.length) is out of range"
    end
    errs
end

function validate(sym::OffsetBackreference)
    errs = ""
    if sym.length > 258
        errs += "Symbol length $(sym.length) is out of range"
    end
    if sym.distance > 32768
        errs += "Symbol distance $(sym.length) is out of range"
    end
    errs
end

function validate(block::UncompressedBlock)
    return ""
end

function validate(block::CompressedBlock)
    errs = ""

    for (k, v) in block.lenlit_clen
        if ! 0 <= k <= 286
            errs += "Invalid lenlit key: $(k);"
        end
        if ! 1 <= v <= 16
            errs += "Invalid lenlit length: $(v);"
        end
    end

    for (k, v) in block.dist_clen
        if ! 0 <= k <= 29
            errs += "Invalid dist key: $(k);"
        end
        if ! 1 <= v <= 16
            errs += "Invalid dist length: $(v);"
        end
    end


    for sym in block.syms
        e = validate(sym)
        if !isempty(e)
            errs += e
            errs += ';'
        end
    end
    errs
end

function validate(stream::GZIPStream)
    errs = ""
    for b in stream.blocks
        e = validate(b)
        if !isempty(e)
            errs += e
            errs += ';'
        end
    end
    errs
end

function validate(stream::LZ77Stream)
    errs = ""
    slen = 0
    for sym in stream.syms
        e = validate(sym)
        if !isempty(e)
            errs += e
            errs += ';'
        end

        if isa(sym, Literal)
            slen += 1
        else
            slen += sym.length
        end
    end

    if slen != stream.decoded_len
        errs += "Stream should have been $(stream.decoded_len) but was $(slen);"
    end
    errs
end
function validate_gz_proto(filename)
    stream = read_gz_proto(filename)
    validate(stream)
end

function validate_lz_proto(filename)
    stream = read_lz_proto(filename)
    validate(stream)
end