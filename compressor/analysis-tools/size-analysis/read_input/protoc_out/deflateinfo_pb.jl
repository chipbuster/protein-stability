# syntax: proto3
using ProtoBuf
import ProtoBuf.meta

mutable struct Literal <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function Literal(; kwargs...)
        obj = new(meta(Literal), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct Literal
const __meta_Literal = Ref{ProtoMeta}()
function meta(::Type{Literal})
    ProtoBuf.metalock() do
        if !isassigned(__meta_Literal)
            __meta_Literal[] = target = ProtoMeta(Literal)
            allflds = Pair{Symbol,Union{Type,String}}[:value => UInt32]
            meta(target, Literal, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_Literal[]
    end
end
function Base.getproperty(obj::Literal, name::Symbol)
    if name === :value
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    else
        getfield(obj, name)
    end
end

mutable struct Backref <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function Backref(; kwargs...)
        obj = new(meta(Backref), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct Backref
const __meta_Backref = Ref{ProtoMeta}()
function meta(::Type{Backref})
    ProtoBuf.metalock() do
        if !isassigned(__meta_Backref)
            __meta_Backref[] = target = ProtoMeta(Backref)
            allflds = Pair{Symbol,Union{Type,String}}[:length => UInt32, :distance => UInt32]
            meta(target, Backref, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_Backref[]
    end
end
function Base.getproperty(obj::Backref, name::Symbol)
    if name === :length
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    elseif name === :distance
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    else
        getfield(obj, name)
    end
end

mutable struct OffsetBackref <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function OffsetBackref(; kwargs...)
        obj = new(meta(OffsetBackref), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct OffsetBackref
const __meta_OffsetBackref = Ref{ProtoMeta}()
function meta(::Type{OffsetBackref})
    ProtoBuf.metalock() do
        if !isassigned(__meta_OffsetBackref)
            __meta_OffsetBackref[] = target = ProtoMeta(OffsetBackref)
            allflds = Pair{Symbol,Union{Type,String}}[:offset => UInt32, :length => UInt32, :distance => UInt32]
            meta(target, OffsetBackref, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_OffsetBackref[]
    end
end
function Base.getproperty(obj::OffsetBackref, name::Symbol)
    if name === :offset
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    elseif name === :length
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    elseif name === :distance
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    else
        getfield(obj, name)
    end
end

mutable struct DeflateSym <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function DeflateSym(; kwargs...)
        obj = new(meta(DeflateSym), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct DeflateSym
const __meta_DeflateSym = Ref{ProtoMeta}()
function meta(::Type{DeflateSym})
    ProtoBuf.metalock() do
        if !isassigned(__meta_DeflateSym)
            __meta_DeflateSym[] = target = ProtoMeta(DeflateSym)
            allflds = Pair{Symbol,Union{Type,String}}[:lit => Literal, :backref => Backref, :offset => OffsetBackref]
            oneofs = Int[1,1,1]
            oneof_names = Symbol[Symbol("sym")]
            meta(target, DeflateSym, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, oneofs, oneof_names)
        end
        __meta_DeflateSym[]
    end
end
function Base.getproperty(obj::DeflateSym, name::Symbol)
    if name === :lit
        return (obj.__protobuf_jl_internal_values[name])::Literal
    elseif name === :backref
        return (obj.__protobuf_jl_internal_values[name])::Backref
    elseif name === :offset
        return (obj.__protobuf_jl_internal_values[name])::OffsetBackref
    else
        getfield(obj, name)
    end
end

mutable struct UncompressedBlock <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function UncompressedBlock(; kwargs...)
        obj = new(meta(UncompressedBlock), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct UncompressedBlock
const __meta_UncompressedBlock = Ref{ProtoMeta}()
function meta(::Type{UncompressedBlock})
    ProtoBuf.metalock() do
        if !isassigned(__meta_UncompressedBlock)
            __meta_UncompressedBlock[] = target = ProtoMeta(UncompressedBlock)
            allflds = Pair{Symbol,Union{Type,String}}[:data => Array{UInt8,1}]
            meta(target, UncompressedBlock, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_UncompressedBlock[]
    end
end
function Base.getproperty(obj::UncompressedBlock, name::Symbol)
    if name === :data
        return (obj.__protobuf_jl_internal_values[name])::Array{UInt8,1}
    else
        getfield(obj, name)
    end
end

mutable struct CompressedBlock_LenlitCodelenEntry <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function CompressedBlock_LenlitCodelenEntry(; kwargs...)
        obj = new(meta(CompressedBlock_LenlitCodelenEntry), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct CompressedBlock_LenlitCodelenEntry (mapentry)
const __meta_CompressedBlock_LenlitCodelenEntry = Ref{ProtoMeta}()
function meta(::Type{CompressedBlock_LenlitCodelenEntry})
    ProtoBuf.metalock() do
        if !isassigned(__meta_CompressedBlock_LenlitCodelenEntry)
            __meta_CompressedBlock_LenlitCodelenEntry[] = target = ProtoMeta(CompressedBlock_LenlitCodelenEntry)
            allflds = Pair{Symbol,Union{Type,String}}[:key => UInt32, :value => UInt32]
            meta(target, CompressedBlock_LenlitCodelenEntry, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_CompressedBlock_LenlitCodelenEntry[]
    end
end
function Base.getproperty(obj::CompressedBlock_LenlitCodelenEntry, name::Symbol)
    if name === :key
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    elseif name === :value
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    else
        getfield(obj, name)
    end
end

mutable struct CompressedBlock_DistCodelenEntry <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function CompressedBlock_DistCodelenEntry(; kwargs...)
        obj = new(meta(CompressedBlock_DistCodelenEntry), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct CompressedBlock_DistCodelenEntry (mapentry)
const __meta_CompressedBlock_DistCodelenEntry = Ref{ProtoMeta}()
function meta(::Type{CompressedBlock_DistCodelenEntry})
    ProtoBuf.metalock() do
        if !isassigned(__meta_CompressedBlock_DistCodelenEntry)
            __meta_CompressedBlock_DistCodelenEntry[] = target = ProtoMeta(CompressedBlock_DistCodelenEntry)
            allflds = Pair{Symbol,Union{Type,String}}[:key => UInt32, :value => UInt32]
            meta(target, CompressedBlock_DistCodelenEntry, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_CompressedBlock_DistCodelenEntry[]
    end
end
function Base.getproperty(obj::CompressedBlock_DistCodelenEntry, name::Symbol)
    if name === :key
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    elseif name === :value
        return (obj.__protobuf_jl_internal_values[name])::UInt32
    else
        getfield(obj, name)
    end
end

mutable struct CompressedBlock <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function CompressedBlock(; kwargs...)
        obj = new(meta(CompressedBlock), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct CompressedBlock
const __meta_CompressedBlock = Ref{ProtoMeta}()
function meta(::Type{CompressedBlock})
    ProtoBuf.metalock() do
        if !isassigned(__meta_CompressedBlock)
            __meta_CompressedBlock[] = target = ProtoMeta(CompressedBlock)
            fnum = Int[3,4,1]
            allflds = Pair{Symbol,Union{Type,String}}[:lenlit_codelen => Base.Dict{UInt32,UInt32}, :dist_codelen => Base.Dict{UInt32,UInt32}, :data => Base.Vector{DeflateSym}]
            meta(target, CompressedBlock, allflds, ProtoBuf.DEF_REQ, fnum, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_CompressedBlock[]
    end
end
function Base.getproperty(obj::CompressedBlock, name::Symbol)
    if name === :lenlit_codelen
        return (obj.__protobuf_jl_internal_values[name])::Base.Dict{UInt32,UInt32}
    elseif name === :dist_codelen
        return (obj.__protobuf_jl_internal_values[name])::Base.Dict{UInt32,UInt32}
    elseif name === :data
        return (obj.__protobuf_jl_internal_values[name])::Base.Vector{DeflateSym}
    else
        getfield(obj, name)
    end
end

mutable struct UnderlyingBlock <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function UnderlyingBlock(; kwargs...)
        obj = new(meta(UnderlyingBlock), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct UnderlyingBlock
const __meta_UnderlyingBlock = Ref{ProtoMeta}()
function meta(::Type{UnderlyingBlock})
    ProtoBuf.metalock() do
        if !isassigned(__meta_UnderlyingBlock)
            __meta_UnderlyingBlock[] = target = ProtoMeta(UnderlyingBlock)
            fnum = Int[10,1]
            allflds = Pair{Symbol,Union{Type,String}}[:raw => UncompressedBlock, :block => CompressedBlock]
            oneofs = Int[1,1]
            oneof_names = Symbol[Symbol("data")]
            meta(target, UnderlyingBlock, allflds, ProtoBuf.DEF_REQ, fnum, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, oneofs, oneof_names)
        end
        __meta_UnderlyingBlock[]
    end
end
function Base.getproperty(obj::UnderlyingBlock, name::Symbol)
    if name === :raw
        return (obj.__protobuf_jl_internal_values[name])::UncompressedBlock
    elseif name === :block
        return (obj.__protobuf_jl_internal_values[name])::CompressedBlock
    else
        getfield(obj, name)
    end
end

mutable struct DEFLATEBlock <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function DEFLATEBlock(; kwargs...)
        obj = new(meta(DEFLATEBlock), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct DEFLATEBlock
const __meta_DEFLATEBlock = Ref{ProtoMeta}()
function meta(::Type{DEFLATEBlock})
    ProtoBuf.metalock() do
        if !isassigned(__meta_DEFLATEBlock)
            __meta_DEFLATEBlock[] = target = ProtoMeta(DEFLATEBlock)
            allflds = Pair{Symbol,Union{Type,String}}[:bfinal => Bool, :data => UnderlyingBlock]
            meta(target, DEFLATEBlock, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_DEFLATEBlock[]
    end
end
function Base.getproperty(obj::DEFLATEBlock, name::Symbol)
    if name === :bfinal
        return (obj.__protobuf_jl_internal_values[name])::Bool
    elseif name === :data
        return (obj.__protobuf_jl_internal_values[name])::UnderlyingBlock
    else
        getfield(obj, name)
    end
end

mutable struct DEFLATEStream <: ProtoType
    __protobuf_jl_internal_meta::ProtoMeta
    __protobuf_jl_internal_values::Dict{Symbol,Any}
    __protobuf_jl_internal_defaultset::Set{Symbol}

    function DEFLATEStream(; kwargs...)
        obj = new(meta(DEFLATEStream), Dict{Symbol,Any}(), Set{Symbol}())
        values = obj.__protobuf_jl_internal_values
        symdict = obj.__protobuf_jl_internal_meta.symdict
        for nv in kwargs
            fldname, fldval = nv
            fldtype = symdict[fldname].jtyp
            (fldname in keys(symdict)) || error(string(typeof(obj), " has no field with name ", fldname))
            values[fldname] = isa(fldval, fldtype) ? fldval : convert(fldtype, fldval)
        end
        obj
    end
end # mutable struct DEFLATEStream
const __meta_DEFLATEStream = Ref{ProtoMeta}()
function meta(::Type{DEFLATEStream})
    ProtoBuf.metalock() do
        if !isassigned(__meta_DEFLATEStream)
            __meta_DEFLATEStream[] = target = ProtoMeta(DEFLATEStream)
            allflds = Pair{Symbol,Union{Type,String}}[:blocks => Base.Vector{DEFLATEBlock}]
            meta(target, DEFLATEStream, allflds, ProtoBuf.DEF_REQ, ProtoBuf.DEF_FNUM, ProtoBuf.DEF_VAL, ProtoBuf.DEF_PACK, ProtoBuf.DEF_WTYPES, ProtoBuf.DEF_ONEOFS, ProtoBuf.DEF_ONEOF_NAMES)
        end
        __meta_DEFLATEStream[]
    end
end
function Base.getproperty(obj::DEFLATEStream, name::Symbol)
    if name === :blocks
        return (obj.__protobuf_jl_internal_values[name])::Base.Vector{DEFLATEBlock}
    else
        getfield(obj, name)
    end
end

export Literal, Backref, OffsetBackref, DeflateSym, UncompressedBlock, CompressedBlock_LenlitCodelenEntry, CompressedBlock_DistCodelenEntry, CompressedBlock, UnderlyingBlock, DEFLATEBlock, DEFLATEStream
# mapentries: "CompressedBlock_DistCodelenEntry" => ("UInt32", "UInt32"), "CompressedBlock_LenlitCodelenEntry" => ("UInt32", "UInt32")
