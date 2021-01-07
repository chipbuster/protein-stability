include("lztypes.jl")
using Gadfly;

function plot_literal_statistics(block::Block)
    literals = filter(x -> isa(x, Literal), block.data)
    values = [ x.value for x in literals ]
    plot(x=values, Geom.histogram)
end

function plot_backref_size_statistics(block::Block)
    backrefs = filter(x -> isa(x, Backreference), block.data)
    sizes = [ b.len for b in backrefs ]
    plot(x=sizes, Geom.histogram)
end