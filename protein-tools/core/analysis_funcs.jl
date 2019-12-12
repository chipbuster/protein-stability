include(joinpath(@__DIR__, "simdata.jl"))

using .SimData;

function autocorrelation(dataset::T) where T <: AbstractSimData
    datagroup = parent(dataset.datapath)
end