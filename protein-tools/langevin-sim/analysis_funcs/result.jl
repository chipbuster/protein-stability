struct AnalysisResult
    origin::Tuple{String,String}
    lengths::Matrix{Float64}
    angles::Matrix{Float64}
    autocor_xs::Vector{Float64}
    autocor_ys::Vector{Float64}
    endpoints::Matrix{Float64}
end
