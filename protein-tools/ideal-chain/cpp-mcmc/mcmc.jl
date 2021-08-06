using StaticArrays
using LinearAlgebra
using Logging
using HDF5

function gen_points(ϕs)
    pts = Matrix{Float64}(undef, 2, length(ϕs)+2)
    pts[:,1] = [0.0,0.0]
    pts[:,2] = [1.0,0.0]
    θs = ϕs .+ π |> cumsum .|> mod2pi
    for (i,θ) in enumerate(θs)
        pts[:,i+2] = pts[:,i+1] + [ cos(θ), sin(θ) ]
    end
    pts
end

gen_end_point(ϕs) = gen_points(ϕs)[:,end]

e2e_dist(ϕs) = gen_end_point(ϕs) |> norm

function read_sim_file(filename)
    h5open(filename) do f
        read(f,"angles")
    end
end
