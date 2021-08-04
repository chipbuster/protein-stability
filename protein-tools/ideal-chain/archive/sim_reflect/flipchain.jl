using LinearAlgebra
using StaticArrays
using Serialization
using Statistics
using StatsBase
using Base.Threads

const rot90 = SMatrix{2,2}(0, 1, -1, 0)
const L = 1  # Bond length

"""Encapsulates parameters of the simulation"""
struct SimParameters
    L::Float64
    eelength::Float64
    numatoms::Int
    num_ts::Int     # The total number of timesteps to take
    record_every::Int  # Number of frames to record
    pinned::Bool    # Apply translational/rotational noise?
end

"""Encapsulates a simulation state"""
struct IdealChain2D
    natoms::Int
    eelength::Float64   # Distance between endpoints, in multiples of L
    config::Matrix{Float64}
end

function gen_ideal_chain(natoms::Int, length::Float64)::IdealChain2D
    if length > natoms - 1
        println("Error: cannot make length $(length) chain with $(natoms) atoms")
        exit(-1)
    end
    # Set up initial zigzag configuration with the correct shape
    config = Matrix{Float64}(undef, 2, natoms)
    x_offset = length / (natoms - 1)
    y_offset = sqrt(1 - x_offset^2)
    config[:,1] = [0,0]
    for i in 2:natoms
        modifier = if i % 2 == 0
            [x_offset,y_offset]
        else
            [x_offset,-y_offset]
        end
        config[:,i] = config[:,i - 1] + modifier
    end
    println("Actual initial length is $(norm(config[:,end] - config[:,1])) on a target eedist of $(length)")
    return IdealChain2D(natoms, length, config)
end

"""Reflect the point about the line created by (e1, e2), modifying the point in-place"""
function reflect_point_across_line!(point, (e1, e2))
    @assert(length(e1) == length(e2) == length(point) == 2)
    n = rot90 * normalize(e2 - e1)
    ref = SMatrix{2,2}(I) - 2 * n * n'

    # If the e1-->e2 vector passes through the origin, we can apply the naive
    # HH reflector, but we have no guarantee that this is the case. To ensure
    # that the reflection is still correct, shift the entire system so that e1
    # is at the origin, apply the reflection, then shift back

    point .-= e1
    point .= ref * point
    point .+= e1
    nothing
end

"""Return the reflection of a point across the line created by (e1, e2)"""
function reflect_point_across_line(point, (e1, e2))
    tmp = copy(point)
    reflect_point_across_line!(tmp, (e1, e2))
    tmp
end

"""Modify the chain in-place by reflecting it across the line formed by config[i1] and config[i2]"""
function apply_reflect_to_chain!(config, (i1, i2))
    reflection_line = (config[:,i1], config[:,i2])
    for i in i1:i2
        @views reflect_point_across_line!(config[:,i], reflection_line)
    end
    nothing
end

function apply_timestep!(chain)
    numatoms = size(chain)[2]
    i1 = 0
    i2 = 0
    while i2 - i1 < 3
        i1 = rand(1:numatoms)
        i2 = rand(1:numatoms)
        (i1, i2) = minmax(i1, i2)
    end
    apply_reflect_to_chain!(chain, (i1, i2))
end

function run_sim!(chain, params)
    (_, natoms) = size(chain)
    T = params.num_ts
    E = params.record_every
    pinned = params.pinned

    snapshots = Array{Float64, 3}(undef, 2, natoms, T)
    framenum = 1

    rot = Matrix{Float64}(undef, 2, 2)

    for t in 1:T
        for _ in 1:E
            apply_timestep!(chain)

            # Apply rotational + translational perturbation
            if !pinned
                θ = 0.05 * randn()
                t = 0.01 * randn(2)
                rot[1,1] = cos(θ)
                rot[2,1] = sin(θ)
                rot[1,2] = -sin(θ)
                rot[2,2] = cos(θ)
                @views chain .= rot * chain
                @views chain .+= t
            end
        end
        snapshots[:,:,framenum] = chain
        framenum += 1
    end

    snapshots
end

function paramscan(atom_counts, ext_fracs, outdir, pinned::Bool)
    p = [ (n, frac) for n in atom_counts, frac in ext_fracs ]

    @threads for (n, frac) in p
        params = SimParameters(L, frac*L*(n-1),n, 1_000_000, 150, pinned)
        chain2d = gen_ideal_chain(n, params.eelength)
        trace = run_sim!(chain2d.config, params)
        serialize(joinpath(outdir, "output_$(n)_$(frac).serial"), trace)
    end
end

paramscan([30,20,10], 0.1:0.1:0.9, ARGS[1], parse(Bool, ARGS[2]))
