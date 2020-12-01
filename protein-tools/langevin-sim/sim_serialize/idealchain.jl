using LinearAlgebra
using NLsolve
using LsqFit
using Distances
using Serialization

# This file uses the scale-free forms of motion derived on 2019-03-09. For now,
# we assume the rest bondlength is 1 unit long.

const L = 1.0

"""Encapsulates parameters of the simulation"""
struct SimParameters
    L::Float64
    c1::Float64
    c2::Float64
    eelength::Float64
    skip::Int
    num_ts::Int     # The total number of timesteps to take
end

"""Encapsulates a simulation state"""
struct IdealChain2D
    natoms::Int
    eelength::Float64   # Distance between endpoints, in multiples of L
    config::Matrix{Float64}
end

function gen_ideal_chain(natoms::Int, length::Float64)::IdealChain2D
    if length > natoms-1
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
        config[:,i] = config[:,i-1] + modifier
    end
    println("Actual initial length is $(norm(config[:,end] - config[:,1])) on a target eedist of $(length)")
    return IdealChain2D(natoms, length, config)
end

"""Compute the forces on a single pair of atoms"""
function compute_force_pair!(forces, ij_buf, config, i, j, restlen)
    nrow, ncol = size(config)
    @views @. ij_buf = config[:,j] - config[:,i]
    dist_ij = norm(ij_buf)
    # If this vector is zero, we can't normalize it. Fortunately, we can
    # just say the force is zero, since stochastic updates will push it
    # out of the zero condition on the next timestep
    if dist_ij < 1e-5
        fill!(ij_buf, 0.0)
    else
        ij_buf ./= dist_ij
        @assert(abs(norm(ij_buf) - 1.0) < 1e-4, "Normalized vector is not unit!")
    end

    # ij_buf is now normalized
    # C_1 effect is added in the main update, not during the subcalculations
    
    @views @. forces[:,i] += (dist_ij - restlen) * ij_buf
    @views @. forces[:,j] -= (dist_ij - restlen) * ij_buf
    nothing   # Suppress returning of one of these vectors--causes allocation
end

"""Compute the forces on a given protein configuration, given the current
configuration and the spring constants
"""
function compute_forces(state::IdealChain2D, forces::Matrix{Float64})
    numAtoms = state.natoms
    config = state.config
    ij_buf = Vector{Float64}(undef, size(config)[1])

    @inbounds for i in 1:numAtoms-1
        j=i+1
        compute_force_pair!(forces, ij_buf, config,i,j, 1.0)
    end
    compute_force_pair!(forces, ij_buf, config, 1, numAtoms, state.eelength)
    return forces
end

"""Take a timestep"""
function take_timestep(simstate, params, forcebuf::Matrix{Float64})
    ts = params.num_ts
    c1 = params.c1
    c2 = params.c2

    # The explicit euler update for forces in the overdamped regime
    n = simstate.natoms

    update = c1 * compute_forces(simstate, forcebuf)

    # The stochastic update
    for i in CartesianIndices(update)
        update[i] += c2 * randn()
    end
    simstate.config .+= update
end

"""Run a sim and dump to serial file.

Since these runs are experimental and all fit in memory, we can skip HDF5."""
function run_sim(filename,natom,params, iscale)
    simstate = gen_ideal_chain(natom, params.eelength)
    simstate.config .*= iscale
    ds = Array{Float64,3}(undef, 2, natom, params.num_ts)
    forcebuf = Matrix{Float64}(undef, 2, natom)
    
    for step in 1:params.num_ts
        for skip in 1:params.skip
            fill!(forcebuf, 0)
            take_timestep(simstate, params, forcebuf)
        end
#        lengths = [ norm(simstate.config[:,i+1] - simstate.config[:,i]) for i in 1:natom-1 ]
#        println("maxlen: $(maximum(lengths)), minlen: $(minimum(lengths))")
#        println("eelen: $(norm(simstate.config[:,end] - simstate.config[:,1]))")

        ds[:,:,step] = simstate.config
    end
    serialize(filename, ds)
end
