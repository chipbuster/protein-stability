using LinearAlgebra
using Printf
using Serialization
using Statistics
using StatsBase
using Base.Threads
using HDF5

#############

const k_B = 1.0

#= 
This simulation uses the unitless parameterization descrbied in the experiment
notes. In short, we define the following two unitless constants:

        c1 = k dt / γ
        c2 = ξ dt / γ L

where k is the spring stiffness, ξ is the strength of the stochastic force,
γ is the damping coefficient, and L is some characteristic length (for ideal
chains, the rest bondlength, for proteins, usually implicitly chosen to be 
1 Angstrom). We then obtain the following unitless equations of motion:

y_{n+1} = y_n - c_1 [ \sum_{bonds} \| y^{(a)} - y^{(b)} \| - 1 ] u_{bond} + c_2 σ_n

where a,b are the endpoints of each bond, u_bond is the normalized directional
vector of the bond, σ_n is a Gaussian R.V, and y_n are the coordinates rescaled
to the lengthscale L. =#

"""Encapsulates parameters of the simulation"""
struct SimParameters
    T::Float64      # The temperature of the system
    δt::Float64     # The timestep of the system
    γ::Float64      # The level of damping in the system
    k::Float64      # Spring strength
end

default_sim_parameters() = SimParameters(0.5, 0.01, 0.95, 38.0)

calc_c1(s::SimParameters) = s.k * s.δt / s.γ
calc_c2(s::SimParameters) = begin
    D = k_B * s.T / s.γ
    c2 = sqrt(2 * D * s.δt)
end
calc_c1c2(s::SimParameters) = (calc_c1(s), calc_c2(s))

"""Encapsulates state of the simulation"""
struct SimState
    positions::Matrix{Float64}   # The positions of the atoms
    restlens::UpperTriangular{Float64,Matrix{Float64}}  # Restlength of bonds
    coeffs::UpperTriangular{Float64,Matrix{Float64}}    # Spring coefficients
end

natoms(s::SimState) = size(s.positions, 2)

"""Convenience struct to avoid allocating scratch space"""
struct ScratchBufs
    forces::Matrix{Float64}
    pairforce::Vector{Float64}
    coeffs::Vector{Float64}
end

ScratchBufs(N) = ScratchBufs(
                        Matrix{Float64}(undef, 3, N), 
                        Vector{Float64}(undef, 3),
                        Vector{Float64}(undef, 2)
                 )

ScratchBufs(N, params) = ScratchBufs(
    Matrix{Float64}(undef, 3, N),
    Vector{Float64}(undef, 3),
    [calc_c1(params), calc_c2(params)]
)

"""Computes the pairwise force on atoms i, j given the current simulation state.

Places the force on atom `i` into the `scratch` parameter, with the implicit
assumption that the force on atom `j` is just the negative of this.
"""
function compute_force_pair!(scratch::AbstractVector{Float64},
                            state::SimState, i::Int, j::Int)
    @views @. scratch = state.positions[:,j] - state.positions[:,i]
    dist_ij = norm(scratch)
    display(dist_ij)

    # If this vector is zero, we can't normalize it. Fortunately, we can
    # just say the force is zero, since stochastic updates will push it
    # out of the zero condition on the next timestep
    if abs(dist_ij) < 1e-7
        scratch .= 0.0
    else
        # The i->j vector is already in here from earlier. Normalize it first
        # and then multiply by the correct values
        kcoef = 1.0 - (state.restlens[i,j] / dist_ij)
        scratch .*= state.coeffs[i,j] * kcoef
    end
    nothing
end

"""Compute the forces given a current configuration."""
function compute_forces!(scratch::ScratchBufs, state::SimState)::Nothing
    numAtoms = natoms(state)
    forces = scratch.forces
    pairforce = scratch.pairforce
    @inbounds for i in 1:numAtoms
        @inbounds for j in i + 1:numAtoms
            # Strictly speaking not necessary (because zero coef is handled)
            # correctly by compute_force_pair! but it's a sparse optimization
            if state.coeffs[i,j] == 0
                continue
            end
            compute_force_pair!(pairforce, state, i,j)
            @views forces[:,i] .+= pairforce
            @views forces[:,j] .-= pairforce
        end
    end
end

"""Take a timestep with overdamped explicit Euler."""
function take_timestep!(state::SimState, scratch::ScratchBufs)
    fill!(scratch.forces, zero(Float64))
    fill!(scratch.pairforce, zero(Float64))
    c1 = scratch.coeffs[1]
    c2 = scratch.coeffs[2]

    # Get deterministic update
    compute_forces!(scratch, state)
    scratch.forces .*= c1

    display(scratch.forces)
    
    # Add stochastic update into force. To avoid allocating, do this in a loop
    for i in eachindex(scratch.forces)
        scratch.forces[i] += c2 * randn()
    end

    state.positions .+= scratch.forces
end

"""Run a simulation on the specified initial simulation state.

    Stores output to an HDF dataset provided by `hdf_fname` and `datapath`
    Only stores every `skipn` data steps.
"""
function run_sim(simstate::SimState, outdata::HDF5Dataset, params::SimParameters, nframes, nskip=1)
    @assert(nskip >= 1, "Trying to skip non-positive number of frames")

    scratch = ScratchBufs(natoms(simstate), params)
    @assert(size(scratch.forces) == size(simstate.positions), "Force-Position size mismatch")

    (c1,c2) = calc_c1c2(params)
    @info @sprintf("Run sim c1=%.3f, c2=%.3f, with %d atoms", c1,c2,natoms(simstate))
    @info @sprintf("Recording %d frames total at every %d frame", nframes, nskip)

    for t in 1:nframes
        # Only record every skipn counts
        for _ in 1:nskip
            take_timestep!(simstate, scratch)
        end
        outdata[:,:,t] = simstate.positions
    end
end

############################
# END SIMULATION FUNCTIONS #
# BEGIN UTILITIY FUNCTIONS #
############################

""" Creates a simple test which can be checked by hand for correctness.

Test: two points on the z-axis, initially at z = +1 and z=-1. They are perturbed
to +/-1.5 and then allowed to evolve under the system.

With basic overdamped Langevin (no random force), we expect to see them drift
towards each other slowly, until the force becomes almost zero and they stop
moving."""
function simple_test()
    # Points across the Z axis
    positions = [0 0; 0 0; 1.5 -1.5]
    restlens = UpperTriangular([0.0 1.0; 0.0 0.0])
    coeffs = restlens
    natoms = 2

    state = SimState(positions, restlens, coeffs)

    params = default_sim_parameters()

    nsteps = 10

    solution = zeros(3, 2, nsteps)
    scratch = ScratchBufs(2)
    scratch.coeffs[1] = 0.4
    scratch.coeffs[2] = 0.0   # Artificially disable stochastic force
    for t in 1:nsteps
        solution[:,:,t] = state.positions
        take_timestep!(state, scratch)
    end
    return (solution, state)
end
