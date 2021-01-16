using LinearAlgebra
using Printf
using Serialization
using Statistics
using StatsBase
using Base.Threads
using HDF5

#############

#= 
This simulation uses the unitless parameterization descrbied in the experiment
notes. In short, we describe distances in units of bond restlength L, and define
two unitless constants:

        c1 = k dt / γ
        c2 = ξ dt / γ L

where k is the spring stiffness, ξ is the strength of the stochastic force,
and γ is the damping coefficient. We then obtain the following unitless 
equations of motion:

y_{n+1} = y_n - c_1 [ \sum_{bonds} \| y^{(a)} - y^{(b)} \| - 1 ] u_{bond} + c_2 σ_n

where a,b are the endpoints of each bond, u_bond is the normalized directional
vector of the bond, and σ_n is a Gaussian R.V. =#

"""Encapsulates parameters of the simulation"""
struct SimParameters
    c1::Float64     # The temperature of the system
    c2::Float64     # The level of damping in the system
    num_ts::Int     # The total number of timesteps to take
end

"""Encapsulates state of the simulation"""
struct SimState
    positions::Matrix{Float64}   # The positions of the atoms
    restlens::UpperTriangular{Float64,Matrix{Float64}}  # Restlength of bonds
    coeffs::UpperTriangular{Float64,Matrix{Float64}}    # Spring coefficients
end

"""Convenience struct to avoid allocating scratch space"""
struct ScratchBufs
    forces:::Matrix{Float64}
    pairforce::Vector{Float64}
end

natoms(s::SimState) = size(s, 2)

"""Computes the pairwise force on atoms i, j given the current simulation state.

Places the force on atom `i` into the `scratch` parameter, with the implicit
assumption that the force on atom `j` is just the negative of this.
"""
function compute_force_pair!(scratch::AbstractVector{Float64}
                            state::SimState, ids::Tuple{Integer,Integer})::Nothing
    (i, j) = ids
    scratch .= @views state.positions[:,j] - state.positions[:,i]
    dist_ij = norm(scratch)

    # If this vector is zero, we can't normalize it. Fortunately, we can
    # just say the force is zero, since stochastic updates will push it
    # out of the zero condition on the next timestep
    if abs(dist_ij) < 1e-8
        scratch .= 0.0
    else
        # The unit i->j vector is already in here from earlier, just multiply it
        # by the correct scalar coefficients
        scratch .*= @views coeffs[i,j] * (dist_ij - restdist[i,j])
    end
    nothing
end

"""Compute the forces given a current configuration."""
function compute_forces!(scratch::ScratchBufs, state::SimState)::Nothing
    numAtoms = natoms(state)
    forces = scratch.forces
    pairforce = scrach.pairforce
    @inbounds for i in 1:numAtoms
        @inbounds for j in i + 1:numAtoms
            if coeffs[i,j] == 0
                continue
            end
            compute_force_pair!(pairforce, state, (i,j))
            forces[:,i] += pairforce
            forces[:,j] -= pairforce
        end
    end
    return forces
end

"""Take a timestep with overdamped explicit Euler."""
function take_timestep(state::SimState, params::SimParameters,
                       forcesbuf::Matrix{Float64})
    # Get deterministic update
    compute_forces!(forcesbuf, state)
    forcesbuf .*= params.c1
    
    # Add stochastic update into force. To avoid allocating, do this in a loop
    for i in eachindex(forcesbuf)
        forcesbuf[i] += params.c2 * randn()
    end

    state.positions += update
end

"""Run a simulation on the specified initial simulation state.

    Stores output to an HDF dataset provided by `hdf_fname` and `datapath`
    Only stores every `skipn` data steps.
"""
function run_sim(simstate::SimState, hdf_fname::String, datapath::String; params::SimParameters, skipn::Int=1)
    @assert(skipn >= 1,  "Trying to skip non-positive number of frames")
    simfile = SimData.InputData(hdf_fname, datapath)
    data = simfile.data

    @printf("Running with %d atoms, T = %f, %d timesteps of size %f \n",
            simstate.N, params.temp, params.num_ts, params.ts)

    attrs(data)["source"] = "langevin"
    attrs(data)["timestep"] = params.ts
    attrs(data)["temp"] = params.temp
    attrs(data)["damp"] = params.damp

    if skipn > 1
       attrs(data)["skip"] = skipn
    end

    for t in 1:params.num_ts
        # Only record every skipn counts
        if t % skipn == 0
            data[:,:,t] = simstate.positions
        end
        take_timestep(simstate, params)
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
    positions = [0 0; 0 0; 0 0]
    state = generate_simstate(positions)
    natoms = state.N

    # Initial perturbation
    state.positions += [0 0; 0 0; 0.5 -0.5]

    params = SimParameters(100.0, 0.001, 0.8, 10_000_000)

    solution = zeros(3, state.N, params.num_ts)
    for t in 1:params.num_ts
        solution[:,:,t] = state.positions
        take_timestep(state, params)
    end
    return (solution, state)
end

function compute_histogram(simoutput; name=nothing)
    binct = 1000 # Doesn't matter what it is, as long as we have the same # bins for both
    (sim, state) = simoutput
    energy = compute_energy(sim, state)
    hist = fit(Histogram, energy; nbins=binct)

    # Normalize Histogram
    edges = collect(hist.edges[1])[1:end - 1]
    weights = hist.weights / sum(hist.weights)

    plot(edges, weights)
    # histogram!(energy; normalize = :probability, bins = binct)   # As a sanity check

    @. model(x, p) = p[1] * exp(-x * p[2])
    f = curve_fit(model, edges, weights, [0.01,0.01])
    plot!(edges, model(edges, f.param))
    if name !== nothing
        savefig(name)
    else
    gui()
    end
end

"""Get a list of pdbnames from a file"""
function get_pdbnames(filename)
    names = Vector{String}()
    open(filename) do file
        for name in eachline(file)
            push!(names, name)
        end
    end
    return names
end

"""Parse the input name, possibly splitting it for subselection"""
function parse_pdb_subrange(name)
    # Base PDBs have no characters
    if length(name) == 4
        return (name, nothing)
    end
    
    # We have a subset range specified in the name. Split on the allowed chars
    toks = split(name, ['-','_'])
    if length(toks) != 3
        println("PDB name is not 4 chars, but has wrong form for subrange!")
        exit(1)
    end

    (name, start, fini) = toks
    s = parse(Int, start)
    e = parse(Int, fini)
    (name, (s, e))
end

# Currently used only for plots
# Has not been updated for c1/c2 parameterization
"""Compute the energy of a state """
function compute_energy(state::SimState)
    natoms = size(state.positions, 2)
    config = state.positions
    restdist = state.restlens
    coeffs = state.coeffs

    energy = 0.0

    @inbounds for i in 1:numAtoms
        @inbounds for j in i + 1:numAtoms
            vec_ij  = config[:,j] - config[:,i]
            dist = norm(vec_ij)
            coeff = coeffs[i,j]
            restlen = restdist[i,j]
            energy += 0.5 * coeff * (dist - restlen)^2
        end
    end
    return energy
end

"""Compute the energy of a positional trace. Useful for computing the energies 
of a simulation result. positions is a 3xNxtimesteps 
"""
function compute_energy(positions, state::SimState)
    energy = Vector{Float64}()
    # It's cheaper to change the state when computing energy than cloning a new
    # state...but let's make sure to restore the original state
    for x in eachslice(positions; dims=3)
        state.positions = x
        push!(energy, compute_energy(state))
    end
    # Restore the orginal position in case the caller cares about it
    state.positions = positions[:,:,1]
    return energy
end