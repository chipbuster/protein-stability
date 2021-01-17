include("langevin_sim.jl")

using Test;

"""Simulates a network where all points are connected to all other points by
bonds of length 1."""
function sim_dense_network(natoms::Integer, nsteps::Integer, nskip=10)
    @assert(natoms <= 20, "Too many atoms for a dense network!")
    positions = randn((3, natoms));
    restlengths = UpperTriangular(ones(natoms, natoms))
    coeffs = restlengths
    state = SimState(positions, restlengths, coeffs)

    trace = Array{Float64, 3}(undef, 3, natoms, nsteps)  # 10s of MB
    params = SimParameters(0.01, 0.01,  0.95, 10.0)  # Small stochastic force (T=0)
    state2 = deepcopy(state)
    run_sim(state, trace, params, nsteps, nskip)
    (trace, state)
end

"""Find the indices of the closest 3 columns to `pt` in the `pts` matrix"""
function find_closest_3(pt::AbstractVector{Float64}, pts::AbstractMatrix{Float64})
    pt3 = pts[:, 1:3]
    dists = [ norm(pt3[:,i] - pt) for i in 1:3 ]
    ind = [ 1 2 3 ]
    for i in 4:size(pts,2)
        dist = norm(pt - pts[:,i])
        if dist < maximum(dists)
            _,j = findmax(dists)
            pt3[:,j] = pts[:,i]
            dists[j] = dist
            ind[j] = i
        end
    end
    @assert(length(Set(ind)) == length(ind), "Closest are not unique!")
    return ind
end

"""Simulates a network where all points are connected to exactly three other
points by springs of length 1."""
function sim_tet_network(natoms::Integer, nsteps, nskip=1)
    @assert(natoms >= 4, "Cannot simulate a tetnet with <4 atoms.")
    positions = zeros((3, natoms))
    for i = 1:3
        positions[:,i] = rand(3)
    end
    restlengths = UpperTriangular(ones(natoms, natoms))
    connectivity = UpperTriangular(zeros(natoms, natoms))
    # Connect the initial triangles
    connectivity[1,2] = 1.0
    connectivity[1,3] = 1.0
    connectivity[2,3] = 1.0

    for i = 4:natoms
        newpt = 1000 * randn(3)
        closest_indices = find_closest_3(newpt, positions[:,1:i-1])
        # Ideally, we'd find the new point that forms a perfect equilateral tet
        # Clumsy hack: pick a point and draw a line, then find the length-1 edge.
        # Hopefully this won't be too far off.
        basept = positions[:, closest_indices[1]]
        testvec = normalize(newpt - basept)
        newpt = basept + testvec
        positions[:, i] = newpt
        for j in closest_indices
            connectivity[j,i] = 1.0
        end
    end

    state = SimState(positions, restlengths, connectivity)
    trace = Array{Float64, 3}(undef, 3, natoms, nsteps)  # 10s of MB
    params = SimParameters(0.01, 0.01,  0.95, 20.0)  # Small stochastic force (T=0)
    state2 = deepcopy(state)
    run_sim(state, trace, params, nsteps, nskip)
    (trace, state2)
end

function pairwise_dists(frame)
    natoms = size(frame,2)
    dists = Matrix{Float64}(undef, natoms, natoms)
    for j = 1:natoms
        for i = 1:j
            dists[i,j] = norm(frame[:,j] - frame[:,i])
        end
    end
    UpperTriangular(dists)
end

max_pairwise_dist(frame) = maximum(pairwise_dists(frame))

"""Checks all-to-all and confirms that they never get too crazy."""
function dense_no_explosion(trace, starting_positions)
    # Note: function is not efficient.
    max_d = max_pairwise_dist(starting_positions)
    for (i, frame) in enumerate(eachslice(trace; dims=3))
        maxdist = max_pairwise_dist(frame)
        if maxdist > 3*max_d || any(isnan.(frame))
            println("Found maximum pairwise distance of $(maxdist)")
            return i
        end
    end
    return 0
end

function no_pairwise_explosions(trace, state)
    n = natoms(state)
    for (frameno, frame) in enumerate(eachslice(trace; dims=3))
        for j = 1:n
            for i = 1:j
                dij = norm(frame[:,j] - frame[:,i])
                if state.coeffs[i,j] > 0 && dij > 10.0
                    return (frameno, i, j, dij)
                end
            end
        end
    end
    return (0,0,0,0)
end

function test_dense_stable()
    for n = [10, 15, 20]
        (trace, init_state) = sim_dense_network(n, 10_000)
        badframe = dense_no_explosion(trace, init_state.positions)
        if badframe != 0
            println("Explosion on dense trace n=$(n) at frame $(badframe)")
            println("Initial state was:")
            display(init_state.positions)
            println("Pairwise distances were:")
            display(pairwise_dists(init_state.positions))
            println("\n")
            for (i,slice) in enumerate(eachslice(trace; dims=3))
                print(" ===== Frame $(i) ===== ")
                display(slice)
                display(pairwise_dists(slice))
                if i == lastframe
                    return
                end
            end
            display_failure(trace, init_state, badframe)
            return false
        end
    end
    return true
end

function test_tets_stable()
    for n = [10,20,30]
        (trace, init_state) = sim_tet_network(n, 10_000, 10)
        (badframe, badi, badj, dij) = no_pairwise_explosions(trace, init_state)
        if badframe != 0
            println("Explosion on tet trace n=$(n) at frame $(badframe)")
            println("Distance between atoms $(badi) and $(badj) was $(dij)")

            println("Connectivity is:")
            display(init_state.coeffs)
            println("\n Positions:")
            display(init_state.positions)
            println("\nDistances:")
            display(pairwise_dists(init_state.positions) .* init_state.coeffs)

            for (i, frame) in enumerate(eachslice(trace; dims=3))
                println("\n ==== Frame $(i) ==== ")
                display(frame)
                println("\nDistances")
                display(pairwise_dists(frame)  .* init_state.coeffs)

                if i == badframe
                    return false;
                end
            end
        end
    end
    return true
end

@test test_tets_stable()
@test test_dense_stable()
