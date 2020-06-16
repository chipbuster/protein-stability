# Tests for the functionality in core.

using Test

###############################
## Geometry Extraction Tests ##
###############################

include("geom_funcs.jl")
using Rotations, StaticArrays

"""Normalize x to be in range by wrapping"""
function wrap_range(x, minr = -pi, maxr = pi)
    rangesize = maxr - minr
    while x > maxr
        x -= rangesize
    end
    while x < minr
        x += rangesize
    end
    x
end

function test_dihedral_calc()
    num_theta = 30
    mat = Array{Float64}(undef, 3, num_theta * 2)
    rand_init = randn(3)
    mat[:,1] = [0,0,0] + rand_init
    mat[:,2] = [1,0,0] + rand_init
    dihedrals = Vector{Float64}()

    #Generate a random helix where every other line moves in pure z-dir and the others
    # are a prescribed angle from [1,0,0]
    theta = 0.0
    for link_num in 2:num_theta
        i = 2 * link_num
        mat[:,i - 1] = mat[:,i - 2] + [0,0,1]   # Simple straight-up Z segment
        dtheta = 2 * pi * (rand() - 0.5)        # Range [-pi, pi]
        push!(dihedrals, -dtheta)
        theta = wrap_range(theta - pi)  # "0 dihedral" is pointing in opposite dir
        theta -= dtheta                 # Apply opposite sign since we view from below
        length_scale = 10 * rand(Float64)  # Make sure we're not length-dependent
        rx = cos(theta)
        ry = sin(theta)
        (px, py, pz) = mat[:,i - 1]
        mat[:,i] = [px + rx, py + ry, pz + 10 * rand()]
    end

    r = rand(RotMatrix{3})
    mat = r * mat   # Apply random rotation

    # Recovery phase
    frame_mat = Array{Float64,3}(undef, 3, num_theta * 2, 1)
    frame_mat[:,:,1] = mat
    recovered = bond_dihedral_frame(frame_mat)

    diffs = recovered[1:2:end] - dihedrals
    all(abs.(diffs) .< 1e-10)
end

function test_angle_calc()
    num_tests = 30
    mat = Matrix{Float64}(undef, 3, num_tests+2)
    mat[:,1] = randn(3)
    mat[:,2] = mat[:,1] + [1,0,0]

    # Generate angles in x-y plane with initial z-value from above
    angles = Vector{Float64}(undef, num_tests)
    theta = 0    # Convenience value: used to track current frame
    for i in 3:num_tests + 2
        magnitude = 10 * rand(Float64)
        newangle = 2 * pi * (rand(Float64) - 0.5)      # (-pi, pi)
        theta = wrap_range(theta + newangle)
        rx = magnitude * cos(theta)
        ry = magnitude * sin(theta)
        mat[:,i] = mat[:,i-1] + [rx, ry, 0]
        angles[i-2] = abs(newangle) # In 3D + higher, only care about magnitude
    end

    r = rand(RotMatrix{3})
    mat = r * mat   # Apply random rotation

    # Recovery phase
    recovered = bond_angle_frame(mat)
    diffs = angles - recovered
    all(abs.(diffs) .< 1e-10)
end

random_angle() = 2π * (rand() - 0.5)    # Random angle in [-π,π]
random_length() = rand() > 0.3 ? 10 * rand() : 0.01 * rand()     # Test "normal" and "very small" lengths
"""Generates a random three points and the returns the triplet and the angle they were generated with."""
function gen_random_angle_triplet()
    leg1_theta = random_angle()
    random_theta = random_angle()
    leg2_theta = leg1_theta + random_theta
    points = Matrix{Float64}(undef,2,3)
    # Negative because we need to think carefully about which angle we're measuring
    # in our code.
    # It's π - internal angle.
    p1 = -random_length() * [ cos(leg1_theta); sin(leg1_theta) ]
    p2 = [0;0]
    p3 = random_length() * [ cos(leg2_theta); sin(leg2_theta) ]
    # random_offset = [ 5000 * (rand() - 0.5); 5000 * (rand() - 0.5)]  # Offset in [-2500,2500]^2
    random_offset = 0
    points[:,1] = p1
    points[:,2] = p2
    points[:,3] = p3
    points .= points .+ random_offset
    (random_theta, points)
end

function test_random_angle_triplet()
    (angle, points) = gen_random_angle_triplet()
    recovered = bond_angle_frame(points)[1]
    abs(angle - recovered) < 1e-5
end


function test_angle_calc_2d_chain()
    num_tests = 30
    mat = Matrix{Float64}(undef, 2, num_tests+2)
    mat[:,1] = randn(2) * 10
    mat[:,2] = mat[:,1] + [1,0]

    # Generate angles in x-y plane with initial z-value from above
    angles = Vector{Float64}(undef, num_tests)
    theta = 0    # Convenience value: used to track current frame
    for i in 3:num_tests + 2
        magnitude = 10 * rand(Float64)
        newangle = 2 * pi * (rand(Float64) - 0.5)      # (-pi, pi)
        theta = wrap_range(theta + newangle)
        rx = magnitude * cos(theta)
        ry = magnitude * sin(theta)
        mat[:,i] = mat[:,i-1] + [rx, ry]
        angles[i-2] = newangle
    end

    r = rand(RotMatrix{2})
    t = randn(2) * 10
    mat = r * mat .+ t   # Apply random rotation + translation

    # Recovery phase
    recovered = bond_angle_frame(mat)
    diffs = angles - recovered
    all(abs.(diffs) .< 1e-10)
end

function test_angle_calc_2d_triplet()
    num_tests = 100_000
    for i = 1:num_tests
        if !test_random_angle_triplet()
            return false
        end
    end
    return true
end

function test_angle_calc_2d()
    test_angle_calc_2d_triplet() && test_angle_calc_2d_chain()
end

function test_length_calc()
    num_tests = 30
    mat = Matrix{Float64}(undef, 3, num_tests+1)
    mat[:,1] = randn(3)
    
    lengths = Vector{Float64}(undef, num_tests)

    for i in 2:num_tests+1
        magnitude = 10 * rand(Float64)
        direction = normalize!(randn(3))
        mat[:,i] = mat[:,i-1] + magnitude * direction
        lengths[i-1] = magnitude
    end

    # Recovery phase
    recovered = bond_length_frame(mat)
    diffs = lengths - recovered
    all(abs.(diffs) .< 1e-10)
end

function test_zmatrix_calc()
    num_atoms = 50  # Can't go too large, or risk cumulative recovery error
    frame = randn(Float64, (3, num_atoms))

    # Need to align initial points carefully--rest can be random
    frame[:,1] .= [0,0,0]
    frame[:,2] .= [0,0,1]
    frame[:,3] = [1,0,1]

    zmat = frame_cartesian_to_zmatrix(frame)
    recovered = frame_zmatrix_to_cartesian(zmat)

    diffs = frame - recovered

    all(abs.(diffs) .< 1e-8)
end


@test test_zmatrix_calc()
@test test_dihedral_calc()
@test test_length_calc()
@test test_angle_calc()
@test test_angle_calc_2d()
