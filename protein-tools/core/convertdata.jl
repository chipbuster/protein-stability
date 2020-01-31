## Routines to convert between different forms of data.
include(joinpath(@__DIR__, "simdata.jl"))

using .SimData;
using HDF5;
using LinearAlgebra;
using Discretizers;
using Test;

"""Generate a new HDF5 file with the same inputdata, but only using every Nth frame"""
function create_dataskip_file(input_file::String, take_every::Int, new_file = ""::String)
    infile = h5open(input_file, "r")
    if length(new_file) == 0
        new_basename = split(basename(input_file), '.')[1] * "_skip.hdf5"
        new_file  = joinpath(dirname(input_file), new_basename)
    end
    outfile = h5open(new_file, "w")
    for datapath in find_all_dataset(infile)
        outfile[datapath] = infile[datapath][:,:,1:take_every:end]
        attrs(outfile[datapath])["skip"] = take_every
    end
    close(outfile)
end

"""Convert an XYZ trace to dihedral angles."""
function xyz_to_dihedrals(input_data::InputData)::ParameterizedData
    # Set up the output data for to_dihedral to plug stuff into
    datapath = input_data.datapath * "/parameterized"

    (_, N, ts) = size(input_data.data)
    if datapath in SimData.find_all_dataset(input_data.h5File)
        paramdata = input_data.h5File[datapath]
        o_delete(paramdata)
    end

    paramdata = d_create(input_data.h5File, datapath, datatype(Float64), dataspace(N - 3, ts))

    attrs(paramdata)["maxval"] = convert(Float64, pi)  # This may need to be changed later
    attrs(paramdata)["type"] = "dihedral"

    to_dihedral(input_data.data, paramdata)
    flush(input_data.h5File)

    ParameterizedData(paramdata, input_data.filepath, input_data.datapath,
    input_data.h5File)
end

"""Bin a dihedral/phi-psi dataset into dihedral angles"""
function bin_dihedrals(input_data::ParameterizedData, nbins::Integer)::BinnedData
    datapath = input_data.datapath * "/binned"
    maxv = read(attrs(input_data.data)["maxval"])

    bounds = collect(range(-maxv, maxv, length = nbins + 1))
    @assert length(bounds) == nbins+1 "Incorrect bounds length from range/collect! Potential Julia bug?"
    (N, ts) = size(input_data.data)
    if datapath in SimData.find_all_dataset(input_data.h5File)
        binneddata = input_data.h5File[datapath]
        o_delete(binneddata)
    end

    binneddata = d_create(input_data.h5File, datapath, datatype(Int), dataspace(N, ts))

    attrs(binneddata)["nbins"] = nbins
    attrs(binneddata)["disctype"] = "linear"

    lindisc = LinearDiscretizer(bounds);
    binneddata[:,:] = encode(lindisc, input_data.data[:,:])
    flush(input_data.h5File)

    BinnedData(binneddata, input_data.filepath, input_data.datapath, input_data.h5File)
end


"""Take in a 3xNxtime set of coordinates and return a set of dihedral angles of
for N-3xtime

Output format follows that found in the Python code:

 [[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
 [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
 ] etc. etc.

Outputs provided by this function are in the range [-pi, pi].
"""
function to_dihedral(frames, outputs)
    (n, natom, timesteps) = size(frames)
    @assert n == 3 "Tried to take dihedrals on non-3D trace"
    for i = 1:timesteps
        outputs[:,i] = frame_dihedrals(frames[:,:,i])
        # println(size(frame_dihedrals(frames[:,:,i])), size(outputs[:,i]))
    end
end

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

"""Takes a single 3xN snapshot and returns an (N-3) long 1D vector containing the
sequential """
# Algorithm taken from https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates
function frame_dihedrals(frame)
    (_, N) = size(frame)
    dihedrals = Vector{Float64}()
    for i = 1:(N - 3)
        a1 = frame[:,i]
        a2 = frame[:,i + 1]
        a3 = frame[:,i + 2]
        a4 = frame[:,i + 3]
        b1 = a2 - a1
        b2 = a3 - a2
        b3 = a4 - a3
        n1 = cross(b1, b2)
        n2 = cross(b2, b3)
        n1 = n1 ./ norm(n1)
        n2 = n2 ./ norm(n2)
        m1 = cross(n1, b2 / norm(b2))
        x = dot(n1, n2)
        y = dot(m1, n2)
        d = atan(y, x)
        push!(dihedrals, d)
    end
    return dihedrals
end

using Rotations, StaticArrays
function test_dihedralization()
    num_theta = 25
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
        push!(dihedrals, dtheta)
        theta = wrap_range(theta - pi)  # "0 dihedral" is pointing in opposite dir
        theta -= dtheta                 # Apply opposite sign since we view from below
        rx = cos(theta)
        ry = sin(theta)
        (px, py, pz) = mat[:,i - 1]
        mat[:,i] = [px + rx, py + ry, pz + 10 * rand()]
    end

    r = rand(RotMatrix{3})
    mat = r * mat   # Apply random rotation

    frame_mat = Array{Float64,3}(undef, 3, num_theta * 2, 1)
    frame_mat[:,:,1] = mat
    recovered = Matrix{Float64}(undef, num_theta * 2 - 3, 1)
    to_dihedral(frame_mat, recovered)

    diffs = recovered[1:2:end] - dihedrals
    all(abs.(diffs) .< 1e-10)
end
