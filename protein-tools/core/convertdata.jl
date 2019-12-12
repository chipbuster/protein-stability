## Routines to convert between different forms of data.
include(joinpath(@__DIR__, "simdata.jl"))

using .SimData;
using HDF5;

"""Find the name of all datasets below a given path in an HDF5 file"""
function find_all_dataset(file::HDF5File, start="/"::String)::Vector{String}
    datasets = Vector{String}()
    for x in file[start]
        if typeof(x) == HDF5Group
            append!(datasets, find_all_dataset(file,name(x)))
        elseif typeof(x) == HDF5Dataset
            push!(datasets,name(x))
        else
            continue   # Not of interest to us
        end
    end
    return datasets
end

"""Generate a new HDF5 file with the same inputdata, but only using every Nth frame"""
function create_dataskip_file(input_file::String, take_every::Int, new_file=""::String)
    infile = h5open(input_file,"r")
    if length(new_file) == 0
        new_basename = split(basename(input_file),'.')[1] * "_skip.hdf5"
        new_file  = joinpath(dirname(input_file), new_basename)
    end
    outfile = h5open(new_file,"w")
    for datapath in find_all_dataset(infile)
        outfile[datapath] = infile[datapath][:,:,1:take_every:end]
    end
    close(outfile)
end

"""Convert an XYZ trace to dihedral angles."""
function xyz_to_dihedrals(dataset::InputData)
    
end

"""Bin a dihedral/phi-psi dataset into dihedral angles"""
function bin_dihedrals(dataset::ParameterizedData)

end


"""Take in a 3xNxtime set of coordinates and return a set of dihedral angles of
for 2Nxtime

Output format follows that found in the Python code:

 [[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
 [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
 ] etc. etc.

"""
function to_dihedral(frames)
    (_, natom, timesteps) = size(frames)
    dihedrals = zeros(natom - 3, timesteps)
    for i = 1:timesteps
        dihedrals[:,i] = frame_dihedrals(frames[:,:,i])
    end
    return dihedrals
end

"""Normalize x to be in range by wrapping"""
function normalize_range(x, minr = -pi, maxr = pi)
    rangesize = maxr - minr
    while x > maxr
        x -= rangesize
    end
    while x < minr
        x += rangesize
    end
    x
    end

"""Takes a single 3xN snapshot and returns a 2*N 1D vector containing the
interleaved phi-psi coordinates"""
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
    # The final result is not quite what we'd like: it uses the wrong convention
    # and is shifted. Remap it back into the correct interval to make it work.
    return normalize_range.(dihedrals)
end