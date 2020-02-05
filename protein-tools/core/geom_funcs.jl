using LinearAlgebra

# Note: arguments are intentionally untyped here to allow for efficient JIT
# compilation on multiple data types. Most functions assume "matrix-like" input,
# i.e. the input type supports indexing and slicing like a matrix.
# The specific assumptions here are as follows:
#
# For single-instance functions, the inputs are assumed to be single vectors
# in 3D.
#
# For functions operating on frames, the inputs are assumed to be in a
# matrix-like with 2 dimensions, of size (3,N) where N is the number of atoms.
#
# For time-series functions, the input is assumed to be in amatrix-like of 
# (3,N,T), where N is the number of atoms and T is the number of frames.
#
# In addition, the functions operating on traces can optionally accept a
# matrix-like to output the data into. This allows the user to avoid having to
# store all the data in RAM. If the output argument is used, it must be non-null
# and already have the correct size.

"""Calculate the distance between given atom coordinates."""
@inline function bond_length(a1, a2)
    return norm(a2 - a1)
end

"""Calculate the 3-atom angle given atom coordinates."""
@inline function bond_angle(a1,a2,a3)
    vec1 = a2 - a1
    vec2 = a3 - a2
    num  = dot(vec1, vec2)
    den  = norm(vec1) * norm(vec2)
    return acos(num / den)
end

"""Calculate the 4-atom angles given atom coordinates"""
# Algorithm taken from https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates
@inline function bond_dihedral(a1,a2,a3,a4)
    b1 = a2 - a1
    b2 = a3 - a2
    b3 = a4 - a3
    n1 = cross(b1, b2)
    n2 = cross(b2, b3)
    normalize!(n1)
    normalize!(n2)
    m1 = cross(n1, b2 / norm(b2))
    x = dot(n1, n2)
    y = dot(m1, n2)
    return atan(y, x)
end

"""Calculate bond lengths in a frame."""
@inline function bond_length_frame(frame)
    (_, natom) = size(frame)
    bl = Vector{Float64}(undef,natom-1)
    @inbounds for i in 1:natom-1
        bl[i] = bond_length(frame[:,i], frame[:,i+1])
    end
    return bl
end

"""Calculate bond angles in a frame."""
@inline function bond_angle_frame(frame)
    (_, natom) = size(frame)
    ba = Vector{Float64}(undef,natom-2)
    @inbounds for i in 1:natom-2
        ba[i] = bond_angle(frame[:,i], frame[:,i+1], frame[:,i+2])
    end
    return ba
end

"""Calculate bond dihedrals in a frame."""
@inline function bond_dihedral_frame(frame)
    (_, natom) = size(frame)
    bd = Vector{Float64}(undef,natom-3)
    @inbounds for i in 1:natom-3
        bd[i] = bond_dihedral(frame[:,i], frame[:,i+1], frame[:,i+2], frame[:,i+3])
    end
    return bd
end

"""Calculate the distribution of bond lengths in a trace"""
function bond_length_trace(trace, output)
    (dim, natom, nsteps) = size(trace)
    bond_lengths = Matrix{Float64}(undef,natom-1,nsteps)
    @inbounds for t in 1:nsteps
        bond_lengths[:,t] = bond_length_frame(dataset[:,:,t])
    end
    return bond_lengths
end
"""Calculate the distribution of bond angles in a trace"""
function bond_angle_trace(trace, output)
    (dim, natom, nsteps) = size(trace)
    bond_angles = Matrix{Float64}(undef,natom-2,nsteps)
    @inbounds for t in 1:nsteps
        bond_angles[:,t] = bond_angle_frame(dataset[:,:,t])
    end
    return bond_angles
end
"""Calculate the distribution of bond dihedrals in a trace"""
function bond_dihedral_trace(trace, output)
    (dim, natom, nsteps) = size(trace)
    bond_dihedrals = Matrix{Float64}(undef,natom-3,nsteps)
    @inbounds for t in 1:nsteps
        bond_dihedrals[:,t] = bond_dihedral_frame(dataset[:,:,t])
    end
    return bond_dihedrals
end
