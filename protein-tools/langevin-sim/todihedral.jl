using LinearAlgebra
using Serialization
using DelimitedFiles

"""Take in a 3xNxtime set of coordinates and return a set of dihedral angles of
for 2Nxtime

Output format follows that found in the Python code:

 [[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
 [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
 ] etc. etc.


"""
# Algorithm taken from https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates
function to_dihedral(frames)
    (_, natom, timesteps) = size(frames)
    dihedrals = zeros(natom-3, timesteps)
    for i = 1:timesteps
        dihedrals[:,i] = frame_dihedrals(frames[:,:,i])
    end
    return dihedrals
end

"""Takes a single 3xN snapshot and returns a 2*N 1D vector containing the
interleaved phi-psi coordinates"""
function frame_dihedrals(frame)
    (_, N) = size(frame)
    dihedrals = Vector{Float64}()
    for i = 1:(N-3)
        a1 = frame[:,i]
        a2 = frame[:,i+1]
        a3 = frame[:,i+2]
        a4 = frame[:,i+3]
        b1 = a2 - a1
        b2 = a3 - a2
        b3 = a4 - a3
        n1 = cross(b1,b2)
        n2 = cross(b2,b3)
        n1 = n1 ./ norm(n1)
        n2 = n2 ./ norm(n2)
        m1 = cross(n1, b2 / norm(b2))
        x = dot(n1,n2)
        y = dot(m1,n2)
        d = atan(y,x)
        push!(dihedrals, d)
    end
    return dihedrals
end

indata = deserialize(ARGS[1])
outdata = to_dihedral(indata)
writedlm(ARGS[2],outdata)