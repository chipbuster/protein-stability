"""Get a vector from atom[0] to atom[end] for each frame."""
function end_effector_vec(data)::Matrix{Float64}
    (dim, natom, nsteps) = size(data)
    eeffs = Matrix{Float64}(undef, 2, nsteps)
    for t in 1:nsteps
        eeffs[:,t] = data[:,end,t] - data[:,1,t]
    end
    return eeffs
end

"""Compute the bond vector for every bond in the trace"""
function bond_ends_vec(data)::Array{Float64,3}
    (dim, natom, nsteps) = size(data)
    bondvec = Array{Float64,3}(undef, 2, natom-1, nsteps)
    for t in 1:nsteps
        for n in 1:natom-1
           bondvec[:,n,t] = data[:,n+1,t] - data[:,n,t]
        end
    end
    return bondvec
end
