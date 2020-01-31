using LinearAlgebra

"""Calculate the distribution of bond lengths"""
function bond_lengths(dataset::InputData)::Matrix{Float64}
    (dim, natom, nsteps) = size(dataset.data)
    bond_lens = Array{Float64,2}(undef,natom-1,nsteps)
    @inbounds for t in 1:nsteps
        frame = dataset.data[:,:,t]
        @inbounds for i in 1:natom-1
            bond_lens[i,t] = norm(frame[:,i+1] - frame[:,i])
        end
    end
    return bond_lens
end

"""Calculate the angle between two vectors."""
function calc_vector_angle(vec1::Vector{Float64}, vec2::Vector{Float64})
    num = dot(vec1,vec2)
    den = norm(vec1) * norm(vec2)
    return acos(num/den)
end

"""Calculate the distribution of bond angles."""
function bond_angles(dataset::InputData)
    (dim, natom, nsteps) = size(dataset.data)
    bond_angles = Array{Float64,2}(undef,natom-2,nsteps)
    @inbounds for t in 1:nsteps
        frame = dataset.data[:,:,t]
        @inbounds for i in 1:natom-2
            bond_angles[i,t] = calc_vector_angle(frame[:,i+2] - frame[:,i+1],
                                      frame[:,i+1] - frame[:,i])
        end
    end
    return bond_angles
end
