"""Get a vector from atom[0] to atom[end] for each frame."""
function end_effector_vec(dataset::InputData)::Matrix{Float64}
    (dim, natom, nsteps) = size(dataset.data)
    eeffs = Matrix{Float64}(undef, 3, nsteps)
    @inbounds for t in 1:nsteps
        eeffs[:,t] = dataset.data[:,end,t] - dataset.data[:,1,t]
    end
    return eeffs
end
