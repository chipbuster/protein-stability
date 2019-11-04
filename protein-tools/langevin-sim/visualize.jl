using Plots
using DifferentialEquations
using Serialization
using ImageMagick
using LinearAlgebra

datfile = "1EN2.pdb.results"

"""Given a Hamiltonian solution, gets the positions as a 3xNxTimesteps matrix

result[:,:,i] is the 3xN result of the i-th timestep
"""
function extract_positions(hamiltonian_sol)
    (numel, numtimesteps) = size(hamiltonian_sol)
    # Size must be divisible by 2 (to get momentum and positions components) and then
    # each solution must have size divisible by 3 (for 3 coordinates per element)
    @assert(numel % 6 == 0, "Wrong number of elements for a 3D Hamiltonian Solution")
    numcoords = numel ÷ 2
    numatoms = numcoords ÷ 3
    result = Array{Float64,3}(undef, 3, numatoms, numtimesteps)
    for i in 1:numtimesteps
        # Extract the positions, which are in the second half of the solution vector
        data = hamiltonian_sol[numcoords + 1:end, i]
        result[:,:,i] = reshape(data, (3, numatoms))
    end
    return result
end

"""Given a 3xNxtimesteps set of positions, generates an animation object"""
function gen_animation(positions)
    (_, _, timesteps) = size(positions)
    anim = @animate for i = 1:timesteps
            plot(positions[1,:,i], positions[2,:,i], positions[3,:,i],
            seriestype = :scatter, markersize = 4)
           end every 10
    return anim
end

data = deserialize(datfile)
positions = extract_positions(data)
animation = gen_animation(positions)
gif(animation, "/tmp/" * datfile * ".gif", fps = 30)