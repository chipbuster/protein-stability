"""Calculate the autocorrelation lags for a simulation.

Takes a dxNxnsteps dataset and returns a vector with <nsteps> elements.

Currently does things in-memory because I'm lazy and haven't needed to not to.
"""
function sim_autocor(dataset::InputData)
    (dim, natoms, nsteps) = size(dataset.data)
    mydat = Array{Float64,3}(dataset.data[:,:,:])

    # Make each individual frame zero-mean
    mydat .= mydat .- mean(mydat; dims=2)

    vals =  collect(1:2:1000)
    cors = Vector{Float64}(undef, length(vals))

    @inbounds @threads for index in 1:length(vals)
        lag = vals[index]
        mycor = 0.0
        ctr = 0
        for i in 1:nsteps - lag
            mycor += cor(vec(mydat[:,:,i]), vec(mydat[:,:,i+lag]))
            ctr += 1
        end
        println(index, "/", length(vals))   # Crude progress meter
        cors[index] = mycor / ctr
    end
    return (vals,cors)
end

function sim_autocor_2d(data)
    (nangles, nsteps) = size(data)
    mydat = Array{Float64,2}(data[:,:])

    # Make each individual frame zero-mean
    mydat .= mydat .- mean(mydat; dims=2)

    vals =  collect(1:2:1000)
    cors = Vector{Float64}(undef, length(vals))

    @inbounds @threads for index in 1:length(vals)
        lag = vals[index]
        mycor = 0.0
        ctr = 0
        for i in 1:nsteps - lag
            mycor += cor(vec(mydat[:,i]), vec(mydat[:,i+lag]))
            ctr += 1
        end
        println(index, "/", length(vals))   # Crude progress meter
        cors[index] = mycor / ctr
    end
    return (vals,cors)
end
