"""Calculate the autocorrelation lags for a simulation.

Takes a dxNxnsteps dataset and returns a vector with <nsteps> elements.

Currently does things in-memory because I'm lazy and haven't needed to not to.
"""

function sim_autocov_adjusted(dataset::InputData)
    (dim, natoms, nsteps) = size(dataset.data)
    mydat = Array{Float64,3}(dataset.data[:,:,:])
    println(typeof(mydat))

    # Make each individual frame zero-mean
    mydat = mydat .- mean(mydat; dims=2)

    vals =  collect(1:200:nsteps)
    covs = Vector{Float64}(undef, length(vals))

    @threads for index in 1:length(vals)
        lag = vals[index]
        mycov = 0.0
        ctr = 0
        for i in 1:nsteps - lag
            x = vec(mydat[:,:,i])
            y = vec(mydat[:,:,i+lag])
            mycov += cov(x,y) / sqrt(var(x) * var(y))
            ctr += 1
        end
#        println(mycov, "   " ,lag)   # Crude progress meter
        covs[index] = mycov / ctr
    end
    return (vals,covs)
end

function sim_autocov(dataset::InputData)
    (dim, natoms, nsteps) = size(dataset.data)
    mydat = Array{Float64,3}(dataset.data[:,:,:])

    # Make each individual frame zero-mean                                                                          
    mydat = mydat .- mean(mydat; dims=2)

    vals =  collect(1:200:nsteps)
    covs = Vector{Float64}(undef, length(vals))

    @threads for index in 1:length(vals)
        lag = vals[index]
        mycov = 0.0
        ctr = 0
        for i in 1:nsteps - lag
            x = vec(mydat[:,:,i])
            y = vec(mydat[:,:,i+lag])
            mycov += cov(x,y)
            ctr += 1
        end
#        println(mycov, "   " ,lag)   # Crude progress meter
        covs[index] = mycov / ctr
    end
    return (vals,covs)
end

function sim_autocor(dataset::InputData)
    (dim, natoms, nsteps) = size(dataset.data)
    mydat = Array{Float64,3}(dataset.data[:,:,:])

    # Make each individual frame zero-mean
    mydat = mydat .- mean(mydat; dims=2)

    vals =  collect(1:200:nsteps)
    cors = Vector{Float64}(undef, length(vals))

    @threads for index in 1:length(vals)
        lag = vals[index]
        mycor = 0.0
        ctr = 0
        for i in 1:nsteps - lag
            mycor += cor(vec(mydat[:,:,i]), vec(mydat[:,:,i+lag]))
            ctr += 1
        end
#        println(mycov, "   " ,lag)   # Crude progress meter
        cors[index] = mycor / ctr
    end
    return (vals,cors)
end
