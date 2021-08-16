using Base.Threads
using Statistics

function sim_autocov(mydat)
    (nangles, nsteps) = size(mydat)

    # Make each individual frame zero-mean
    mydat = mydat .- mean(mydat; dims=1)

    vals = 100:100:2000
    covs = Vector{Float64}(undef, length(vals))

    @inbounds @threads for index in 1:length(vals)
        lag = vals[index]
        mycov = 0.0
        ctr = 0
        @inbounds for i in 1:nsteps - lag
            x = vec(@view mydat[:,i])
            y = vec(@view mydat[:,i+lag])
            mycov += dot(x,y)
            ctr += 1
        end
        println(mycov, "   " ,lag)   # Crude progress meter
        covs[index] = mycov / ctr
    end
    return (vals,covs)
end
