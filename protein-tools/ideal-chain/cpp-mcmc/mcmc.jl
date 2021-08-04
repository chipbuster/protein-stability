using StaticArrays
using LinearAlgebra
using Logging

struct MCMCSimParams
    nangles::Int
    nsamples::Int
    r0::Float64
    dr::Float64
    skipct::Int
end

lohi(x::MCMCSimParams) = Base.extrema((x.r0, x.r0+x.dr))

function gen_end_point(ϕs)
    θs = ϕs .+ π |> cumsum .|> mod2pi
    endpt = @SVector [1.0, 0.0]
    for θ in θs
        endpt += @SVector [ cos(θ), sin(θ) ]
    end
    endpt
end

e2e_dist(ϕs) = gen_end_point(ϕs) |> norm

"""Generate another step of a Markov chain. Returns whether the step was 
accepted and modifies the value of `cur` with the next step"""
function mcmc_step(cur, prev, r_lo, r_hi, stepsz)
    cur .= prev
    for i in 1:size(cur,1)
        cur[i] += stepsz * randn()
    end
    cur .= mod2pi.(cur)
    
    e2e_d = norm(gen_end_point(cur))
    if r_lo < e2e_d < r_hi
        return true
    else
        cur .= prev
        return false
    end
end

"""Find a valid initial state for the chain"""
function find_init_state(nangles, r_lo, r_hi)
    #= A chain with angles all equal to π is fully extended. Hence r_hi must be
    #less than its e2e dist (or r_hi is invalid).
    #  A chain with angles all equal to π - 2π/(nangles+1) must have e2e dist zero
    #since it forms a polygon.
    #  Finally, the e2e dist is continuous in the angles.
    #  Use these facts to find a valid initial state by bisection. =#
    target_e2e = (r_hi + r_lo) / 2
    hi = π
    lo = π - 2π / (nangles + 1)
    mid = (hi + lo) / 2

    # End to end distance if *all* angles have value x
    e2e_allp(x) = e2e_dist(ones(nangles) .* x)

    @assert(e2e_allp(hi) >= r_hi - 1e-10, "Hi endpoint below target r_hi")
    @assert(e2e_allp(lo) <= r_lo + 1e-10, "Lo endpoint above target r_lo")

    ctr = 0
    while e2e_allp(mid) > r_hi || e2e_allp(mid) < r_lo
        if e2e_allp(mid) > target_e2e
            hi = mid
        else
            lo = mid
        end
        ctr += 1
        mid = (hi + lo) / 2
        if ctr > 100
            @error "Unable to find appropriate starting point for N=$(nangles)"
            return nothing
        end
    end

    return ones(nangles) * mid
end
 
"""Sample an nangle chain with nsamples MCMC steps. Hard constraint
end-to-end distance between r0 and r0 + dr, skipping skipct MCMC steps
between each recorded step."""
function sample_chain(simparams, initstep=1)
    # Angles are internal angle (3-atom). E2E distance is measured assuming
    # that all bonds are length 1.0 and the first bond points directly along
    # the +x axis.

    output = zeros(simparams.nangles, simparams.nsamples)

    (r_lo, r_hi) = lohi(simparams)
    stepsz = simparams.dr / 2
    acc = 0 ## Track the number of accepted samples

    curstate = find_init_state(simparams.nangles, r_lo, r_hi)
    prevstate = copy(curstate)

#=
    if initstep == 1
        curstate = find_init_state(simparams.nangles, r_lo, r_hi)

        # Really crude heuristic: burn in for 20x the simparams.skipct. If more is
        # needed it can be excluded from the output
        burnin_factor = 20
        prevstate = copy(curstate)
        for _ in 1:burnin_factor, _ in 1:simparams.skipct
            acc += mcmc_step(curstate, prevstate, r_lo, r_hi, stepsz)
            prevstate .= curstate
        end

        # Check the acceptance ratio for the burn in period
        ratio = acc / (burnin_factor * simparams.skipct)
        if ratio < 1/simparams.skipct
            @warn """r0 = $(simparams.r0), dr = $(simparams.dr) had an acceptance
            ratio of $(ratio) during the burn-in phase You might want to consider
            skipping more points or changing the stepsize""" 
        end

        acc = 0 # Reset acceptance ratio for main run
    end
=#

    for i in initstep:simparams.nsamples
        for _ in 1:simparams.skipct
            acc += mcmc_step(curstate, prevstate, r_lo, r_hi, stepsz)
            prevstate .= curstate
        end
        output[:,i] .= curstate
    end

    # Check the acceptance ratio for the main simulation
    ratio = acc / (simparams.nsamples * simparams.skipct)
    if ratio < 1/simparams.skipct
        @warn """r0 = $(simparams.r0), dr = $(simparams.dr) had an acceptance
        ratio of $(ratio) during the main phase You might want to consider
        skipping more points or changing the stepsize"""
    end

    return output
end

# Legacy: only used for notebook tests
function run_mcmc_test(natoms, nsamples, r0, dr)
    output = zeros(natoms, nsamples)
    r_lo = r0
    r_hi = r0 + dr
    stepsz = dr / 2

    output[:,1] = find_init_state(natoms, r_lo, r_hi)
    accept = 0
    reject = 0 
    for i in 2:nsamples
        cur = @view output[:, i]
        prev = @view output[:, i-1]
        if mcmc_step(cur, prev, r_lo, r_hi, stepsz)
            accept += 1
        else
            reject += 1
        end
    end
    println("Accepted $(accept) and rejected $(reject) steps")
    return (output[:,begin:50:end], (accept, reject))
end

