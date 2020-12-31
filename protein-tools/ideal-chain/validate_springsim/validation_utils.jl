using LinearAlgebra
using Discretizers
using Statistics
using StatsBase

include("/home/chipbuster/codes/protein-stability/protein-tools/core/geom_funcs.jl")
include("/home/chipbuster/codes/protein-stability/protein-tools/core/compressor.jl")

# For (almost) all tests, the input will be in a trace, which are
# (ndim, natom, nsteps) arrays of doubles.
const SimTrace = AbstractArray{Float64, 3}
ndim(x::SimTrace) = size(x)[1]
natom(x::SimTrace) = size(x)[2]
nstep(x::SimTrace) = size(x)[3]

pct_diff(x,y) = abs(y - x) / min(x,y)
rot_to_mat(θ) = [ cos(θ)  -sin(θ); sin(θ) cos(θ) ]

####################
## File Utilities ##
####################

function filename_to_label(fname)
    fn = basename(fname)
    fn = fn[1:end-7]
    (_, N, R) = split(fn,'_')
    "N=$(N),R=$(R)"
end

function load_file(fname, nframes=4096)::SimTrace
    rawtrace = deserialize(fname)
    nsteps = nstep(rawtrace)
    nskip = if nsteps < nframes
        1
    else
        nsteps ÷ nframes
    end
    collect(rawtrace[:,:,1:nskip:end])
end

#########################
## Entropy Computation ##
#########################
const nbins = 170
const lindisc = LinearDiscretizer(range(-pi, pi, length=nbins))

function absolute_angle_entropy(trace)
    angles = Matrix{Float64}(undef, natom(trace) - 1, nstep(trace))
    for t in 1:nstep(trace)
        bonds = diff(trace[:,:,t]; dims=2)
        angles[:,t] = [ atan(x[2],x[1]) for x in eachcol(bonds) ]
    end
    ravel_angles = vec(angles)
    binned = encode(lindisc, ravel_angles)
    compression_eta(convert(Vector{UInt8}, binned))
end

function internal_angle_entropy(trace)
    angles = bond_angle_trace(trace)
    ravel_angles = vec(angles)
    binned = encode(lindisc, ravel_angles)
    compression_eta(convert(Vector{UInt8}, binned))
end

function trace_entropy(trace)
    a = absolute_angle_entropy(trace)
    i = internal_angle_entropy(trace)
    (a,i)
end

function sim_autocor(mydat, lag)
    # Make each individual frame zero-mean
    mydat = mydat .- mean(mydat; dims=2)

    mycor = 0.0
    ctr = 0
    @inbounds for i in 1:size(mydat)[2] - lag
        mycor += cor(vec(mydat[:,i]), vec(mydat[:,i+lag]))
        ctr += 1
    end
    mycor / ctr
end

###########################
## Distribution Checking ##
###########################

function is_distributed_by(data, testfunction)
    """An internal function for testing whether data satisfies a given distribution.

    The testfunction should return true if it thinks the data satisfy the properties
    and false otherwise."""
    # If too many data points, hypothesis tests will basically almost always
    # fail. Solve this by subsampling data if too many points.
    dset_max_size = 3000

    if length(data) <= dset_max_size
        testfunction(data)
    else
        # This technically *can* fail due to unlucky sampling, so take best
        # 2/3 test results.
        passed = [ testfunction(sample(data, dset_max_size, replace=false)) for _ in 1:3 ]
        testres = map(x-> x ? 1 : 0, passed)
        sum(testres) > 1
    end
end

function is_unif_distrib(data, (a,b))
    """Use a simple KS test to determine if data is drawn uniformly from (a,b)"""
    hyp(x) = pvalue(ExactOneSampleKSTest(x, Uniform(a,b))) > 0.08
    is_distributed_by(data, hyp)
end

function is_gaussian_distrib(data, (μ, σ))
    """Use a simple KS test to determine if data is drawn from Gaussian (μ,σ) """
    hyp(x) = pvalue(ExactOneSampleKSTest(data, Normal(μ, σ))) > 0.08
    is_distributed_by(data, hyp)
end

#############################
## Other Utility Functions ##
#############################

# These computation functions technically need to know the mass of the particles
# and strength of springs, but since we're only calculating deviations, we can
# get away with defining the strength of the spring to be 1 force/distance, the
# mass of every atom to be one unit, and the timestep to be one unit.
const k = 1.0
const m = 1.0
const dt = 1.0

col_norms(A) = mapslices(norm, A; dims=1)
col_sqnorms(A) = mapslices( x -> sum(x.^2), A; dims=1)

function frame_pe(frame::AbstractArray{Float64,2})
    """Computes the potential (spring) energy in the given frame"""
    bondvecs = diff(frame; dims=2)
    bond_energies = 0.5 * k .* col_sqnorms(bondvecs)
    eebond = (frame[:,end] - frame[:,1])
    return sum(bond_energies) + 0.5 * k * sum(eebond.^2)
end

function frame_ke(frame1::Matrix{Float64}, frame2::Matrix{Float64})
    """Computes kinetic energy of the chain between two given frames"""
    error("Frame KE is meaningless without original simulation")
#    vel = frame2 - frame1
#    atom_energies = 0.5 * m .* col_sqnorms(vel)
#    return sum(atom_energies)
end

# Note: unverfied
function frame_radius_of_gyration(frame::AbstractArray{Float64,2})
    """Compute the radius of gyration for the given frame."""
    n = size(frame)[2]
    frame_mean = mean(frame; dims=2)
    1/n * sum( col_sqnorms(frame .- frame_mean) )
end

function compute_trace_energy(trace::SimTrace)
    """Returns a (2, nframe-1) matrix, where each column is a frame from the
    second frame to the end. The first row is the PE for that frame, the second
    row is the KE for that frame (from the previous frame)"""
    energy = Vector{Float64}(undef, nstep(trace) - 1)
    for t in 1:nstep(trace)-1
        energy[t] = frame_pe(trace[:,:,t])
    end
    energy
end

"""Returns matrix where each column is a vector of pairwise bond lengths
for a particular frame"""
function pairwise_bond_lengths(trace::SimTrace)
    pblens = Matrix{Float64}(undef, natom(trace) - 1, nstep(trace))

    # Check that no bond is horribly distended, assume restlength of 1.0
    for (ts, frame) in enumerate(eachslice(trace; dims=3))
        bonds = diff(frame; dims=2)
        blens = col_norms(bonds)
        pblens[:,ts] = blens
    end
    pblens
end

"""Returns a matrix of nsteps x 3. Each column is the angle of some bond
vector: first column is first bond, second is last bond, third is e2e """
function directional_vectors(trace::SimTrace)
    output = Matrix{Float64}(undef, nstep(trace), 3)

    first_bonds = dropdims(mapslices(x->x[:,2] - x[:,1], trace; dims=(1,2)); dims=2)
    last_bonds = dropdims(mapslices(x->x[:,end-1] - x[:,end], trace; dims=(1,2)); dims=2)
    e2e_vec = dropdims(mapslices(x->x[:,end] - x[:,1], trace; dims=(1,2)); dims=2)

    fb_angles = [ atan(x[2],x[1]) for x in eachcol(first_bonds )]
    lb_angles = [ atan(x[2],x[1]) for x in eachcol(last_bonds) ]
    e2e_angles = [ atan(x[2],x[1]) for x in eachcol(e2e_vec) ]

    output[:,1] = fb_angles
    output[:,2] = lb_angles
    output[:,3] = e2e_angles

    output
end

"""Returns a vector of chainlengths (arclength) per frame."""
function chainlengths(trace::SimTrace)
    chainlens = Vector{Float64}(undef, nstep(trace))
    for (ts, frame) in enumerate(eachslice(trace; dims=3))
        bonds = diff(frame; dims=2)
        blens = col_norms(bonds)
        chainlen = sum(blens)
        chainlens[ts] = chainlen
    end
    chainlens
end