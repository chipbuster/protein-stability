using Plots;
using Statistics;
using StatsBase;
using LinearAlgebra;
using Distributions;
using HypothesisTests;
using Base.Threads;
using Serialization;
using DataStructures;

include("validation_utils.jl")
include("plot_validation.jl")

#####################
## Validity Checks ##
#####################

# Questionable function: leave unimplemented for now.
function check_boltzmann_distrib(trace::SimTrace)
    """Check that the energy of the simuation is approximately Boltzmann"""
    error("This test does not make sense without recorded velocities")
end

function pairwise_bondlength_distrib_ok(trace::SimTrace)
    """Verify that the pairwise bondlength distribution is reasonable."""
    blens = vec(pairwise_bond_lengths(trace))

    if any(blens .> 2.0)
        println("Bond length anomaly on frame $(ts)")
        return false
    end

    if !is_gaussian_distrib(blens, (mean(blens), std(blens)))
        println("Pairwise bond lengths are not gaussian distributed")
        return false
    end

    return true
end

function no_directional_dependence(trace)
    """Verify that the simulation does not have a preferred axis (should only
    be applied to unpinned simulations"""
    # Test first bond, last bond, and end-effector distributions

    angles = directional_vectors(trace)
    @views fb_angles = angles[:,1]
    @views lb_angles = angles[:,2]
    @views e2e_angles = angles[:,3]

    bounds = (-pi, pi)

    is_unif_distrib(fb_angles, bounds) && is_unif_distrib(lb_angles, bounds) && is_unif_distrib(e2e_angles, bounds)
end

function chainee_distrib_is_gaussian(trace)
    """Verify that the chain end-to-end vector is reasonable.

    Cannot be reasonably used on constrained chains.
    """
    # Since the chain ee vector is a sum of RVs, it should have a gaussian
    # distribution when projected onto any axis: we choose +x, +y (because easy)

    ee_vec = mapslices(x->x[:,end] - x[:,1], trace; dims=(1,2))
    ee_vec = dropdims(ee_vec, dims=2)  # Place eevecs in columns of matrix

    xs = ee_vec[1,:]
    ys = ee_vec[2,:]

    xs_param = mean(xs), std(xs)
    ys_param = mean(ys), std(ys)
    is_gaussian_distrib(xs, xs_param) && is_gaussian_distrib(ys, ys_param)
end

function chainlength_distrib_ok(trace)
    """Verify that the sum of bond lengths (chain length) is Gaussian.

    """
    target_chainlength = natom(trace) - 1.0
    allowed_variation = 0.10 * target_chainlength + 1.5

    cls = chainlengths(trace)

    for cl in cls
        if abs(cl - target_chainlength) > allowed_variation
            println("Chainlength anomaly on frame $(ts): length is $(chainlen) on a target of $(target_chainlength)")
            return false
        end
    end

    return true
end

function timereversed_entropy_same(trace, nskip=1)
    """Verify that the time-reversed entropy estimate is similar to the forward one."""
    data = trace[:,:,1:nskip:end]

    reversed = data[:,:,end:-1:1]
    (a_forward, i_forward) = trace_entropy(data)
    (a_reverse, i_reverse) = trace_entropy(reversed)

    abs(a_forward - a_reverse) < 0.05  && abs(i_forward - i_reverse) < 0.05
end

function twohalf_entropy_same(trace, nskip=1)
    """Verify that the entropy of the two halves of the simulation are similar.
    This is done to check for burn-in effects"""
    data = trace[:,:,1:nskip:end]

    mid = nstep(data) ÷ 2
    half1 = data[:,:,1:mid]
    half2 = data[:,:,mid+1:end]

    (a1, i1) = trace_entropy(half1)
    (a2, i2) = trace_entropy(half2)

    abs(a1 - a2) < 0.05 && abs(i1 - i2) < 0.05
end

function twohalf_physquant_same(trace, nskip=1)
    """Tests that the potential energy and radius of gyration do not significantly
    differ between the first and second half of the simulation."""

    data = trace[:,:,1:nskip:end]

    mid = nstep(data) ÷ 2
    half1 = data[:,:,1:mid]
    half2 = data[:,:,mid+1:end]

    pe1 = [ frame_pe(f) for f in eachslice(half1; dims=3) ]
    pe2 = [ frame_pe(f) for f in eachslice(half2; dims=3) ]

    rg1 = [ frame_radius_of_gyration(f) for f in eachslice(half1; dims=3) ]
    rg2 = [ frame_radius_of_gyration(f) for f in eachslice(half2; dims=3) ]

    pe_test = ApproximateTwoSampleKSTest(pe1, pe2)
    rg_test = ApproximateTwoSampleKSTest(rg1, rg2)

    pvalue(pe_test) > 0.1 && pvalue(rg_test) > 0.1
end

function random_isometry_invariant(trace)
    """Verify that the compression ratio does not change if the simulation is
    transformed by a random isometry (translation + rotation + mirroring)"""
    translation = natom(trace) * rand(2)
    θ1 = 2 * pi * (rand() - 0.5)
    θ2 = 2 * pi * (rand() - 0.5)
    r1 = rot_to_mat(θ1)
    r2 = rot_to_mat(θ2)
    refl_mat = rand(Bool) ? [1 0; 0 -1] : [1 0; 0 1]

    # A function to apply the isometry to a single vector
    isom(x) = (r2 * refl_mat * r1 * x) + translation
    transformed = mapslices(isom, trace, dims=1)
    
    (a_orig, i_orig) = trace_entropy(trace)
    (a_tran, i_tran) = trace_entropy(transformed)

    abs(a_orig - a_tran) < 0.03 && abs(i_orig - i_tran) < 0.03
end

function test_isometry_invariance(trace)
    """Verify that the compression ratio does not change if the simulation is
    transformed by a random isometry (translation + rotation + mirroring)"""
    for _ in 1:10
        if ! random_isometry_invariant(trace)
            return false
        end
    end
    return true
end

function angle_autocor_degradation(trace, nskip=1)
    """Verify that the autocorrelation value around the chosen number of skipped
    timesteps is low in an angles representation"""
    angles = bond_angle_trace(trace)
    ac = abs(sim_autocor(angles, nskip))
    ac < 0.05
end

function validate_file(fname, target_nframes)
    rawtrace = deserialize(fname)
    nsteps = nstep(rawtrace)
    nskip = if nsteps < target_nframes
        1
    else
        nsteps ÷ target_nframes
    end

    # Just do the subsetting before testing...
    trace = collect(rawtrace[:,:,1:nskip:end])

    test_res = Dict(
        "pairwise_bondlength_distrib_ok" => pairwise_bondlength_distrib_ok(trace),
        "no_directional_dependence" => no_directional_dependence(rawtrace),
        # "chainee_distrib_is_gaussian" => chainee_distrib_is_gaussian(trace),
        "chainlength_distrib_ok" => chainlength_distrib_ok(trace),
        "timereversed_entropy_same" => timereversed_entropy_same(trace),
        "twohalf_entropy_same" => twohalf_entropy_same(trace),
        "twohalf_physquant_same" => twohalf_physquant_same(trace),
        "isometry_invariant" => test_isometry_invariance(trace),
        "angle_autocor_degradation" => angle_autocor_degradation(trace)
    )

    output = []
    for (name, res) in test_res
        if res != true
            push!(output, name)
        end
    end
    output
end

function usage()
    println("Usage: $(PROGRAM_FILE) <plotopt> [plotdir] <inputs>")
    println("\tplotopt: When to generate plots. \"always\" or \"never\"")
    println("\tplotdir: Directory to generate output plots in. Only needs to be specified if <plotopt> is not \"never\"")
    println("\tinputs: One or more serialized traces to process")
end

function filename_to_label(fname)
    fn = basename(fname)
    fn = fn[1:end-7]
    (_, N, R) = split(fn,'_')
    "N=$(N),R=$(R)"
end

function main()
    if ARGS[1] == "-h" || ARGS[1] == "--help" || length(ARGS) < 3
        usage()
        exit(0)
    end

    if ARGS[1] != "always" && ARGS[1] != "never"
        usage()
        exit(1)
    end

    targetfiles = if ARGS[1] == "never"
        ARGS[2:end]
    else
        ARGS[3:end]
    end

    failed_tests = DefaultDict([])

    nsamples_per_test = 4096

    l = SpinLock()
    @threads for fname in targetfiles
        tests_failed = validate_file(fname, nsamples_per_test)
        if length(tests_failed) != 0
            lock(l)
                println("File $(fname) failed validation:")
                for t in tests_failed
                    println("\t Failed $(t)")
                    push!(failed_tests[t], fname)
                end
            unlock(l)
        end
    end

    if ARGS[1] == "never"
        println("Not generating plots because \"never\" specified for plots.")
        exit(0)
    elseif ARGS[1] == "always"
        println("Begin plotting phase.")
        outdir = ARGS[2]

        ## We know how to generate plots for 6 tests: pairwise length,
        ## angles, chainlengths, autocorrelation, and twohalf physquant. Do each
        ## in turn

        # Pairwise lengths
        failed = failed_tests["pairwise_bondlength_distrib_ok"]
        outname = joinpath(outdir, "pairwise_bondlengths.png")
        gen_allplot_for_plotfun(targetfiles, failed, plot_pairwise_bondlen_distrib,
                                filename_to_label, outname)

        # angles
        failed = failed_tests["no_directional_dependence"]
        outname = joinpath(outdir, "chain_directional_dependence.png")
        gen_allplot_for_plotfun(targetfiles, failed, plot_dir_deps, 
                                filename_to_label, outname)

        # chainlengths
        failed = failed_tests["chainlength_distrib_ok"]
        outname = joinpath(outdir, "chain_lengths.png")
        gen_allplot_for_plotfun(targetfiles, failed, plot_chainlength_distrib,
                                filename_to_label, outname)

        # autocorrelation
        failed = failed_tests["angle_autocor_degradation"]
        outname = joinpath(outdir, "autocorrelation.png")
        gen_allplot_for_plotfun(targetfiles, failed, plot_autocor_decay,
                                filename_to_label, outname)

        # twohalf physquant
        failed = failed_tests["twohalf_physquant_same"]
        outname = joinpath(outdir, "twohalf_physquant.png")
        gen_allplot_for_plotfun(targetfiles, failed, plot_phys_quant_halves,
                                filename_to_label, outname)
    else
        error("Unreachable")
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end