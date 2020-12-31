using Plots
using LinearAlgebra

"""Given a list of filenames and a plotting function, generates a vertical
display of the plotting function applied to all elements of that list"""
function gen_allplot_for_plotfun(filelist, failed_list, plotfun, labelfun, outfile)
    plist = Vector{Any}(undef, length(filelist))
    l = SpinLock()
    @threads for i in 1:length(filelist)
        filename = filelist[i]
        failed = filename ∈ failed_list
        data = load_file(filename, 1 << 15) #Get some extra data for plots
        @assert(length(data) > 0)
        label = labelfun(filename)
        plist[i] = plotfun(data, label, failed)
    end
    n = length(plist)
    p = plot(plist..., layout=(n,1), size=(1400, 500n))
    png(p, outfile)
end

function plot_pairwise_bondlen_distrib(trace, label_text="", failed=false)
    blens = vec(pairwise_bond_lengths(trace))
    color = failed ? :red : :blue
    histogram(blens, xlabel="Length (Multiple of restlen)", title="Pairwise Bondlen " * label_text, legend=false, color=color)
end

function plot_dir_deps(trace, label_text="", failed=false)
    # This print is needed to avoid a type error where the histogram function
    # attempts to access element [0] of an Array{Symbol,1} instead of the
    # correct datatype. Don't ask me wtf is going on there (probably some sort
    # of race, but I have no idea how that could possibly work)
    print("Type of trace is $(typeof(trace))")

    angles = directional_vectors(trace)
    @views fb_angles = angles[:,1]
    @views lb_angles = angles[:,2]
    @views e2e_angles = angles[:,3]

    color = failed ? :red : :blue
    label = plot(legend=false,grid=false,foreground_color_subplot=:white,xlim=(0,1),ylim=(0,1), annotations=(0.0,0.6,label_text))
    h1 = histogram(fb_angles, xlabel="Bond Angle", title="First Angle", legend=false, color=color)
    h2 = histogram(lb_angles, xlabel="Bond Angle", title="Last Angle",legend=false, color=color)
    h3 = histogram(lb_angles, xlabel="Bond Angle", title="E2E Angle",legend=false, color=color)

    plot(label, h1,h2,h3, layout=(1,4), size=(700,300))
end

function plot_chainlength_distrib(trace, label_text="", failed=false)
    color = failed ? :red : :blue
    ls = chainlengths(trace)

    histogram(ls, xlabel="Chainlength (Multiple of restlen)", title="Chainlengths " * label_text, legend=false, color=color)
end

function plot_autocor_decay(trace, label_text="", failed=false)
    color = failed ? :red : :blue
    angles = bond_angle_trace(trace)

    ys = []
    for n in 1:100
        push!(ys, sim_autocor(angles, n))
    end

    (big,small) = extrema(ys)
    mmtext = " (Min:$(small), Max:$(big)"

    plot(1:100, ys, title="Autocor for "*label_text*mmtext, ylabel="Autocorrelation", xlabel="τ", legend=false, color=color)
end

function plot_phys_quant_halves(trace, label_text="", failed=false)
    data = trace
    mid = nstep(data) ÷ 2
    half1 = data[:,:,1:mid]
    half2 = data[:,:,mid+1:end]

    pe1 = [ frame_pe(f) for f in eachslice(half1; dims=3) ]
    pe2 = [ frame_pe(f) for f in eachslice(half2; dims=3) ]

    rg1 = [ frame_radius_of_gyration(f) for f in eachslice(half1; dims=3) ]
    rg2 = [ frame_radius_of_gyration(f) for f in eachslice(half2; dims=3) ]

    pe_h = plot(xlabel="Potential Energy (AU)", ylabel="Frequency", title="PE for "*label_text)
    histogram!(pe_h, pe1, color=:blue, alpha=0.6, label="1st half", linealpha=0.1)
    histogram!(pe_h, pe2, color=:red,  alpha=0.6, label="2nd half", linealpha=0.1)

    rog_h = plot(xlabel="Radius of Gyration (AU)", ylabel="Frequency", title="RoG for "*label_text)
    histogram!(rog_h, rg1, color=:blue, alpha=0.6, label="1st half", linealpha=0.1)
    histogram!(rog_h, rg2, color=:red, alpha=0.6, label="2nd half", linealpha=0.1)

    p = plot(pe_h, rog_h, layout=(1,2), size=(800,400))
end
