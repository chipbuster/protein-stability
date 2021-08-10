includet("../eecons.jl")
includet("../mcmc-lib.jl")

using StatsPlots

function plot_frame(angles, lo, hi)
    pts = points_from_angles(angles)
    p = plot(xlim=(-hi,hi), ylim=(-hi,hi), size=(800,800), legend=false)
    plot!(p, pts[1,:], pts[2,:], color=:blue)
    scatter!(p, pts[1,:], pts[2,:])
    scatter!(p, [pts[1,end]], [pts[2,end]], color=:green, markersize=8)

    θs = range(0.0, 2π, length=100)
    xs = cos.(θs)
    ys = sin.(θs)
    plot!(p, lo*xs, lo*ys, color=:red, linewidth=2)
    plot!(p, hi*xs, hi*ys, color=:red, linewidth=2)
    p
end

function plot_trace(fname, trace, lo, hi)
    anim = @animate for i in 1:size(trace,2)
        println("Progress: $(i) of $(size(trace,2))")
        plot_frame(trace[:,i], lo, hi)
    end
    gif(anim, fname, fps=60)
end

function plot_hdf5_file(infname, outfname, nframes=1000, nskip=1)
    GR.inline("png")
    ang = load_sim_angles(infname)
    mdata = load_sim_metadata(infname)
    lo = mdata.param1
    hi = mdata.param2

    toplot = ang[:,range(1,step=nskip,length=nframes)]
    plot_trace(outfname, toplot, lo, hi)
end