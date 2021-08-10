includet("../volcons.jl")
includet("../mcmc-lib.jl")

using StatsPlots

function plot_frame(angles, hi)
    pts = points_from_angles(angles)
    limval = length(angles) + 1.0
    p = plot(xlim=(-limval,limval), ylim=(-limval,limval), size=(800,800), legend=false)
    plot!(p, pts[1,:], pts[2,:], color=:blue)
    scatter!(p, pts[1,:], pts[2,:])

    xs = [1.0 1.0]
    ys = [-limval, limval]
    plot!(p, [0.0, 0.0], ys, color=:red, linewidth=2)
    plot!(p, [hi,hi], ys, color=:red, linewidth=2)
    p
end

function plot_trace(fname, trace, hi)
    anim = @animate for i in 1:size(trace,2)
        println("Progress: $(i) of $(size(trace,2))")
        plot_frame(trace[:,i], hi)
    end
    gif(anim, fname, fps=60)
end

function plot_hdf5_file(infname, outfname, nframes=1000, nskip=1)
    GR.inline("png")
    ang = load_sim_angles(infname)
    mdata = load_sim_metadata(infname)
    hi = mdata.param1

    toplot = ang[:,range(1,step=nskip,length=nframes)]
    plot_trace(outfname, toplot, hi)
end