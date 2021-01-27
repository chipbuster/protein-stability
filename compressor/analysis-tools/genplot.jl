using Plots;
using StatPlots;
using DataFrames;

include("size-analysis/mod.jl")
include("../../protein-tools/core/compressor.jl")

"""Plotting function which hardcodes directory structure"""
function plot_diffs(datadir, N, f, outfile)
    fname_3AA = "output_$(N)_$(f).rel.bin.json"
    fname_2AA = "output_$(N)_$(f).abs.bin.json"

    data_3AA = parse_gzip_json(joinpath(datadir, fname_3AA))
    data_2AA = parse_gzip_json(joinpath(datadir, fname_2AA))

    p = compare_datasets(data_2AA, data_3AA, "2AA", "3AA", (N, N * f))
    draw(SVG(outfile, 10cm, 30cm), p)
    p
end

function getNRS(fn)
    fn = basename(fn)
    dotparts = split(fn, '.')
    if dotparts[end] != "gz"
        error("Not a GZIP")
    end
    nosuf = join(dotparts, '.')
    parts = split(dotparts, '_')
    N = parse(Int, parts[2])
    f = parse(Float64, parts[3])
    (N, N * f)
end

function plot_size_dependence(datadir)
    df = DataFrame(N=Int[], R=Float64[], fsize=Float64[], )
    for file in readdir(datadir)
        (N, R) = getNR(file)
        S 
    end
    p = plot(data, color=:)
end

function plot_diffs_preset(indir, outdir)
    Ns = [10,20,30]
    fs = 0.1:0.1:0.9
    for N in Ns, f in fs
        println("Plotting $(N), $(f)")
        outname = joinpath(outdir, "lzbreakdown_$(N)_$(f).svg")
        plot_diffs(indir, N, f, outname)
    end
end

function hardcode_plot_dir()
    indir = "$(homedir())/tmp/gzip-data/jsons/pinreflect"
    outdir = "$(homedir())/tmp/gzip-data/plots"
    plot_diffs_preset(indir, outdir)
end

plot_diffs(ARGS[1], parse(Int, ARGS[2]), parse(Float64, ARGS[3]), ARGS[4])