using Gadfly;
using Cairo;
using DataFrames;

include("size-analysis/lztypes.jl")
include("size-analysis/lzutils.jl")
include("size-analysis/lzanalysisbase.jl")
include("size-analysis/read_json.jl")
include("size-analysis/plotting.jl")

"""Plotting function which hardcodes directory structure"""
function plot_diffs(datadir, N, f, outfile)
    fname_3AA = "output_$(N)_$(f).rel.bin.json"
    fname_2AA = "output_$(N)_$(f).abs.bin.json"

    data_3AA = parse_gzip_json(joinpath(datadir, fname_3AA))
    data_2AA = parse_gzip_json(joinpath(datadir, fname_2AA))

    p = compare_datasets(data_2AA, data_3AA, "2AA", "3AA", (N, N*f))
    draw(SVG(outfile, 21cm, 9cm), p)
    p
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