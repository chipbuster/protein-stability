using Plots;
using StatsBase;
using ColorTypes;

# create a transparent scatter plot with an 'annotation' that will become title
function title_plot(plots, text)
    y = ones(3) 
    title = Plots.scatter(y, marker=0, markeralpha=0, annotations=(2, y[2], Plots.text(text)), axis=false, leg=false, size=(200, 100))

    # combine the 'title' plot with your real plots
    Plots.plot(
        title,
        plots,
        layout=grid(2, 1, heights=[0.1,0.9])
    )
end

function literal_bits_pct_per_block(blocks::Vector{Block})
    [ literal_bits_compressed(b) / compute_compressed_size(b) for b in blocks ]
end

function backreference_bits_pct_per_block(blocks::Vector{Block})
    [ backreference_bits_compressed(b) / compute_compressed_size(b)  for b in blocks ]
end

function backreference_bits_pct_saved_per_block(blocks::Vector{Block})
    [ backreference_bits_saved(b) / compute_compressed_size(b)  for b in blocks ]
end

function backreference_distances(blocks::Vector{Block})
    stream = get_lzstream(blocks)
    [ b.dist for b in stream if isa(b, Backreference) ]
end

function backreference_lengths(blocks::Vector{Block})
    stream = get_lzstream(blocks)
    [ b.length for b in stream if isa(b, Backreference) ]
end

literal_bits_pct_per_block(b::DEFLATEStream) = literal_bits_pct_per_block(b.blocks)
backreference_bits_pct_per_block(b::DEFLATEStream) = backreference_bits_pct_per_block(b.blocks)
backreference_bits_pct_saved_per_block(b::DEFLATEStream) = backreference_bits_pct_saved_per_block(b.blocks)
backreference_distances(b::DEFLATEStream) = backreference_distances(b.blocks)
backreference_lengths(b::DEFLATEStream) = backreference_lengths(b.blocks)

macro compare_distributions(data1, data2, func, name1, name2, xlabel, title)
    quote
        begin
            pts1 = $(esc(func))($esc(data1))
            pts2 = $(esc(func))($esc(data2))

            p = plot(xlabel=$xlabel, title=$title, ylabel="Frequency")
            p = histogram!(p, pts1, label=$name1, normalize=:pdf, linealpha=0.2, alpha=0.7)
            p = histogram!(p, pts2, label=$name2, normalize=:pdf, linealpha=0.2, alpha=0.7)
            p
        end
    end
end

function compare_datasets_bitsused(data1, data2, name1, name2, globalpararms)
    (N, R, sortty) = globalpararms
    c1 = @compare_distributions(data1, data2, literal_bits_pct_per_block, 
                        name1, name2, "Fraction", "Fraction of literal bits per block")
    c2 = @compare_distributions(data1, data2, backreference_bits_pct_per_block, 
                        name1, name2, "Fraction", "Fraction of Backreference bits per block")
    c3 = @compare_distributions(data1, data2, backreference_bits_pct_saved_per_block, 
                        name1, name2, "Fraction", "Space Saved by Backreference per block")
    title_plot(vstack(c1, c2, c3), "Simple LZ77 Stats for N=$(N), R=$(R), $(sortty)")
end

function compare_datasets_backreferences(data1, data2, name1, name2, globalparams)
    (N, R, sortty) = globalparams
    c1 = @compare_distributions(data1, data2, backreference_distances,
                               name1, name2, "Distances", "Backreference Distances")
    c2 = @compare_distributions(data1, data2, backreference_lengths,
                               name1, name2, "Lengths", "Backreference Lengths")
    title_plot(vstack(c1, c2), "Backreference Stats for N=$(N), R=$(R), $(sortty)")
end
