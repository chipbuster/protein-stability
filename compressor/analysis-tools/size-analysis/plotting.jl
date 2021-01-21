using Plots;
using StatsBase;
using ColorTypes;

# create a transparent scatter plot with an 'annotation' that will become title
function title_plot(plots, text)
    y = ones(3) 
    title = Plots.scatter(y, marker=0,markeralpha=0, annotations=(2, y[2], Plots.text(text)),axis=false, leg=false,size=(200,100))

    # combine the 'title' plot with your real plots
    Plots.plot(
        title,
        plots,
        layout=grid(2,1,heights=[0.1,0.9])
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

function plot_raw_datasize(df)
    
end

"""Call func on the two blocks and plot a comparison histogram"""
function comparison_histogram(blocks1, blocks2, func; label1="", label2="", title="", xlabel="")
    colors = [RGB(0, .5, 1), RGB(.88, 0, .37)]

    nums1 = func(blocks1)
    nums2 = func(blocks2)

end

function compare_datasets(data1, data2, name1, name2, globalpararms)
    (N, R) = globalpararms
    set_default_plot_size(10cm,30cm)
    c1 = comparison_histogram(data1, data2, literal_bits_pct_per_block, 
                         label1=name1, label2=name2,
                         title="Fraction of literal bits per block", xlabel="Fraction")
    c2 = comparison_histogram(data1, data2, backreference_bits_pct_per_block, 
                         label1=name1, label2=name2,
                         title="Fraction of Backreference bits per block", xlabel="Fraction")
    c3 = comparison_histogram(data1, data2, backreference_bits_pct_saved_per_block, 
                         label1=name1, label2=name2,
                         title="Space Saved by Backreference per block", xlabel="Fraction")
    title_plot(vstack(c1,c2,c3), "Simple LZ77 Stats for N=$(N), R=$(R)")
end
