using Gadfly;
using ColorTypes

function literal_bits_pct_per_block(blocks::Vector{Block})
    [ literal_bits_compressed(b) / compute_compressed_size(b) for b in blocks ]
end

function backreference_bits_pct_per_block(blocks::Vector{Block})
    [ backreference_bits_compressed(b) / compute_compressed_size(b)  for b in blocks ]
end

function backreference_bits_pct_saved_per_block(blocks::Vector{Block})
    [ backreference_bits_saved(b) / compute_compressed_size(b)  for b in blocks ]
end

"""Call func on the two blocks and plot a comparison histogram"""
function comparison_histogram(blocks1, blocks2, func; label1="", label2="", title="", xlabel="")
    colors = [RGB(0, .5, 1), RGB(.88, 0, .37)]

    # The last block might be an outlier (due to partial blocks) so omit it
    nums1 = func(blocks1)
    nums2 = func(blocks2)
    l1 = layer(x=nums1, Geom.histogram(density=true), Theme(default_color=colors[1], alphas=[0.7], line_width=0mm))
    l2 = layer(x=nums2, Geom.histogram(density=true), Theme(default_color=colors[2], alphas=[0.7], line_width=0mm))

    p1 = plot(l1, l2,Guide.title(title), Guide.xlabel(xlabel), Guide.ylabel("Probability"),
            Guide.manual_color_key("Legend", [label1, label2], [colors[1], colors[2]]))
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
    title(vstack(c1,c2,c3), "Simple LZ77 Stats for N=$(N), R=$(R)")
end