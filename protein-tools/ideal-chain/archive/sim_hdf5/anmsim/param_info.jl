include("langevin_sim.jl")
include("run_anm.jl")

using Printf;

# Taken from https://stackoverflow.com/a/52213830
function commas(num::Integer)
    str = string(num)
    return replace(str, r"(?<=[0-9])(?=(?:[0-9]{3})+(?![0-9]))" => ",")
end

# Nicked from https://www.cs.nmsu.edu/~mleisher/julia/hrbc.jl
#
# Unit strings and denominators for binary division.
#
units   = ["K",  "M",  "G",  "T",  "P"]
bin_div = [0x1p10, 0x1p20, 0x1p30, 0x1p40, 0x1p40]

function humanReadableByteCountSI(bytes::Int64)
    neg = bytes < 0 ? "-" : ""
    b = bytes == typemin(Int64) ? typemax(Int64) : abs(bytes)
    if b < 1000
        return @sprintf("%s%d B", neg, b)
    end
    for i in 1:5
        if b < 999950
            return @sprintf("%s%.1f %sB", neg, b / 1e3, units[i])
        end
        b /= 1000
    end
    return @sprintf("%s%.1f EB", neg, b / 1e3)
end

function humanReadableByteCountBin(bytes::Int64)
    b = bytes == typemin(Int64) ? typemax(Int64) : abs(bytes)
    if b < 1024
        return @sprintf("%d B", b)
    end
    sf = 40
    for i in 1:5
        if b < 0xfffcccccccccccc >> sf
            if i == 5
                b >>= 10
            end
            return @sprintf("%.1f %siB", b / bin_div[i], units[i])
        end
        sf -= 10
    end
    b = (b >> 20) / 0x1p40
    return @sprintf("%.1f EiB", b)
end

## End NMSU content
function interpret_args(sa::SimArgs)
    Cαs = get_pdb_Cα(sa.pdb_filepath)
    simstate = generate_anm_simstate(Cαs, sa.bond_cutoff, sa.bond_modifier)
    nrecorded_elem = length(simstate.positions) * sa.nframes

    println("== Literal arguments:")
    println("  PDB File: $(sa.pdb_filepath)")
    @printf("  Bond Cutoff: %.3f\n", sa.bond_cutoff)
    @printf("  Bond Strength Multiplier: %.3f\n", sa.bond_modifier)
    println("  Frames to Record: $(commas(sa.nframes))")
    println("  Record every $(commas(sa.nskip)) frames")
    println("  Simulation Parameters:")
    @printf("    Temperature: %.3f\n", sa.params.T)
    @printf("    Timestep: %.3f\n", sa.params.δt)
    @printf("    Damping: %.3f\n", sa.params.γ)
    @printf("    Spring Strength: %.3f\n", sa.params.k)
    println("")
    println("== Additional Data:")
    @printf("  Simulation c1: %.4f\n", calc_c1(sa.params))
    @printf("  Simulation c2: %.4f\n", calc_c2(sa.params))
    @printf("  Number of atoms: %d\n", length(Cαs))
    @printf("  Number of bonds: %d\n", sum(simstate.coeffs .!= 0.0))
    println("  Output size: $(humanReadableByteCountBin(sizeof(Float64) * nrecorded_elem))")
    println("  Total timesteps: $(commas(sa.nframes * sa.nskip))")
end

if abspath(PROGRAM_FILE) == @__FILE__
    if length(ARGS) == 0
        println("Displays info about an argset.")
        println("Usage: <pdbfile> [extra args]")
        println("See run_anm.jl for details about valid extra arguments.")
        exit(1)
    else
        # Add dummy arguments for HDF5 file + datapaths
        fakeargs = vcat(["";""], ARGS)
        simargs = parse_args(fakeargs)
        interpret_args(simargs)
    end
end