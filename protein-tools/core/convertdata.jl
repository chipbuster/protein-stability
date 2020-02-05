## Routines to convert between different forms of data.
include(joinpath(@__DIR__, "simdata.jl"))

using .SimData;
using HDF5;
using LinearAlgebra;
using Discretizers;
using Test;

"""Generate a new HDF5 file with the same inputdata, but only using every Nth frame"""
function create_dataskip_file(input_file::String, take_every::Int, new_file = ""::String)
    infile = h5open(input_file, "r")
    if length(new_file) == 0
        new_basename = split(basename(input_file), '.')[1] * "_skip.hdf5"
        new_file  = joinpath(dirname(input_file), new_basename)
    end
    outfile = h5open(new_file, "w")
    for datapath in find_all_dataset(infile)
        outfile[datapath] = infile[datapath][:,:,1:take_every:end]
        indata  = infile[datapath]
        outdata = outfile[datapath]
        inattr  = attrs(indata)
        outattr = attrs(outdata)
        
        # Copy over attributes
        for a in names(inattr)
            outattr[a] = read(inattr[a])
        end
        # Compute skip based on existing skips: if curskip is n and new is m,
        # we are skipping a total of m*n frames.
        if exists(outattr, "skip")
            oldskip = read(inattr["skip"])
            new_skip = oldskip * take_every
            write(outattr["skip"], new_skip)
        else
            outattr["skip"] = take_every
        end
    end
    close(outfile)
end

"""Convert an XYZ trace to dihedral angles."""
function xyz_to_dihedrals(input_data::InputData)::ParameterizedData
    # Set up the output data for to_dihedral to plug stuff into
    datapath = input_data.datapath * "/parameterized"

    (_, N, ts) = size(input_data.data)
    if datapath in SimData.find_all_dataset(input_data.h5File)
        paramdata = input_data.h5File[datapath]
        o_delete(paramdata)
    end

    paramdata = d_create(input_data.h5File, datapath, datatype(Float64), dataspace(N - 3, ts))

    attrs(paramdata)["maxval"] = convert(Float64, pi)  # This may need to be changed later
    attrs(paramdata)["type"] = "dihedral"

    bond_dihedral_trace(input_data.data, paramdata)
    flush(input_data.h5File)

    ParameterizedData(paramdata, input_data.filepath, input_data.datapath,
    input_data.h5File)
end

"""Bin a dihedral/phi-psi dataset into dihedral angles"""
function bin_dihedrals(input_data::ParameterizedData, nbins::Integer)::BinnedData
    datapath = input_data.datapath * "/binned"
    maxv = read(attrs(input_data.data)["maxval"])

    bounds = collect(range(-maxv, maxv, length = nbins + 1))
    @assert length(bounds) == nbins+1 "Incorrect bounds length from range/collect! Potential Julia bug?"
    (N, ts) = size(input_data.data)
    if datapath in SimData.find_all_dataset(input_data.h5File)
        binneddata = input_data.h5File[datapath]
        o_delete(binneddata)
    end

    binneddata = d_create(input_data.h5File, datapath, datatype(Int), dataspace(N, ts))

    attrs(binneddata)["nbins"] = nbins
    attrs(binneddata)["disctype"] = "linear"

    lindisc = LinearDiscretizer(bounds);
    binneddata[:,:] = encode(lindisc, input_data.data[:,:])
    flush(input_data.h5File)

    BinnedData(binneddata, input_data.filepath, input_data.datapath, input_data.h5File)
end
