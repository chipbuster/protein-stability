module SimData

using HDF5;

abstract type AbstractSimData end;

export AbstractSimData, InputData, ParameterizedData, BinnedData
export create_binneddata, create_inputdata, create_parameterizeddata,
       find_all_dataset

"""A specifier for an HDF5 dataset.

filepath specifies the on-disk path to the HDF5 file
datapath specifies the group-path within the HDF5 blob to the desired dataset
"""
struct HDF5LocIdent
    filepath::String
    datapath::String
end

struct InputData <: AbstractSimData
    data::HDF5Dataset
    filepath::String
    datapath::String
    h5File::HDF5File
end

InputData(fp::String, dp::String) = begin
    file = h5open(fp, "cw")
    dat  = file[dp * "/inputdata"]
    InputData(dat, fp, dp, file)
end

struct ParameterizedData <: AbstractSimData 
    data::HDF5Dataset
    filepath::String
    datapath::String
    h5File::HDF5File
end

ParameterizedData(fp::String, dp::String) = begin
    file = h5open(fp, "cw")
    dat  = file[dp * "/parameterized"]
    ParameterizedData(dat, fp, dp, file)
end


struct BinnedData <: AbstractSimData 
    data::HDF5Dataset
    filepath::String
    datapath::String
    h5File::HDF5File
end

BinnedData(fp::String, dp::String) = begin
    file = h5open(fp, "cw")
    dat  = file[dp * "/binned"]
    ParameterizedData(dat, fp, dp, file)
end

function close(data::T) where T <: AbstractSimData
    close(data.h5File)
end

"""Create a dataset with the specified data, backed by an HDF5 file."""
function create_inputdata(fp::String, dp::String, data, attr)
    file = h5open(fp, "cw")
    file[dp * "/inputdata"] = data
    fdata = file[dp * "/inputdata"]
    print(typeof(fdata))
    for (k, v) in attr
        attrs(fdata)[k] = v
    end
    InputData(fdata, fp, dp, file)
end

function create_parameterizeddata(fp::String, dp::String, data, attr)
    file = h5open(fp, "cw")
    file[dp * "/parameterized"] = data
    fdata = file[dp * "/parameterized"]
    print(typeof(fdata))
    for (k, v) in attr
        attrs(fdata)[k] = v
    end
    ParameterizedData(fdata, fp, dp, file)
end

function create_binneddata(fp::String, dp::String, data, attr)
    file = h5open(fp, "cw")
    file[dp * "/binned"] = data
    fdata = file[dp * "/binned"]
    print(typeof(fdata))
    for (k, v) in attr
        attrs(fdata)[k] = v
    end
    BinnedData(fdata, fp, dp, file)
end

"""Find the name of all datasets below a given path in an HDF5 file"""
function find_all_dataset(file::HDF5File, start="/"::String)::Vector{String}
    datasets = Vector{String}()
    for x in file[start]
        if typeof(x) == HDF5Group
            append!(datasets, find_all_dataset(file,name(x)))
        elseif typeof(x) == HDF5Dataset
            push!(datasets,name(x))
        else
            continue   # Not of interest to us
        end
    end
    return datasets
end

end # End module SimData