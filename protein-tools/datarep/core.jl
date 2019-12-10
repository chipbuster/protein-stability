using HDF5;

abstract type AbstractSimData end;

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
    dat  = d_open[dp * "/parameterized"]
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
    dat  = d_open(dp * "/binned")
    ParameterizedData(dat, fp, dp, file)
end

