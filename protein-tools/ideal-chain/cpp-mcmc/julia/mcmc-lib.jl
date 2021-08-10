using StaticArrays
using LinearAlgebra
using Logging
using HDF5

struct SimMetadata
    accepted::Int
    rejected::Int
    param1::Float64
    param2::Float64
    param3::Float64
    numAngles::Int
    numSteps::Int
    skips::Int
    gaussWidth::Float64
end

accept_frac(simmetadata) = simmetadata.accepted / (simmetadata.accepted + simmetadata.rejected)

function load_sim_angles(filename)
    h5open(filename) do f
        read(f, "angles")
    end
end

function load_sim_metadata(filename)
    h5open(filename) do f
        dset = f["angles"]

        accepted = read_attribute(dset, "accepted")
        rejected = read_attribute(dset, "rejected")
        p1 = read_attribute(dset, "param1")
        p2 = read_attribute(dset, "param2")
        p3 = read_attribute(dset, "param3")
        numAngles = read_attribute(dset, "numAngles")
        numSteps = read_attribute(dset, "numSteps")
        skips = read_attribute(dset, "skips")
        gaussWidth = read_attribute(dset, "gaussWidth")

        SimMetadata(accepted, rejected, p1, p2, p3, numAngles, numSteps, skips, gaussWidth)
    end
end