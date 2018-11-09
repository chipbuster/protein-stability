"""
Pack a set of Int8s, if we're guaranteed that there's enough of them
to make a full packing
"""

function packInt8Full(arr::Vector{Int8}, packN)::Int8
    if packN == 1
        return arr[1]
    elseif packN == 2
        return (arr[1] << 4) ⊻ arr[2]
    elseif packN == 4
        return (arr[1] << 6) ⊻ (arr[2] << 4) ⊻ (arr[3] << 2) ⊻ arr[4]
    else
        error("Array input is of wrong size")
    end
end

function packInt8(arr::Vector{Int8}, packN)::Vector{Int8}
    pInd = 0    # Index where we pack from
    endFullSize = div(length(arr),packN) * packN

    while pInd < endFullSize
        packInt8Full(arr[(pInd+1) : (pInd + packN)], packN)
        pInd += packN
    end
end