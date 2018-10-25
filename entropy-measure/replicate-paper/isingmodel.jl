# Remember that this language is one-indexed you damn twit!

# Control whether we are doing symmetric or antisymmetric Ising models
const symmetric = true
const J = 0.01 * (symmetric ? 1.0 : -1.0)

# Constants to define spin-up or spin-down
const up = 1
const dn = -1

function H(grid::Matrix{Int})::Float64
    gridX,gridY = size(grid)
    energies = zeros(gridX, gridY)
    # @inbounds   # Don't enable this quite yet
    for j=1:gridY
        for i=1:gridX
            nbrs = listNeighbors(i,j,gridX,gridY)
            for (ni,nj) in nbrs
                energies[i,j] += J * grid[ni,nj] * grid[i,j]
            end
        end
    end

    return sum(energies)
end

function listNeighbors(i::Int, j::Int, gridX::Int, gridY::Int)::Vector{Tuple{Int,Int}}
    #= Eliminate for now, assume periodic boundary conditions
    if i - 1 >= 1 && j - 1 >= 1
        push!(retval, (i-1,j-1))
    end
    if i - 1 >= 1 && j + 1 <= gridY
        push!(retval, (i-1,j+1))
    end
    if i + 1 <= gridX && j - 1 >= 1
        push!(retval, (i+1,j-1))
    end
    if i + 1 <= gridX && j + 1 <= gridY
        push!(retval, (i+1,j+1))
    end
    =#
    wrapVal(x, targ) = x > targ ? 1 : (x < 1 ? targ : x)
    wX(x) = wrapVal(x, gridX)
    wY(y) = wrapVal(y, gridY)

    udneighbors = [ (wX(x),j) for x in (i+1,i-1) ]
    lrneighbors = [ (i,wY(y)) for y in (j+1,j-1) ]
    return [udneighbors; lrneighbors]
end

