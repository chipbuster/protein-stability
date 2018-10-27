# Remember that this language is one-indexed you damn twit!
using Printf

# Control whether we are doing symmetric or antisymmetric Ising models
const symmetric = true
const J = 0.5 * (symmetric ? -1.0 : 1.0)  # Coupling coefficient
const T = 30.0                             # Temperature
const kB = 1.0                              # Boltzmann's constant

# Constants to define spin-up or spin-down
const up = 1
const dn = -1

"""H(grid)

   Compute the energy of an Ising grid"""
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

"""dH(grid, index)

   Compute the energy change for flipping an Ising grid at an index"""
function dH(grid::Matrix{Int}, index::CartesianIndex{2})::Float64
    gridX,gridY = size(grid)

    nbrs = listNeighbors(index[1],index[2],gridX,gridY)
    dE = 0.0

    for (ni,nj) in nbrs
        dE += 2 * grid[index] * grid[ni,nj]
    end

    return dE
end


function listNeighbors(i::Int, j::Int, gridX::Int, gridY::Int)::Vector{Tuple{Int,Int}}
    wrapVal(x, targ) = x > targ ? 1 : (x < 1 ? targ : x)
    wX(x) = wrapVal(x, gridX)
    wY(y) = wrapVal(y, gridY)

    udneighbors = [ (wX(x),j) for x in (i+1,i-1) ]
    lrneighbors = [ (i,wY(y)) for y in (j+1,j-1) ]
    return [udneighbors; lrneighbors]
end

"""Generate an nxn grid of spins.

If init is false, all spins are spin-up. If it is true, spins are randomly
initialized (50-50)."""
function genSpinGrid(n::Int, init=true)::Matrix{Int}
    init ? rand([up,dn],(n,n)) : ones(n,n)
end

flip(spin::Int) = spin == -1 ? 1 : -1

"""Run the Ising simulation for n steps, generating an iterator over states."""
function runIsingMC(inp::Matrix{Int},steps::Int)::Channel{Matrix{Int}}
    ch = Channel{Matrix{Int}}(steps)
    let (n1,n2) = size(inp)
        @assert n1 == n2 "Input to runIsing is not a square matrix!"
    end
    for _ = 1:steps
        # Randomly choose a site to flip
        randomPair = rem.(rand(UInt,2),size(inp)[1]) .+ 1   # Generate random numbers
        flipSite = CartesianIndex(tuple(randomPair...)) # Convert list into index
        siteOrig = inp[flipSite]

        dE = dH(inp,flipSite)
        E = H(inp)
        @printf("%f, %f\n", E, 1 - exp(-dE / (kB * T)))
#        show(flipSite); print("    "); show(EOld); println()

        r = rand(Float64)
        if dE > 0.0 && r >= exp(-dE / (kB * T))       # Reject transition
            put!(ch, copy(inp))
        else
            inp[flipSite] = flip(inp[flipSite])
            put!(ch, copy(inp))
        end
    end

    close(ch)
    return ch
end

function runIsingWolff(inp::Matrix{Int}, steps::Int)

end