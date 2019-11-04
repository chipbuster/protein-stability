module ContactOrder

using LinearAlgebra

export contact_order

"""An aggregate to capture a contact between two Ca atoms in a protein"""
struct Contact
    ident::Tuple{Int,Int}  # The indices of the contacting proteins
    dist::Float64          # The euclidian distance between the contact endpoints
    bbdist::Int            # The number of residues separating the two contacts
end

"""Utility function to generate Contact at given distance/index pair"""
function make_contact(dist, i, j)
    bbdist = abs(j - i)
    return Contact((i, j), dist, bbdist)
end

"""Find the set of contacts in a protein Ca trace

Input: a 3xN array corresponding to C-alpha atom positions in a PDB. The atoms
should be ordered in backbone ordering in default PDB order (N-terminus to 
C-terminus), so that backbone distance can be calculated correctly.
"""
function find_contacts(positions::AbstractArray{Float64,2}, cutoff::Float64)::AbstractVector{Contact}
    contacts = Vector{Contact}()

    (dim, N) = size(positions)
    for i = 1:N
        for j = i + 1:N
            dist = norm(positions[:,i] - positions[:,j])
            if dist >= cutoff
                c = make_contact(dist, i, j)
                push!(contacts, c)
            end
        end
    end
    return contacts
end

"""Compute the contact order given a list of contacts and a protein size

Computed according to Eqn (1) in Plaxo, Simons, Baker, J Mol Bio (1998) 277
"""
function contact_order_from_contacts(contacts::Vector{Contact}, nresidues::Int)
    # Variable names from paper or see (https://en.wikipedia.org/wiki/Contact_order)
    L = nresidues
    N = length(contacts)

    if N == 0
        println("No contacts found!")
        return 0.0
    end

    bbdists = [ c.bbdist for c in contacts ]
    println(contacts)
    return sum(bbdists) / (N * L)
end

"""Compute the contact order given the Ca positions"""
function contact_order(positions::AbstractArray{Float64,2}, cutoff::Float64)
    (_, nresidues) = size(positions)
    contacts = find_contacts(positions, cutoff)
    return contact_order_from_contacts(contacts, nresidues)
end

end    # End module ContactOrder
