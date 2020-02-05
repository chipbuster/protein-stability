module ContactOrder
using LinearAlgebra
using BioStructures

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

Input: A Vector of Bio.AbstractResidue that represents the protein. This function
selects the heavy atoms and computes the minimum squared distance between them.
"""
function find_contacts(aminos::Vector{AbstractResidue}, cutoff::Float64)::AbstractVector{Contact}
    contacts = Vector{Contact}()
    N = length(aminos)
    for i in 1:N
        for j in i + 1:N
            aa_i = aminos[i]
            aa_j = aminos[j]
            # Note: important to use heavyatomselector instead of e.g. calphaselector
            for a_i in collectatoms(aa_i, heavyatomselector)
                for a_j in collectatoms(aa_j, heavyatomselector)
                    d = sqdistance(a_i, a_j)
                    if d < cutoff^2
                        c = make_contact(d, resnumber(aa_i), resnumber(aa_j))
                        push!(contacts, c)
                    end
                end
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
    return sum(bbdists) / L
end

"""Compute the contact order given the residues"""
function contact_order(residues, cutoff::Float64, absolute = true::Bool)
    nresidues = length(residues)
    contacts = find_contacts(residues, cutoff)
    rco = contact_order_from_contacts(contacts, nresidues)

    if absolute
        return rco / nresidues    # ACO
    else
        return rco
    end
end

end    # End module ContactOrder
