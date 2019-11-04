"""A library of useful helper functions when working with protein structures in Julia"""
module PeptideUtils

export AMINO_MASS, get_pdb_calpha

using BioStructures
using Bio
using Printf

const BS = BioStructures

"""A lookup table used to determine the mass of a given amino acid. Numbers are
taken from http://www.matrixscience.com/help/aa_help.html, units are in amu"""
const AMINO_MASS = Dict{String,Float64}("ALA" => 71.0779,
  "ARG" => 156.1857,
  "ASN" => 114.1026,
  "ASP" => 115.0874,
  "ASX" => 114.595,  # For ASX, average ASN + ASP (since we don't know which)
  "CYS" => 103.1429,
  "GLU" => 129.114,
  "GLN" => 128.1292,
  "GLX" => 128.6216, # For GLX, average GLN + GLU (since we don't know which)
  "GLY" => 57.0513,
  "HIS" => 137.1393,
  "ILE" => 113.1576,
  "LEU" => 113.1576,
  "LYS" => 128.1723,
  "MET" => 131.1961,
  "PHE" => 147.1739,
  "PRO" => 97.1152,
  "SER" => 87.0773,
  "THR" => 101.1039,
  "SEC" => 150.0379,
  "TRP" => 186.2099,
  "TYR" => 163.1733,
  "VAL" => 99.1311)

"""Read protein structure and return list of calpha atoms and masses of residues.

Returns a tuple (positions,masses), where positions is a 3xN matrix with positions
of calpha atoms in the columns, and masses is an N-vector with the mass of residue
i at the i-th index.
"""
function get_pdb_calpha(struc)
    residues = Bio.Structure.collectresidues(struc)

    # I'm not sure what the cleanest way to build up a matrix in Julia is...for
    # now I'll just make an empty matrix and hcat to it.
    positions = zeros(3, 0)
    masses = Vector{Float64}()
    for res in residues
        c_alpha_set = BS.collectatoms(res, BS.calphaselector)
        if isempty(c_alpha_set)
            # println(BS.resname(res))  # If we're worried about the ignored res
            continue # Not really a residue we care about
        end

        @assert(length(c_alpha_set) == 1, "A single residue has more than one c-alpha atom!")
        ca_atom = c_alpha_set[1]    # The only atom in the set
        atom_pos = BS.coords(ca_atom)
        mass = get(AMINO_MASS, BS.resname(res), 0)
        if mass == 0
            @printf("WARNING: Unknown residue: %s\n", BS.resname(res))
            @printf("Setting mass to a default value\n")
            mass = 100.0
        end

        positions = hcat(positions, atom_pos)
        push!(masses, mass)
    end
    (positions, masses)
end

end # end module PeptideUtils