"""A library of useful helper functions when working with protein structures in Julia"""
module PeptideUtils

export AMINO_MASS, get_pdb_calpha, parse_pdb_subrange

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

span is used for taking subspans of the calpha trace to match protein fragments
published in BroomDB (Broom et. al. 2015).
"""
function get_pdb_calpha(struc; span=nothing::Union{Nothing, Tuple{Int,Int}})
    residues = Bio.Structure.collectresidues(struc)

    positions = zeros(3, 0)
    masses = Vector{Float64}()
    for res in residues
        # We cannot simply use indices to identify residues because the first
        # residue in a PDB file might not be 1 (e.g. in 1e41, the first residue
        # is residue 89. Here, we check to see if the residue is in range
        # of the span we want--if not, we skip it pre-emptively.
        if span !== nothing
            (b, e) = span
            if resnumber(res) < b || resnumber(res) > e
                continue
            end
        end

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

"""Parse the input name, possibly splitting it for subselection

Broom's DB has contiguous protein subranges in it, e.g. "1EN2, residues 15-37".
These are encoded in our case names as part of the name, e.g. 1en2-15-37, using
either dashes (-) or underscores (_). This function parses such a name out to
the PDB name + (start, end) if it exists, or pdbname + nothing if the input
was a raw 4-char pdb name.
"""
function parse_pdb_subrange(name)                                                           
    # Base PDBs have no characters                                                          
    if length(name) == 4                                                                    
        return (name, nothing)                                                              
    end                                                                                     
                                                                                            
    # We have a subset range specified in the name. Split on the allowed chars              
    toks = split(name, ['-','_'])                                                           
    if length(toks) != 3                                                                    
        println("PDB name is not 4 chars, but has wrong form for subrange!")                
        exit(1)                                                                             
    end                                                                                     
                                                                                            
    (pdbname, start, fini) = toks                                                              
    s = parse(Int, start)                                                                   
    e = parse(Int, fini)                                                                    
    (pdbname, (s,e))                                                                           
end      

end # End module PeptideUtils
