# Compute the contact order of a given protein

include("PeptideUtils.jl")
include("ContactOrder.jl")
using .ContactOrder
using .PeptideUtils
using BioStructures
using Printf

if length(ARGS) < 2
    @printf("Usage: %s <PDB_ID> <cutoff>\n", PROGRAM_FILE)
    exit(1)
end

rawname = ARGS[1]
cutoff = parse(Float64, ARGS[2])

(name, span) = parse_pdb_subrange(rawname)

pdbstruct = retrievepdb(name)

residues = collectresidues(pdbstruct, standardselector)

if span === nothing
    targetres = residues
else
    (minres, maxres) = span
    targetres = filter(residues) do x
        n = resnumber(x)
        n >= minres && n <= maxres
    end
end

@printf("%s: %f\n", rawname, contact_order(targetres, cutoff))
