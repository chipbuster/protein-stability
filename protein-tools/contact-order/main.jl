# Compute the contact order of a given protein

include("../juliautils/PeptideUtils.jl")
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
(pos, _) = get_pdb_calpha(pdbstruct; span=span)
println(pos)
@printf("Contact order of %s is %f\n", rawname, contact_order(pos, cutoff))
