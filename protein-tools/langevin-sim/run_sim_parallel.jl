using Distributed

@everywhere include("simulate_anm.jl")

# Currently does not include any code for subrange selection
# or temperature customization

pdbnames = get_pdbnames(ARGS[1])
numpdb = length(pdbnames)
@printf("I have %d pdbs: ", numpdb)
for name in pdbnames
    print(name*", ")
end
println("")
@sync @distributed for name in pdbnames
    run_sim(name)
end
