using Distributed

@everywhere include("simulate_anm.jl")

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