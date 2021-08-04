
if length(ARGS) < 1
    println("Usage: $(PROGRAM_FILE) <output file> <additional args>")
    println("Additional args are in the format key:value")
    println("Addtional args are:")
    println("  natom: int      number of atoms in simulation  (default: 10)")
    println("  nstep: int      number of recorded timesteps  (default: 10,000)")
    println("  nskip: int      number of frames simulated per recorded timestep (default: 1)")
    println("  eedist: float   end-to-end distance of chain  (default: 1.0)")
    println("  rblen: float    bondlength for non end-to-end bonds  (default: 1.0)")
    println("  c1: float       The c1 parameter  (default: 0.0)")
    println("  c2: float       The c2 parameter  (default: 0.0)")
    println("  iscale: float   The initial scaling of the system (default: 1.0)")
    exit(1)
end

include("idealchain.jl")
# Necessary params
#   - output location
#   - num atoms
#   - num sim steps
#   - skip (steps between recorded frames)
#   - eedist
#   - rest bondlength
#   - parameter c1
#   - parameter c2

filename = ARGS[1]

natom = 10
nstep = 10_000
nskip = 1
eedist = 1.0
rblen = 1.0
c1 = 0.0
c2 = 0.0
iscale = 1.0

for addon in ARGS[2:end]
    (key, value) = split(addon,':')
    if key == "natom"
        global natom
        natom = parse(Int, value)
    elseif key == "nstep"
        global nstep
        nstep = parse(Int, value)
    elseif key == "nskip"
        global nskip
        nskip = parse(Int, value)
    elseif key == "eedist"
        global eedist
        eedist = parse(Float64, value)
    elseif key == "rblen"
        global rblen
        rblen = parse(Float64, value)
    elseif key == "c1"
        global c1
        c1 = parse(Float64, value)
    elseif key == "c2"
        global c2
        c2 = parse(Float64, value)
    elseif key == "iscale"
        global iscale
        iscale = parse(Float64, value)
    else
        println("Key $(key) was not recognized as a valid option")
    end
end

params = SimParameters(rblen, c1, c2, eedist, nskip, nstep)

println("Parameters are $(params)")

run_sim(filename, natom, params, iscale)
