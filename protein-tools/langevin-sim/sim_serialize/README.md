Ideal Spring Simulation Code
============================

Simulation code for a 2D ideal chain system with springs.

The simulation physics are controlled by two dimensionless parameters: $c_1$ and
$c_2$. The former controls the strength of the restorative spring forces, and
the latter controls the strength of the random thermal motion of the atoms.

Other options which control the simulation setup (and their defaults) can be 
found by reading `run_idealchain.jl`. These mostly have to do with setting up
the physical configuration of the chains and how long the simulation runs for
and how it is recorded.

This code has optimizations in the state updates that avoids unnecessary
allocations, resulting in a drastic speedup.
