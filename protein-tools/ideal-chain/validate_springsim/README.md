Spring Simulation Autovalidation
===============================

Automated validation routines for spring-based simulations. Also contains
utlities for plotting said results.

Currently designed for files named output_n_R.serial. As long as filename
contains serialized traces (2, natom, nsteps), changes in filename can be
accomodated by changing the filename_to_label function. If a change in file
format occurs, we will need to modify the loading as well.

Example executions:

JULIA_NUM_THREADS=8 julia validate_sims.jl always /tmp/plotdir /Experiments/data/07/*.serial
JULIA_NUM_THREADS=8 julia validate_sims.jl never /Experiments/data/07/*.serial
