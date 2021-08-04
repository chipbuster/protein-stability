HDF5 Chain Simulations
======================

A set of chain simulations based on HDF5-backed storage. This
allows us to run simulations without storing the intermediate
results in memory, letting us run truly monstrous-length chain
simulations without going OOM.

As a downside, HDF5.jl is not yet thread-safe and the HDF5 code
is more complicated than its array + Serialize.jl based counterpart,
and is also not yet thread-safe, so if the simulation is short 
(<1M timesteps recorded), the Serialize.jl-based code should be
preferred.

The code from 2019/2020 is in `old_simulation`. Updated code is in `simulation`.