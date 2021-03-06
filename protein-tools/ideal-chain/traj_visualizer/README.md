Network Visualizer
==================

This is a network visualizer for the mass-spring networks generated by the
Julia code. It assumes a *very* specific format in the HDF5 dataset that you
feed it, but should otherwise generally just work™.

## Build

Standard CMake build procedure

```
mkdir build
cd build
cmake ../src -DCMAKE_BUILD_TYPE=Release -G Ninja
ninja
```

This creates the binary `build/bin/netviz`

## Usage

```
  path/to/netviz <path-to-hdf5-file> <datapath-within-hdf5-file>
```

For example, `netviz /data/rings.hdf5 /chains/10/data`.

The visualization code will attempt to guess whether the connectivity is chain
or ring-like based on the datapath argument (this is terrible hardcoded behavior
and should be changed at some point).

## Components

This is forked from [Josh Vekhter's starter kit](https://github.com/the13fools/geometry-processing-starter-kit)
which is in turn based on the following projects sorta smashed together in a
way that I don't fully understand:

- [Etienne's physics starter code](https://github.com/evouga/libigl-example-physics-project)
- [Nicholas Sharp's Polyscope](http://polyscope.run/)
- [imgui](https://github.com/ocornut/imgui)
- [libigl](https://libigl.github.io/)
