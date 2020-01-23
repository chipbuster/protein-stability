Description of Data Representation/Transformations
==================================================

# Summary

**IMPORTANT NOTE: Because of how HDF5.jl stores data, the data seen in Python
and Julia will be transpositions of each other!** This must be taken into account
when writing analysis code.

There are three general stages that a dataset passes through in the Avinery
procedure:
   1. Input
   2. Parameterized
   3. Binned

The stage 3 data is compressed to get the compression ratio. In addition,
each representation has associated with it some units and some additional
ancilliary information about how this data was generated.

The core algorithms are mostly implemented in Julia, for improved speed and
memory usage. However, some parts of this pipeline (notably, the actual
compression and the reading from MD traces) rely on libraries that do not
exist in the Julia ecosystem and thus must be done in Python.

The `core` source code contains the code to construct/manipulate each representation.

<--! More file overviews here -->

# Data Representations

### Input

This is as close to the raw input data as we can achieve without just using
the raw data. It consists of the representation of the system as output by
the simulation program. For example, in an MD trace, it is an array of
(3,N,T) floats, where N is the number of Ca atoms and T is the number of
timesteps. It is the same for a Langevin simulation an elastic network model.
In principle, if a simulation worked on Ramachandran angles, it could be a
(2,N,T) array of floats for phi-psi angles, though this is unlikely.

The input data might also contain some metadata describing how the simulation
was run: for example, the parameters/program for an MD trace, or the
temperature and damping parameters for a Langevin simulation.

Examples of data form:
   - R3 Cartesian trace (3,N,T)
   - R2 Cartesian trace (2,N,T)
   - R1 Cartesian trace (1,N,T)
   - Phi-Psi trace      (2,N,T)
   - Other              (??)

### Parameterized

This is a data format that is ready for binning. Ideally, it should be
completely translation and rotation invariant, while still capturing all the
important information in the simulation. The metadata for this format should
contain unit and bound info (for example, limits, mapping functions, and
units)

Examples of forms:
  - Phi-Psi angles (degrees)
  - Phi-Psi angles (radians)
  - Angle from point on offset-axis
  - Remapped points (e.g. via cotan inverse)

### Binned

This form is ready to compress. It is a 1D sequence of integer datatypes
corresponding to bins. Metadata should include the binning and linearization
strategies (e.g. `uniform [-pi,pi]` and `peano-curve`).

# Common Keys

### General Keys
- `temperature`: the temperature value of the simulation
- `dt`: the size of each timestep (context-sensitive units)
- `pdbid`: the PDB structure (if applicable) of the unit

### Input Data
- `source`: Describes the source of the input
   + `md:<name>`: Describes a molecular dynamics simulation of a particular type
   + `langevin`: A simple langevin simulation
- `skip`: If applicable, the number of skipped iterations from raw

### Parameterization Data
- `maxval`: the maximum positive value of parameterized data. Data should be in
            range `[-maxval, maxval]`
- `type`: the category of parameterization being used. Current options:
      + `dihedral`: dihedral angles of Ca traces
      + `ramachandran`: phi-psi angles of a protein simulation

### Binned Data
- `nbins`: the number of bins used
- `disctype`: the discretezation type
   + `linear`: linearly separated bins (the only valid value right now)
