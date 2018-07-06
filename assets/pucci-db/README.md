Pucci et. at. 2016 Thermostability Database
============================================

A thermodynamic database of curated mutations for stability testing.

# Extraction

Data was extracted from Supplementary Info of referenced paper (at end) using
[Tabula](https://tabula.technology/). This resulted in issues with column
alignment and dash characters ("-") being turned into uppercase "U", and spaces
being replaced with slashes (" " --> "/").

# Cleaning

Dashes in numerical fields were changed back from "U" to "-" with the following
sed script:

```
sed 's/,U\([0-9]*\)/,\-\1/g' tabula-Pucci\ et\ al.\ 2016\ -\ 036301-1\(1\).csv > Pucci2016-r1.csv
```

Column alignment was fixed with:

```
sed 's/^\"\"\,//g' Pucci2016-r1.csv > Pucci2016-r2.csv
```

Space-slash issues were fixed with:

```
tr '/' ' ' <Pucci2016-r2.csv >Pucci2016-r3.csv
```

This leaves a few issues where dashes in organism names are replaced by capital
U--however, I think this is unlikely to be significant for this work.

# Data Download

Downloads were accomplished by using `javaws` on the files resulting from the
[PDB mass-download page](https://www.rcsb.org/pages/download_features).

`pdb+sf` contains PDB files with structure factor info. `pdb` contains no
structure factors.

# Publication

https://www.biorxiv.org/content/early/2016/01/10/036301

Pucci, Fabrizio, Raphaël Bourgeas, and Marianne Rooman. 2016. “High-Quality
Thermodynamic Data on the Stability Changes of Proteins upon Single-Site
Mutations.” bioRxiv. https://doi.org/10.1101/036301.
