package require autoionize
package require autopsf
package require mutator

# Usage: vmd -dispdev text -e automutate.tcl -args <pdbid> <mutation id string> <target res num> <target residue 3-letter>
# Places dimensional info in file dimensions.out

set pdbname [lindex $argv 0]
set mutid   [lindex $argv 1]
set targnum [lindex $argv 2]
set targres [lindex $argv 3]
mol new $pdbname.pdb

# Script loads a molecule in, applies transformations:
#      - Generate PSF
#      - Solvate
#      - Mutate
#      - Ionize
#      - Output cell parameters

## Solvation and PSF generation
# -protein: explicitly select protein
# -include water: explicity select waters (all others ignored)
# -solvate: default solvation option: box with 12A of water buffer
# -top: use topology files provided in list (charmm36 topologies)
autopsf -mol top -protein -include water -solvate -top [list\
           /opt/proteins/ffparams/c36_18/top_all36_carb.rtf\
           /opt/proteins/ffparams/c36_18/top_all36_cgenff.rtf\
           /opt/proteins/ffparams/c36_18/top_all36_lipid.rtf\
           /opt/proteins/ffparams/c36_18/top_all36_na.rtf\
           /opt/proteins/ffparams/c36_18/top_all36_prot.rtf\
           /opt/proteins/ffparams/c36_18/toppar_water_ions.str\
           ]

## Mutate and generate files
mutator -psf ${pdbname}_autopsf.psf -pdb ${pdbname}_autopsf.pdb -o MUTATED -resid $targnum -mut $targres -FEP $pdbname-fep

## Before continuting, we need a PDB file for future steps to use.
file copy $pdbname-fep.fep $pdbname-fep.pdb

## Ionize to neutralize
# -sc 0.15: neutralize and set salt concentration to 0.15M
# -o ${pdbname}_prepared: set prefix to prepared
autoionize -psf $pdbname-fep.fep.psf -pdb $pdbname-fep.pdb -sc 0.15 

## Move all files to some sane default names
file mkdir prepared
file copy $pdbname-fep.fep prepared/$pdbname-$mutid.fep
file copy ionized.pdb prepared/$pdbname-$mutid.pdb
file copy ionized.psf prepared/$pdbname-$mutid.psf

## Print coordinate data to file
set outfile [open "dimensions.out" w]

set sel [atomselect top "all"]

puts -nonewline $outfile "DIMENSIONS MINMAX: "
puts $outfile [measure minmax $sel]
puts -nonewline $outfile "DIMENSIONS CENTER: "
puts $outfile [measure center $sel]
close $outfile

exit
