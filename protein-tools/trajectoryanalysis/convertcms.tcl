# Usage: vmd -dispdev text -e convertcms.tcl -args <cmsfile> <pdboutname>
# Places dimensional info in file dimensions.out
# Used to normalize desmonddata

set infilename [lindex $argv 0]
set outfilename [lindex $argv 1]
mol new $infilename

set atms [atomselect top "all"]

$atms writepdb $outfilename
exit