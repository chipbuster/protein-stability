import sys, os, glob
import shutil
import tempfile
import subprocess
import zstandard

mydir = os.path.dirname(os.path.realpath(__file__))

def normalize_topofile(topofile):
    """ Normalize a topology file into PDB format.

    Use a VMD TCL script to normalize a topology file into a standard format.
    Take in the path to the file to be normalize. Return the path to the new 
    normalized file, creating + normalizing it as a side-effect."""

    outfile = os.path.join("/tmp",os.path.basename(topofile[:-3] + "pdb"))
    #print("Converting CMS to PDB with VMD, please be patient...", end="")
    convertscript = os.path.join(mydir,"convertcms.tcl")

    exitstatus = subprocess.run(["vmd","-dispdev","text","-e",convertscript,
                    "-args",topofile,outfile])
    exitstatus.check_returncode()  #If conversion was unsuccessful, abort prog
    return outfile

def normalize_desmond_data(source_dir, target, compress=False):
    """Collect Desmond data into a standardized format

    Given a path to a Desmond output directory in `source_dir`, we try to
    normalize its trajectory data. This normalized data consists of topology
    information in a PDB file, and trajectory data that can be read by VMD
    (usually via a `clickme.dtr` file)

    We can output one of two things:

      - If `compress` is false, treat `target` as a directory and output there
      - If `compress` is true, treat `target` as an archive name and output there.
    """

    pdb_pattern = os.path.join(source_dir, '*.pdb')
    cms_pattern = os.path.join(source_dir, '*.cms')
    trj_pattern = os.path.join(source_dir, '*_trj')

    # Get name of directory (with some weirdness for how basename works in py)
    name = os.path.basename(source_dir[:-1] if source_dir[-1] == '/' else source_dir)
    tmptld = tempfile.mkdtemp()  # Keep so we can have a nice neat subdirectory name in tar
    tmpdir = os.path.join(tmptld, name)
    os.mkdir(tmpdir)

    if glob.glob(pdb_pattern):
        for pdb in glob.glob(pdb_pattern):
            shutil.copy2(pdb, tmpdir)
    elif glob.glob(cms_pattern):
        for cms in glob.glob(cms_pattern):
            pdb = normalize_topofile(cms)
            shutil.copy2(pdb, tmpdir)
            os.remove(pdb)
    else:
        raise ValueError("No topology files detected in " + source_dir)

    num_trj = len(glob.glob(trj_pattern))
    if num_trj != 1:
        raise ValueError("I expected exactly 1 trajectory folder, but I found " 
                + str(num_trj) + ": " + str(glob.glob(trj_pattern)))

    trj_dir = glob.glob(trj_pattern)[0]
    if not os.path.isdir(trj_dir):
        raise ValueError(trj_dir + " is not a path!")

    shutil.copytree(trj_dir, os.path.join(tmpdir, os.path.basename(trj_dir)))

    # Now decide on output
    if not compress:
        shutil.copy2(tmpdir, target)
    else:
        exitstatus = subprocess.run(['tar', '--zstd', '-cvf', target, '-C', 
                                     os.path.dirname(tmpdir), name])
        exitstatus.check_returncode()

    # Clean up. The directory we copied from might have been read-only, so lets fix that
    subprocess.run(['chmod','-R','u+rwx',tmpdir])
    shutil.rmtree(tmpdir)

if __name__ == '__main__':
    # normalize_desmond_data(sys.argv[1], sys.argv[2], compress=True)  # To generate compressed tarballs
    name = normalize_topofile(sys.argv[1])
    dst = sys.argv[1][:-4] + '.pdb'
    shutil.copy(name, dst)
    print("Created file " + dst)
    
