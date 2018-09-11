#!/usr/bin/env python3
""" 
    Generate all possible mutants for a protein and dump into a pickle
"""

import sys, os
import time
import pickle
import subprocess
from multiprocessing import Pool
from typing import List, Tuple
from Bio.PDB.PDBParser import PDBParser
from Bio.PDB.PDBExceptions import PDBException

from test_utils import *

# List of all amino acids
aas = [
    "Ala", "Gly", "Ile", "Leu", "Pro", "Val", "Phe", "Trp", "Tyr", "Asp",
    "Glu", "Arg", "His", "Lys", "Ser", "Thr", "Cys", "Met", "Asn", "Gln"
]

def get_pdb_id(fname):
    """Generate PDB ID from file name of PDB"""
    return os.path.basename(fname).split('.')[0]

def gen_mutation_sequence(fname):
    """Read a given PDB file and return a list of ProtParams that describe the
       mutations to be performed.
    """

    strictParser = PDBParser(PERMISSIVE=0)
    laxParser = PDBParser(PERMISSIVE=1)

    pdbName = get_pdb_id(fname)
    try:
        pdbStructure = strictParser.get_structure(pdbName, fname)
    except PDBException as pe:
        pdbStructure = laxParser.get_structure(pdbName, fname)
    except IOError as ioe:
        print("IOError encountered when trying to parse PDB file")
        print(ioe)
        sys.exit(1)

    toRun = []

    for model in pdbStructure:
        for chain in model:
            for residue in chain:
                resID = residue.get_id()
                if resID[0] != ' ':
                    # This is a HETATM. Disregard it and continue.
                    continue
                for mutAA in aas:
                    if to_one_letter(
                            residue.get_resname()) == to_one_letter(mutAA):
                        continue  # this is the identity mutation
                    param = ProtParams(
                        pdbid=pdbName,
                        chain=residue.get_parent().get_id(),
                        resid=resID[1],
                        reswt=to_one_letter(residue.get_resname()),
                        resmut=to_one_letter(mutAA),
                        pH=7.7,
                        temp=30,
                        ddT=None,
                        ddG=None)

                    toRun.append(param)

    return toRun


def run_imutant_once(protParams: ProtParams, pdbPath: str,
                     dsspPath: str) -> ProtResults:
    """Build the call array, call the program, then parse the results"""
    callArray = ["python2", "-O"]
    callArray.append(imut_progname)
    callArray.append("-pdbv")
    callArray.append(pdbPath)
    callArray.append(dsspPath)
    callArray.append(protParams.chain)
    callArray.append(str(protParams.resid))
    callArray.append(protParams.resmut)
    callArray.append(str(protParams.pH))
    callArray.append(str(protParams.temp))

    results = subprocess.run(
        callArray,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True,
        timeout=30)

    try:
        (ddG, rsa) = process_results(results.stdout)
    except Exception as e:
        print("Something went wrong with this IMutant Run")
        print(e)
        raise RuntimeError("Call to IMutant2.0 failed. Call arguments were " +
                           " ".join(callArray))

    return ProtResults(protParams, ddG, rsa)


def run_imutant_once_with_catch(inp):
    try:
        return run_imutant_once(*inp)
    except RuntimeError as e:
        print(e)
        print("Caught runtime error. Continuing...")
    except Exception as e:
        print("Unknown error occured with " + inp.pdbid)
        print(e)
    finally:
        return None

def process_results(input: str) -> Tuple[float, float]:
    """Parse out ddT, ddG, and RSA from results"""
    input = input.splitlines()

    # IMutant output is meant to be human-readable, not machine-parseable. To
    # get the correct line, we look for the line starting with "Position", indic
    # that it's the header, then grab the line after that and parse it.
    getNext = False
    for line in input:
        if line.strip()[0:8] == 'Position':
            getNext = True
            continue

        if getNext:
            entries = line.split()
            ddG = float(entries[3])
            rsa = float(entries[-1])

            return (ddG, rsa)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: %s <path_to_pdb> <path_to_dssp>" % sys.argv[0])
        sys.exit(-1)

    pdbPath = sys.argv[1]
    dsspPath = sys.argv[2]
    candidates = gen_mutation_sequence(sys.argv[1])

    print("Finished generating candidates! There are %d mutations to test." %
          len(candidates))

    t1 = time.perf_counter()

    progArgs = [(param, pdbPath, dsspPath) for param in candidates]

    with Pool(processes=15) as pool:
        results = list(pool.map(run_imutant_once_with_catch, progArgs))

    t2 = time.perf_counter()

    pklfname = get_pdb_id(pdbPath) + "_results.pkl"

    with open(pklfname, 'wb') as picklefile:
        pickle.dump(results, picklefile)

    print("Finished! Took %f seconds to run %d mutations." % (t2 - t1,
                                                              len(candidates)))
