#!/usr/bin/env python3

import sys, os, csv, subprocess
from typing import List, Tuple
from multiprocessing import Pool
import pickle
import pandas

sys.path.append("../common/")
from test_utils import *

imut_progname = "/opt/proteins/imutant/I-Mutant2.0.7/I-Mutant2.0.py"
pucci_datafile = "/home/chipbuster/NAS3/PDBStorage/puccidb/Pucci2016-r3.csv"
pdb_datapath = "/home/chipbuster/NAS3/PDBStorage/pdbredo/"

def process_results(input: str) -> Tuple[float, float, float]:
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

def run_imutant_once(protParams: ProtParams) -> ProtResults:
    """Build the call array, call the program, then parse the results"""
    callArray = ["python2", "-O"]
    callArray.append(imut_progname)
    callArray.append("-pdbv")
    callArray.append(genPDBpath(protParams.pdbid, pdb_datapath))
    callArray.append(genDSSPpath(protParams.pdbid, pdb_datapath))
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
        return run_imutant_once(inp)
    except RuntimeError as e:
        print(e)
        print("Caught runtime error. Continuing...")
        return None
    except Exception as e:
        print("Unknown error occured with " + inp.pdbid)
        print(e)
        return None


def main(args):

    params = []
    results = []
    with open(pucci_datafile, 'r') as infile:
        data = pandas.read_csv(infile)
        for i in range(data.shape[0]):
            try:
                p = ProtParams(*([None] * 9))#Cannot call mutator inline or will get None
                p.set_from_pucci_row(data.iloc[i])
                if p is not None:
                    params.append(p)
                    print(p)
            except ValueError as e:
                print("Error on line " + str(i))
                print(e)

    # If the length of the PDB ID is not one, this is a mutated protein. Exclude
    # it to avoid ceaseless error dumps from not having the mutations
    params = [ x for x in params if len(x.pdbid) == 4 ]

    with Pool(processes=16) as pool:
        results = list(pool.map(run_imutant_once_with_catch, params))

    with open("results.pkl", 'wb') as picklefile:
        pickle.dump(results, picklefile)

if __name__ == "__main__":
    main(sys.argv)
