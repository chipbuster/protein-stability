#!/usr/bin/env python3

import sys, os, csv, subprocess
from typing import List, Tuple
from multiprocessing import Pool
import pickle

from test_utils import *

imut_progname = "/opt/proteins/imutant/I-Mutant2.0.7/I-Mutant2.0.py"
pucci_datapath = "/home/chipbuster/Spinny/pucci-db"


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
    callArray.append(genPDBpath(protParams.pdbid, pucci_datapath))
    callArray.append(genDSSPpath(protParams.pdbid, pucci_datapath))
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

    if results.returncode != 0:
        print(results.stderr)
        raise RuntimeError("Call to IMutant2.0 failed. Call arguments were " +
                           str(callArray))

    (ddG, rsa) = process_results(results.stdout)

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
    with open(os.path.join(pucci_datapath, "Pucci2016-r3.csv"), 'r') as infile:
        for _ in range(3):
            next(infile)  # Skip the header rows for this file

        lineno = 3
        for line in infile:
            lineno += 1
            if (line[0] == ","):
                continue  # Line has no data
            else:
                try:
                    p = ProtParams()#Cannot call mutator inline or will get None
                    p.set_from_pucci_row(line.split(","))
                    if p is not None:
                        params.append(p)
                except ValueError as e:
                    print("Error on line " + str(lineno))
                    print(e)


    with Pool(processes=16) as pool:
        results = list(pool.map(run_imutant_once_with_catch, params))

    with open("results.pkl", 'wb') as picklefile:
        pickle.dump(results, picklefile)

if __name__ == "__main__":
    main(sys.argv)