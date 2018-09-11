import sys, os
import time
import pickle
import subprocess
from multiprocessing import Pool
from typing import List, Tuple
from Bio.PDB.PDBParser import PDBParser
from Bio.PDB.PDBExceptions import PDBException
from gen_all_imutant import get_pdb_id

def load_and_sort(fname: str) -> List:
    """Take a filename with pickled prediction results and return a sorted list 
       of non-Nones"""
    with open(fname, 'rb') as pklfile:
        results = pickle.load(pklfile)
    nonNoneResults = [x for x in results if x is not None ]
    validResults = [ x for x in nonNoneResults if x.ddG is not None ]

    if len(validResults) < 0.9*len(results):
        print("[WARN]: %d/%d results were None, this run may be hopelessly busted"
                %(len(results)-len(validResults),len(results)))

    # Define a comparison function on ProtResults: returns pos number if
    # first element is greater, negative if second is greater, zero if tied
    def get_ddg(pred):
        return pred.ddG

    validResults.sort(key=get_ddg)
    return validResults

def get_inputs(pickleFName:str, pdbFName:str):
    results = load_and_sort(pickleFName)
    strictParser = PDBParser(PERMISSIVE=0)
    laxParser = PDBParser(PERMISSIVE=1)

    pdbName = get_pdb_id(pdbFName)
    try:
        pdbStructure = strictParser.get_structure(pdbName, pdbFName)
    except PDBException:
        pdbStructure = laxParser.get_structure(pdbName, pdbFName)
    except IOError as ioe:
        print("IOError encountered when trying to parse PDB file")
        print(ioe)
        sys.exit(1)

    return (results, pdbStructure)

def calc_residue_location(residue):
    """Calculate location of amino acid residue"""
    # For now, just use location of alpha-carbon. In future, can generalize to
    # some coarse-grained model or centroid of AA.
    return residue['CA'].get_coord()

def create_coarse_grain_model(pdbStructure):
    """Create a coarse grained model of the PDB, a lookup from (Chain,Res)
       tuples to numpy 3-vectors describing the location of that residue"""

    cgmodel = dict()
    for model in pdbStructure:
        for chain in model:
            cid = chain.id
            for residue in chain:
                resID = residue.get_id()
                if resID[0] != ' ':
                    # This is a HETATM. Disregard it
                    continue
                else:
                    resid = resID[1]
                    ident = (chain.id, resid)
                    cgmodel[ident] = calc_residue_location(residue)

    return cgmodel


