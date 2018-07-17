from typing import List, Tuple
import Bio.SeqUtils
import os

def to_one_letter(name: str) -> str:
    """Convert a three-letter AA code to one letter using Biopython"""
    # Biopython requires upper-lower-lower format for lookups
    name = name.lower()
    name = name[0].upper() + name[1:]
    return Bio.SeqUtils.IUPACData.protein_letters_3to1[name.strip()]


class ProtParams(object):
    pdbid: str  #PDB ID: 4char alphanum
    chain: str  #Chain ID: single letter
    resid: int  #Residue ID
    reswt: str  #Wild-type residue
    resmut: str  #Mutated residue
    pH: float  #pH of experiment
    temp: float  #temperature of experiment
    ddT: float  #experimental ddT
    ddG: float  #experimental ddG

    def to_csv_row(self) -> str:
        out = ""
        out += self.pdbid + ","
        out += self.chain + ","
        out += str(self.resid) + ","
        out += self.reswt + ","
        out += self.resmut + ","
        out += str(self.pH) + ","
        out += str(self.temp) + ","
        out += str(self.ddT) + ","
        out += str(self.ddG)
        return out

    def set_from_pucci_row(self, args: List[str]):
        self.pdbid = args[1]
        self.chain = args[2]
        self.resid = int(args[3])
        self.reswt = to_one_letter(args[4])
        self.resmut = to_one_letter(args[5])
        self.pH = float(args[19].strip('[]'))

        # These arguments are sometimes missing in the data.
        try:
            self.temp = float(args[13])
        except:
            self.temp = None

        try:
            self.ddT = float(args[6])
        except:
            self.ddT = None

        try:
            self.ddG = float(args[12])
        except:
            self.ddG = None


class ProtResults:
    params: ProtParams
    ddG: float  # change in delta G
    rsa: float  # change in relevant solvent accessible area

    def to_csv_row(self) -> str:
        out = self.params.to_csv_row()
        out += "," + str(self.ddG)
        out += "," + str(self.rsa)
        return out

    def __init__(self, p, g, r):
        self.params = p
        self.ddG = g
        self.rsa = r


def genPDBpath(name: str, datapath: str) -> str:
    return os.path.join(datapath, "pdb", name) + ".pdb"


def genDSSPpath(name: str, datapath: str) -> str:
    return os.path.join(datapath, "dssp", name) + ".dssp"

