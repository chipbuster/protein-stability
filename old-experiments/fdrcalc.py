#!/usr/bin/env python3

import sys

def calc_fdr(sens, spec, acc):
    """Compute the false discovery rate.

    Given the sensitivity, specificity, and accuracy as numbers in [0,1], 
    computes the false discovery rate. """

    # Compute Prevalence
    prev = (acc - spec) / (sens - spec)
    print(prev) # Prevalence is > 1. Why?
   
    oppSpec = 1.0 - spec
    oppPrev = 1.0 - prev

    fdr = oppSpec * oppPrev / (oppSpec * oppPrev + sens * prev)

    return fdr

if __name__ == "__main__":
    try:
        sens = float(sys.argv[1])
        spec = float(sys.argv[2])
        acc = float(sys.argv[3])

        fdr = calc_fdr(sens, spec, acc)
        print("FDR is " + str(fdr))
    except (ValueError, IndexError) as e:
        print("Program caught exception: " + str(e))
        print("Usage: %s <sens> <spec> <accu>" % sys.argv[0])
