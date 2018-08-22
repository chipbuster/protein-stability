Graphset Test Code
==================

# Fundamental idea

Use single-mutation predictor to boost to multiple mutations:
   1. Generate all single-point mutation predictions
   2. Find mutations that are both positive and "not too close" for some definition of too close
   3. Combine to yield set of multimutations

# 1. Generate all SPMs

Two ways to do this:
  - Generate and test all possible SPMs
  - Generate sample SPMs using recommender, test with discriminator known to be high-quality (e.g. FEP+)
  - For now, use the first (IMutant, test all mutations)

# 2. Find mutations that are both positive and "not too close"

  Consider 2 aa along backbone or within 5 angstroms to be "too close" (will need to add sophistication for long-range charge interactions)

# 3. Combine

Easy!