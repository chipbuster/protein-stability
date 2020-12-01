Avinery Reflection-Based Chain
==============================

A reflection based chain simulation. Unclear if this is correct or not.

Procedure: pick random indices i,j, such that |i - j| >= 3.

Draw the line from atom[i] to atom[j] and reflect all atoms between the
two across that line (atoms not between i and j are ignored).

Apply repeatedly until simulation is converged.

Note that this method alone will cause the chain to act as if it has
fixed endpoints, as the ends of the chain can never be reflected.
