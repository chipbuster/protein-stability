#include<math.h>
#include<stdbool.h>
#include<stdlib.h>
#include<assert.h>

// Convert angle1, angle2 into the square of the end-to-end distance.
// Uses angle ranges in [0,pi] where pi/2 is a straight line (half the angle 
// from the mid-atom to the line connecting the two endpoints)
float angles_to_e2esq(float a1, float a2){
    float x, y;
    x = 1.0;
    y = 0.0;

    x += cos(M_PI + 2.0 * a1);
    y += sin(M_PI + 2.0 * a1);

    x += cos(2.0 * a2);
    y += sin(2.0 * a2);

    return x*x + y*y;
}

// Returns whether the given angles generate a two-angle-chain whose
// end-to-end distance lies within the interval [lo, hi]
bool angles_sat_constraint(float a1, float a2, float lo, float hi){
    float losq = lo * lo;
    float hisq = hi * hi;

    assert(losq <= hisq);

    float e2esq = angles_to_e2esq(a1, a2);

    return losq <= e2esq && e2esq <= hisq;
}

// Finds a single pair of angles that works via rejection sampling.
// Output is written into first two elements of out.
void find_sat_angles(float* out, float lo, float hi){
    while(1){
        // Generate two floats in [0, pi]. Note: rounding properties are not 
        // great here -- right endpoint might never get hit exactly
        float a1 = ((double)rand()/(double)(RAND_MAX/M_PI));
        float a2 = ((double)rand()/(double)(RAND_MAX/M_PI));
        if(angles_sat_constraint(a1, a2, lo, hi)){
            out[0] = a1;
            out[1] = a2;
            return;
        }
    }
}

// Returns a buffer containing 2*nsamples elements. Each pair within the buffer
// is a pair of angles satisfying the specified constraint. Caller must free
// the buffer
float* sample_sat_angles(size_t nsamples, float lo, float hi){
    float* output = malloc(sizeof(float) * 2 * nsamples);
    float* curptr = output;
    for(int i = 0; i < nsamples; ++i){
        find_sat_angles(curptr, lo, hi);
        curptr += 2;
    }
    return output;
}
