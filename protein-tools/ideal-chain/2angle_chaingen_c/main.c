#include<stdio.h>
#include<stdlib.h>
#include<time.h>
float* sample_sat_angles(size_t nsamples, float lo, float hi);

int main(int argc, char** argv){
    srand(time(NULL));
    float* output = sample_sat_angles(10000000, 1.0, 1.2);
    FILE* outf = fopen(argv[1], "w");
    fwrite(output, sizeof(float), 20000000, outf);
    fclose(outf);
}
