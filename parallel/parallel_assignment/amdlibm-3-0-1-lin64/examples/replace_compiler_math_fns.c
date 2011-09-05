#include <stdio.h>
#include <math.h>
#include "amdlibm.h"

/* using the following directive to replace all compiler math functions
with AMD LibM functions */
#define REPLACE_WITH_AMDLIBM

int main()
{
    {
        float in, out;
        in = 0.7f;
        out = expf(in);
        printf("\nReplacing compiler's expf function with amd_expf ... \n");
        printf("Input: %f, Output: %f\n", in, out);
    }
    {
        float in, out;
        in = 0.7f;
        out = sinf(in);
        printf("\nReplacing compiler's sinf function with amd_sinf ... \n");
        printf("Input: %f, Output: %f\n", in, out);
    }
    {
        double in1, in2, out;
        in1 = 5.0;
        in2 = 7.0;
        out = pow(in1, in2);
        printf("\nReplacing compiler's pow function with amd_pow ... \n");
        printf("Input: (%lf, %lf), Output: %lf\n", in1, in2, out);
    }

    return 0;
}

