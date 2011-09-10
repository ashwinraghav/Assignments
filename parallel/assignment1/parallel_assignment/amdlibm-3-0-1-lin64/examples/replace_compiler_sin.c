#include <stdio.h>
#include <math.h>
#include "amdlibm.h"

/* using the following directives to replace sin with amd_sin */
#undef sin
#define sin amd_sin

int main()
{
    double in, out;
    in = 0.7;
    out = sin(in);
    printf("\nReplacing compiler's sin function with amd_sin ... \n");
    printf("Input: %lf, Output = %lf\n", in, out);

    return 0;
}

