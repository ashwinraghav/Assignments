#include <stdio.h>

extern void use_cbrt_functions();

int main()
{
    printf("\nIllustration of AMD LibM functions\n");
    printf("With C compiler\n\n");
    use_cbrt_functions();
    printf("\n\n");
    return 0;
}

