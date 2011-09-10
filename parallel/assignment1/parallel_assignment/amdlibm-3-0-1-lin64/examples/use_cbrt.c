#include <stdio.h>
#include "amdlibm.h" 

void use_cbrt_functions()
{
    /*

      There are 6 different flavors of cbrt routines in AMD LibM.
      Example usage for all of them is demonstrated here.

      double amd_cbrt(double x);
      float amd_cbrtf(float x);
      __m128d amd_vrd2_cbrt(__m128d x);
      __m128 amd_vrs4_cbrtf(__m128 x);
      void amd_vrda_cbrt( int len, double *src, double *dst );
      void amd_vrsa_cbrtf( int len, float *src, float *dst );

    */

    {
        /*double precision calculation*/
        double result_cbrt; 
        double input; 
        input = 34.65; 
        printf("\nUsing amd_cbrt ... \n");
        result_cbrt = amd_cbrt(input);
        printf("Input: %lf, Output = %lf\n", input, result_cbrt);
    }
    {
        /*single precision calculation*/
        float result_cbrt; 
        float input; 
        input = 34.65; 
        printf("\nUsing amd_cbrtf ... \n");
        result_cbrt = amd_cbrtf(input);
        printf("Input: %f, Output = %f\n", input, result_cbrt);
    }
    {
        /*double precision vector variant calculation*/
        __m128d result_cbrt; 
        __m128d input; 
        double  input_array[2] = {34.65, 67.89};
        double  output_array[2];

        input = _mm_loadu_pd(input_array);
        printf("\nUsing amd_vrd2_cbrt ... \n");
        result_cbrt = amd_vrd2_cbrt(input);
        _mm_storeu_pd(output_array, result_cbrt);

        printf("Input: {%lf, %lf}, Output = {%lf, %lf}\n",
                input_array[0], input_array[1],
                output_array[0], output_array[1]);
    }
    {
        /*single precision vector variant calculation*/
        __m128 result_cbrt; 
        __m128 input; 
        float  input_array[4] = {34.65, 67.89, 91.0, 198.34};
        float  output_array[4];

        input = _mm_loadu_ps(input_array);
        printf("\nUsing amd_vrs4_cbrtf ... \n");
        result_cbrt = amd_vrs4_cbrtf(input);
        _mm_storeu_ps(output_array, result_cbrt );

        printf("Input: {%f, %f, %f, %f}, Output = {%f, %f, %f, %f}\n",
                input_array[0], input_array[1], input_array[2], input_array[3],
                output_array[0], output_array[1], output_array[2], output_array[3]);
    }
    {
        /*double precision array/buffer variant calculation*/
        int i;
        double  input_array[10] = {34.65,76.56,91.78,101.98,1001.7890,10098.1257,1098.56,9187.3,5.1,0.00657};
        double  output_array[10];
        printf("\nUsing amd_vrda_cbrt ... \n");
        amd_vrda_cbrt(10, input_array,output_array);
    
        for(i=0; i<10; i++)
            printf("Input[%d]: %15.8lf, Output[%d] = %15.8lf\n", i, input_array[i], i, output_array[i]);
    }
    {
        /*single precision array/buffer variant calculation*/
        int i;
        float  input_array[10] = {34.65,76.56,91.78,101.98,1001.7890,10098.1257,1098.56,9187.3,5.1,0.00657};
        float  output_array[10];
        printf("\nUsing amd_vrsa_cbrtf ... \n");
        amd_vrsa_cbrtf(10, input_array,output_array);

        for(i=0; i<10; i++)
            printf("Input[%d]: %15.8f, Output[%d] = %15.8f\n", i, input_array[i], i, output_array[i]);
    }

}

