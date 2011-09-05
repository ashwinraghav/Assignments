/***************************************************************************
Mathematical Acceleration Subsystem Component V4.1 

Licensed Materials - Property of IBM
5724-K76	
(C) Copyright IBM Corporation 1985, 2004.  All Rights Reserved.
US Government Users Restricted Rights - Use, duplication or 
disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
***************************************************************************/

#pragma options float=rsqrt libansi
#include <math.h>

void vrec (double y[], double x[], int *n) 
{
  /* Sets y[i] to the reciprocal of x[i], for i=0,..,n-1 */
  int i;
  for (i=0; i<*n; i++)
    y[i] = 1.0/x[i];
}
  
void vsqrt (double y[], double x[], int *n) 
{
  /* Sets y[i] to the square root of x[i], for i=0,..,n-1 */
  int i;
  for (i=0; i<*n; i++)
    y[i] = sqrt(x[i]);
}

void vrsqrt (double y[], double x[], int *n) 
{
  /* Sets y[i] to the reciprocal of the square root of x[i], for i=0,..,n-1 */
  int i;
  for (i=0; i<*n; i++)
    y[i] = 1.0/sqrt(x[i]);
}

void vsrec (float y[], float x[], int *n) 
{
  /* Sets y[i] to the reciprocal of x[i], for i=0,..,n-1 */
  int i;
  for (i=0; i<*n; i++)
    y[i] = 1.0f/x[i];
}

void vssqrt (float y[], float x[], int *n) 
  /* Sets y[i] to the square root of x[i], for i=0,..,n-1 */
{
  int i;
  for (i=0; i<*n; i++)
    y[i] = sqrt(x[i]);
}

void vsrsqrt (float y[], float x[], int *n) 
{
  /* Sets y[i] to the reciprocal of the square root of x[i], for i=0,..,n-1 */
  int i;
  for (i=0; i<*n; i++)
    y[i] = 1.0f/sqrt(x[i]);
}
