/***************************************************************************
Mathematical Acceleration Subsystem Component V4.1 

Licensed Materials - Property of IBM
5724-K76	
(C) Copyright IBM Corporation 1985, 2004.  All Rights Reserved.
US Government Users Restricted Rights - Use, duplication or 
disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
***************************************************************************/
 
/* C/C++ prototype declarations for Linux MASS vector functions */

#ifdef __cplusplus
extern "C" {
#endif

extern void vrec (double y[], double x[], int *n); 
  /* Sets y[i] to the reciprocal of x[i], for i=0,..,n-1 */

extern void vsqrt (double y[], double x[], int *n); 
  /* Sets y[i] to the square root of x[i], for i=0,..,n-1 */

extern void vrsqrt (double y[], double x[], int *n); 
  /* Sets y[i] to the reciprocal of the square root of x[i], for i=0,..,n-1 */

extern void vsrec (float y[], float x[], int *n); 
  /* Sets y[i] to the reciprocal of x[i], for i=0,..,n-1 */

extern void vssqrt (float y[], float x[], int *n); 
  /* Sets y[i] to the square root of x[i], for i=0,..,n-1 */

extern void vsrsqrt (float y[], float x[], int *n); 
  /* Sets y[i] to the reciprocal of the square root of x[i], for i=0,..,n-1 */

#ifdef __cplusplus
}
#endif
