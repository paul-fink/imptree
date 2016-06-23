/*
** Header file to be included in entropy files
**
** It contains all the basic auxiliary functions
**
*/
#include<R.h>
#include<Rmath.h>
#include<R_ext/Print.h>

/* functions dealing with min and max */
int maxIndex(double[], int);
int maxIndexInSet(int[], int[], int);

/* Summing over an array */
double fArraySum(double[], int);
int iArraySum(int[], int);

/* sorting an array according to an int array*/
void re_sort_by_index(double[], int[], int); 

/* compare floats */
int fcmp(double, double);

/* calculate the Shannon entropy */
double entropy(double[], int);
