/*
Maxium and Minimum Entropy Algorithm according to 
	Abellan & Moral (Maximum of Entropy for credal Sets)

The main functions are 'MaxEntropy' and 'MinEntropy', which are 
called via the 'C' function in R

Futher are auxiliary functions:
+ getMinEntropy:	calculates the minimum entropy distribution
+ minInSet: 		calculating the minimal and second minimal value
				of an array subject to a set of allowed indices
+ countMins:		Counting the numer of minimal values of an array,
				subject to a set of allowed indices
+ getMaxEntropy:	Wrapper function for 'MaxEntropy'

'MaxEntropy' and 'MinEntropy' take the same arguments as:
+ upper:	numeric array containing upper bounds of the
			probability intervals (length: n)
+ lower:	numeric array containing lower bounds of the
			probability intervals (length: n)
+ dim:		numeric given the lenght of the arrays 
*/


#include "utils.h"


double minInSet (double[], int[], int, int);
int countMins (double, double[], int[], int);
void maxEntropyIDM(double[], double[], int*);
void minEntropyIDM(double[], double[], int*);

/* Auxiliary functions, see description above */

double minInSet (double *array, int *set, int dim, int index) {
	int c;
	double min1, min2;
/* Initianlizing the minimal values with a not reachable one in the array */
	min1 = min2 = 1000000000.0;
	for ( c = 0 ; c < dim ; c++ ) {
/* Only considering those which are allowed according to the set */
		if(set[c]> 0) {
/* In case a new minimum is identified, the old minimum in 'min1'
** is passed to 'min2' and 'min1' gets the new one */
			if ( fcmp(array[c], min1) < 0) {
				min2 = min1;
				min1 = array[c];
			} else if( fcmp(array[c], min2) < 0 && fcmp(array[c], min1) > 0) {
/* In case a the value is greater than the minimum, 
** but smaller than the second minimal value, then 
** it is assigned to 'min2' */
				min2 = array[c];
			}
		}
	}
/* return either the minimum or the second minimal value as 
** specified by 'index' */
	if(index == 1) {
		return min1;
	} else if (index == 2) {
		return min2;
	} else {
		return -1.0;
	}
}

/*
	Counting the number of times the minimum value is in the 'array',
	but only considering those indices which are allowed according to 'set'
*/
int countMins (double min, double *array, int *set, int dim) {
	int n = 0;
	int i;
	for ( i = 0 ; i < dim ; i++ ) {
		if( set[i] > 0 && fcmp(array[i], min) == 0) {
			n++;
		}
	}
	return n;
}


/* Functions called by R, see description above */

void maxEntropyIDM (double *lower, double *upper, int *dim) {
/* Initializing */	
	double sum, min, smin, minarray[3];
	int i, j, nmin, set[*dim], minset[3];
/* Summing up all lower values */
	sum = fArraySum(lower, *dim);
/* Generating a set of index which shall be considered for adjustment:
** Only those may be adjusted, where the lower < upper */
	for (i = 0; i < *dim; i++) {
		if(fcmp(lower[i], upper[i]) == 0) {
			set[i] = 0;
		} else {
			set[i] = 1;
		}
	}
/* Intializing the required set for the minimum calculation (req. for 'minInSet') */
	for (i = 0; i < 3; i++) minset[i] = 1;
/* we only proceed into the adjustment, when the sum is lower than 1,
** i.e. not allready a probability distribution */
	while (fcmp(sum, 1.0) < 0) {

/* obtainig the minimal and second minimal value, as well as the 
** number of times the minimal vlaue is attained */
		min = minInSet(lower, set, *dim, 1);
		smin = minInSet(lower, set, *dim, 2);
		nmin = countMins(min, lower, set, *dim);

		for ( j = 0 ; j < *dim ; j++ ) {
/* adjustment takes place only for minimal values */
			if(fcmp(lower[j], min) == 0) {
				minarray[0] = upper[j] - lower[j];
				minarray[1] = (1 - sum) / nmin;
				if(fcmp(smin, 0.0) > 0) {
					minarray[2] = smin - min;
				} else {
					minarray[2] = 1;
				}
				lower[j] = lower[j] + minInSet (minarray, minset, 3, 1);
/* adjusting the set */
				if(fcmp(lower[j], upper[j]) == 0) {
					set[j] = 0;
				}
			}
		}
/* updating the sum value */
		sum = fArraySum(lower, *dim);
	}
}


void minEntropyIDM(double *lower, double *upper, int *dim) {

/* Initializing */
	int index = -1;
/* get the index with the (first) maximum */
	index = maxIndex(lower, *dim);
/* if valid index then set the value of the lower boundary to the 
** value of the upper one */
	if ( index > -1 && index < *dim ) {
		lower[index] = upper[index];
	}
}


