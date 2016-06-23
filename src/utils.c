/*
*
* File containing various array-based functions
*
*/

#include "utils.h"

/*
** get the index with the (first) maximum value
** in an array
*/

int maxIndex (double *array, int dim) {
	
	int i;
	int index = -1;
	double max = -1.0;	
	
	for(i = 0; i < dim; i++) {
		if(array[i] > max) {
			max = array[i];
			index = i;
		}
	}
        return index;
}

/*
** get the index with the (first) maximum value
** in an array based on a selection defined in set
*/

int maxIndexInSet(int *array, int *set, int dim) {

	int i;
	int index = -1;
	int max = -1;

	for(i = 0; i < dim; i++) {
		if(set[i] > 0 && array[i] > max) {
			max = array[i];
			index = i;
		}
	}
	return index;
}


/*
**	Sum over a float array
*/
double fArraySum (double *array, int dim) {

        double sum = 0;
        int i;

        for ( i = 0 ; i < dim ; i++ ) {
                sum += array[i];
        }
        return sum;
}

/*
** Sum over an integer array
*/
int iArraySum(int *array, int dim) {

        int i, sum = 0;

        for(i = 0; i < dim; i++) {
                sum += array[i];
        }
        return sum;
}

/*
** resort an array according to a given index
**
** index may be in any order and array is sorted
** along with the index being sorted in increasing order 
*/
void re_sort_by_index(double *x, int *index, int dim) {

        int i;
        double res[dim];

        for(i = 0; i < dim; i++) {
                res[index[i]] = x[i];
        }
        for(i = 0; i< dim; i++) {
                x[i] = res[i];
        }
}

/*
** compare two double values for equality
** by assessing it indirectly
** 
** Based on
**
** Knuth's floating point comparison operators, from:
** Knuth, D. E. (1998). The Art of Computer Programming.
** Volume 2: Seminumerical Algorithms. 3rd ed. Addison-Wesley.
** Section 4.2.2, p. 233. ISBN 0-201-89684-2.
*/
int fcmp(double x, double y) {
	
	int exponent;
	double delta, difference;
	double epsilon = 1.0e-16;

	frexp(fabs(x) > fabs(y) ? x : y, &exponent);
	delta = ldexp(epsilon, exponent); 
	difference = x - y;

	if (difference > delta) {
		 /* x > y */
		return 1;
	} else if (difference < -delta) {
		/* x < y */
		return -1;
	} else {
		/* -delta <= difference <= delta */
		/* x == y */
		return 0;
	}
}

/* Calculation of Shannon's Entropy, if only using values larger than 0 */
double entropy(double *x, int dim) {

	double sum = 0;
	int i;
	for(i = 0; i < dim; i++) {
		if(x[i] > 0.0) {
			sum -= log(x[i]);
		}
	}
	return sum;
}



