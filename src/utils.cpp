/*
*
* File containing various array-based functions
*
*/

#include "utils.h"
#define LOG2E 0.693147180559945


/*
** get the index with the (first) maximum value
** in an array based on a selection defined in set
*/
// [[Rcpp::export]]
int maxIndexInSet(NumericVector array, LogicalVector set) {

	int index = -1;
	int max = -1;

	for(int i = 0; i < array.size(); i++) {
		if(set[i] && array[i] > max) {
			max = array[i];
			index = i;
		}
	}
	return index;
}

/*
** resort an array according to a given index
**
** index may be in any order and x is sorted
** along with the index being sorted in increasing order 
*/
NumericVector re_sort_by_index(NumericVector x, IntegerVector index) {
  
  NumericVector res(index.size());

  for(int i = 0; i < index.size(); i++) {
    res[index[i]] = x[i];
  }
  return res;
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
// [[Rcpp::export]]
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
double entropy(NumericVector x) {

	double sum = 0;
	for(int i = 0; i < x.size(); ++i) {
		if(x[i] > 0.0) {
			sum -= x[i] * log2(x[i]);
		}
	}
	return sum;
}

/* calculate logarithm to base 2*/
double log2(double x) {
  return log(x) / LOG2E;
}

double calcT(double maxE, double minE, double maxEbase, double minEbase, double maxEposs, double gamma) {
  
  double T = 2;
  if(maxE < maxEbase) {
    double pessimism = (maxEposs - maxEbase) / (maxEposs - maxE);
    double optimism = (minEbase - minE) / (maxE + fabs(minE - minEbase));
    double T =  gamma * pessimism - (1 - gamma) * optimism;
    if(maxE < minEbase) {
// T may get at minimum -1 so with -3 we are guaranteed to have it as splitting candidate
      T -= 3;
    }
  }
  return T;
}


