/*
*
* File containing various array-based functions
*
*/
#include "utils.h"

#define LOG2E 0.693147180559945

/* UNUSED
** resort an array according to a given index
**
** index may be in any order and x is sorted
** along with the index being sorted in increasing order 
*/ 
// Rcpp::NumericVector re_sort_by_index(Rcpp::NumericVector x, Rcpp::IntegerVector index) {
//   
//   Rcpp::NumericVector res(index.size());
// 
//   for(int i = 0; i < index.size(); i++) {
//     res[index[i]] = x[i];
//   }
//   return res;
// }

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
int fcmp(const double x, const double y) {
	
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
double entropy(Rcpp::NumericVector x) {

	double sum = 0;
	for(int i = 0; i < x.size(); ++i) {
		if(x[i] > 0.0) {
			sum -= x[i] * log(x[i]);
		}
	}
	return sum / LOG2E;
}

/* entropy values here are all negative */
double calcT(const double maxE, const double minE,
                   const double maxEbase, const double minEbase, 
                   const double maxEposs, const double gamma) {
  double T = 2.0;
  if(maxE < maxEbase) {
    double pessimism = (maxEposs - maxEbase) / (maxEposs - maxE);
    double optimism = (minEbase - minE) / (maxE + fabs(minE - minEbase));
    // Minus in Hurvitz-like value due to partial negative entropy values in fraction
    double T =  gamma * pessimism - (1.0 - gamma) * optimism;
    if(maxE < minEbase) {
      // T may get at minimum -1 so with -3 we are guaranteed to have it as splitting candidate
      T -= 3.0;
    }
  }
  return T;
}

Rcpp::NumericMatrix asMatrixProbinterval(const ProbInterval & probint) {
  Rcpp::NumericMatrix result = Rcpp::NumericMatrix(2, probint.freq.size());
  result(0, Rcpp::_) = probint.upper;
  result(1, Rcpp::_) = probint.lower;
  return result;
}
