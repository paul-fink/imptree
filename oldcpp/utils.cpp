/*
*
* File containing various array-based functions
*
*/
#include "utils.h"


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
double entropy(std::vector<double> x) {

	std::transform(x.begin(), x.end(), x.begin(),
                [](double d) -> double {
                return ((d > 0.0) ? (d * std::log2(d)) : 0.0);
  });
  return -std::accumulate(x.begin(), x.end(), 0.0);
}

