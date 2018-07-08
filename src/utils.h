#ifndef RCPP_IMPTREE_UTILS_H
#define RCPP_IMPTREE_UTILS_H

/*
** Header file to be included in entropy files
**
** It contains all the basic auxiliary functions
**
*/
#include<R.h>
#include<Rmath.h>
#include<R_ext/Print.h>
#include<Rcpp.h>

using namespace Rcpp;


/* functions dealing with min and max */
int maxIndexInSet(NumericVector array, LogicalVector set);

/* sorting an array according to an int array*/
NumericVector re_sort_by_index(NumericVector array, IntegerVector index); 

/* compare floats */
int fcmp(const double x, const double y);

/* calculate the Shannon entropy */
double entropy(NumericVector array);

/* calculate logarithm to base 2 */
double log2(const double x);

double calcT(const double maxE, const double minE,
             const double maxEbase, const double minEbase, 
             const double maxEposs, const double gamma);
#endif /* RCPP_IMPTREE_UTILS_H */
