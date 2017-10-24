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
int maxIndex(NumericVector array);
int maxIndexInSet(NumericVector array, LogicalVector set);

/* sorting an array according to an int array*/
NumericVector re_sort_by_index(NumericVector array, IntegerVector index); 

/* compare floats */
int fcmp(double, double);

/* calculate the Shannon entropy */
double entropy(NumericVector array);

double calcT(double maxE, double minE, double maxEbase, double minEbase, double maxEposs, double gamma);

#endif /* RCPP_IMPTREE_UTILS_H */
