#ifndef RCPP_IMPTREE_UTILS_H
#define RCPP_IMPTREE_UTILS_H

/*
** Header file to be included in entropy files
**
** It contains all the basic auxiliary functions
**
*/
#include <Rcpp.h>
#include "structs.h"


/* sorting an array according to an int array*/
//Rcpp::NumericVector re_sort_by_index(Rcpp::NumericVector array, Rcpp::IntegerVector index); 

/* compare floats */
int fcmp(const double x, const double y);

/* calculate the Shannon entropy */
double entropy(std::vector<double> x);

#endif /* RCPP_IMPTREE_UTILS_H */
