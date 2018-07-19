#ifndef RCPP_IMPTREE_STRUCTS_H
#define RCPP_IMPTREE_STRUCTS_H

#include <Rcpp.h>
#include "enums.h"

struct ProbInterval {
  int obs;
  Rcpp::IntegerVector freq;
  Rcpp::NumericVector lower;
  Rcpp::NumericVector upper;
  
  std::string to_string(const int nsmall = 6, const std::string &sep = "\t") const;
};

struct Config {
  double s; // strictly positive
  double gamma; // in (0;1)
  double tbase; // in [-1,2]
  int minbucket; // positive int
  int maxdepth; // strictly positive int
  EntropyCorrection ec;
  SplitMetric sm;
  IpType ip;
};

struct Data {
  Data() {};
  Data(const Rcpp::IntegerMatrix & mat);
  
  Rcpp::IntegerMatrix data;
  int classidx;
  Rcpp::IntegerVector nlevels;
  Rcpp::List labels;
  Rcpp::CharacterVector varnames;
  
};
#endif /*RCPP_IMPTREE_STRUCTS_H*/