#ifndef RCPP_IMPTREE_STRUCTS_H
#define RCPP_IMPTREE_STRUCTS_H

#include <Rcpp.h>
#include "translation.h"
#include "enums.h"

struct ProbInterval {
  int obs;
  std::vector<int> freq;
  std::vector<double> lower;
  std::vector<double> upper;
  
  std::string to_string(const int nsmall = 6, const std::string &sep = "\t") const;
  Rcpp::NumericMatrix toMatrix() const;
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