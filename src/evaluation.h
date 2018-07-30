#ifndef RCPP_IMPTREE_EVALUATION_H
#define RCPP_IMPTREE_EVALUATION_H

#include <Rcpp.h>
#include "enums.h"
#include "structs.h"

class Evaluation {
  
  double utility_;
  Dominance dominance_;
  std::vector<ProbInterval> probInts_;
  Rcpp::LogicalMatrix boolPrediction_;
  Data observations_;
  
  double obs_det_ = 0.0;
  int obs_indet_ = 0;
  
  double size_indet_ = 0.0;
  
  double acc_disc_ = 0.0;
  double acc_util_ = 0.0;
  
  double acc_single_ = 0.0;
  double acc_set_ = 0.0;
  
  double quadratic_utility(double acc_disc);
  std::vector<bool> computeNonDominatedSet(const ProbInterval &probint);
  void updateCredalStatistics(int obsIdx);
  void finalizeCredalStatistics();
  void evaluate();
  
public:
  Evaluation(const double utility, const Dominance dominance,
             const std::vector<ProbInterval> &probInts, Data observations);
  
  Rcpp::List summary() const;
  Rcpp::List probIntervalList() const;
  Rcpp::LogicalMatrix predictions() const {return boolPrediction_;} 
  
};

#endif /*RCPP_IMPTREE_EVALUATION_H*/
