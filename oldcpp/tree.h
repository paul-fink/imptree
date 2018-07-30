#ifndef RCPP_IMPTREE_TREE_H
#define RCPP_IMPTREE_TREE_H


#include <Rcpp.h>
#include "node.h"
#include "enums.h"
#include "structs.h"
#include "utils.h"
#include "evaluation.h"

class Node;

class Iptree {
  Node* root_ = nullptr;
  Config* cfg_;
  Data* data_;
  
  
public:
  inline Config* getConfig() {return cfg_;}
  inline Data* getData() const {return data_;}
  inline bool hasRoot() {return (root_ != nullptr);}
  
  Iptree(const Rcpp::IntegerMatrix & data, const Rcpp::List & config);
  ~Iptree();
  
  void growTree();
  
  // Evaluation method
  Evaluation evaluate(const Rcpp::IntegerMatrix & newdata, const Rcpp::List & evalconfig);
  
  // Summary methods
  int getNumberOfNodes() const;
  int getNumberofLeaves() const;
  int getDepth() const;
  
  void printTree(const int nsmall, const std::string &sep) const;
  
  ProbInterval getIndexProbInterval(std::vector<int>& idxs) const;
};

#endif /*RCPP_IMPTREE_TREE_H*/
