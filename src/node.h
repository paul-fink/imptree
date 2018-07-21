// [[Rcpp::plugins(cpp11)]]

#ifndef RCPP_IMPTREE_NODE_H
#define RCPP_IMPTREE_NODE_H


#include <Rcpp.h>
#include "utils.h"
#include "structs.h"
#include "tree.h"
#include "enums.h"

class Iptree;

class Node {

  Node *parent_ = nullptr;
  int depth_ = 0;
  std::vector<Node*> children_;
  ProbInterval probInt_;
  
  std::vector<int> obsidxs_;
  
  int splitvaridx_ = -1;
  std::vector<int> splitset_;
  
  virtual Rcpp::NumericVector maxEntropy(const ProbInterval &probint, const bool exact) = 0;
  virtual Rcpp::NumericVector minEntropy(const ProbInterval &probint) = 0;
  virtual double correctionEntropy(Rcpp::NumericVector probs, const int n) = 0;
  virtual ProbInterval probabilityInterval(Rcpp::IntegerVector observations) = 0;
  
  Rcpp::IntegerVector getNodeObservations(const int variableIndex);
  int calcSplitVariable();
  
protected:
  Iptree* tree_ = nullptr;
  
public:
  Node(Iptree *tree, int depth = 0, Node *parent = nullptr);
  virtual ~Node();
  
  inline bool hasParent() const {return this->parent_ != nullptr;}
  
  inline int getDepth() {return depth_;}

  Node* getChild(const int i) {return children_.at(i);}
  size_t size() const {return children_.size();}
  
  void setSplitVariable(const int idx);
  void addSplitObs(const int obsidx) {obsidxs_.push_back(obsidx);}
  
  ProbInterval classify(Rcpp::IntegerVector observation);
  
  // Factory Method
  static Node* createNode(IpType ipt, Iptree *tree, int depth, Node* parent = 0);
  
  void makeChildren();
  
  // Summary methods
  int numLeaves() const;
  int numNodes() const;
  void addDepth(std::vector<int> * depths) const;
  
  void printNode(const int parentIdx, const int nsmall, const std::string &sep) const;
};

class IDMNode : public Node {
  
private:
  double s_ = 0;
  
  Rcpp::NumericVector maxEntropy(const ProbInterval &probint, const bool exact = true);
  Rcpp::NumericVector minEntropy(const ProbInterval &probint);
  double correctionEntropy(Rcpp::NumericVector probs, const int n);
  ProbInterval probabilityInterval(Rcpp::IntegerVector observations);
  
  Rcpp::IntegerVector minInSet(Rcpp::NumericVector array, Rcpp::LogicalVector set);
  
public:
  IDMNode(Iptree *tree, int depth, Node* parent = nullptr);
};

class NPINode : public Node {
  
private:
  
  Rcpp::NumericVector maxEntropy(const ProbInterval &probint, const bool exact = true);
  Rcpp::NumericVector maxEntropyApprox(const ProbInterval &probint);
  Rcpp::NumericVector maxEntropyExact(const ProbInterval &probint);
  Rcpp::NumericVector minEntropy(const ProbInterval &probint);
  double correctionEntropy(Rcpp::NumericVector probs, const int n);
  ProbInterval probabilityInterval(Rcpp::IntegerVector observations);
  
  int maxIndexInSet(Rcpp::NumericVector array, Rcpp::LogicalVector set);
  
public:
  NPINode(Iptree *tree, int depth, Node* parent = nullptr);
};


#endif /* RCPP_IMPTREE_NODE_H */
