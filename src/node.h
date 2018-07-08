#ifndef RCPP_IMPTREE_NODE_H
#define RCPP_IMPTREE_NODE_H

class Iptree;

#include <vector>
#include <Rcpp.h>
#include "utils.h"
#include "tree.h"
#include "enums.h"
using namespace Rcpp;

struct ProbInterval {
  int obs;
  IntegerVector freq;
  NumericVector lower;
  NumericVector upper;
};


class Node {

  Node *parent_;
  int depth_;
  std::vector<Node*> children_;
  ProbInterval probInt_;
  
  std::vector<int> obsidxs_;
  
  int splitvaridx_;
  std::vector<int> splitset_;
  
  virtual NumericVector maxEntropy(const ProbInterval &probint, const bool exact);
  virtual NumericVector minEntropy(const ProbInterval &probint);
  virtual double correctionEntropy(NumericVector probs, const int n);
  virtual ProbInterval probabilityInterval(IntegerVector observations);
  
  IntegerVector getNodeObservations(const int variableIndex);
  NumericVector calcTValue(std::vector<int> vidx);
  
protected:
  Iptree *tree_;
  
public:
  Node(Iptree *tree, int depth, Node *parent = 0);
  ~Node();
  
  inline bool hasParent() const {return (!!(this->parent_));}
  
  inline int getDepth() {return depth_;}

  Node* getChild(const int i) {return children_.at(i);}
  int size() const {return children_.size();}
  void setSplitVariable(const int idx);
  
  void setSplitObs(std::vector<int> obs) {obsidxs_ = obs;}
  void addSplitObs(const int obsidx) {obsidxs_.push_back(obsidx);}
  
  static Node* createNode(IpType::Enum ipt, Iptree *tree, int depth, Node* parent);
  
  void makeChildren();
};

class IDMNode : public Node {
  
private:
  double s_;
  
  NumericVector maxEntropy(const ProbInterval &probint, const bool exact = true);
  NumericVector minEntropy(const ProbInterval &probint);
  double correctionEntropy(NumericVector probs, const int n);
  ProbInterval probabilityInterval(IntegerVector observations);
  IntegerVector minInSet(NumericVector array, LogicalVector set);
  
public:
  IDMNode(Iptree *tree, int depth, Node* parent = 0);
};

class NPINode : public Node {
  
private:
  
  NumericVector maxEntropy(const ProbInterval &probint, const bool exact = true);
  NumericVector minEntropy(const ProbInterval &probint);
  
  NumericVector maxEntropyApprox(const ProbInterval &probint);
  NumericVector maxEntropyExact(const ProbInterval &probint);
  
  double correctionEntropy(NumericVector probs, const int n);
  ProbInterval probabilityInterval(IntegerVector observations);
  
public:
  NPINode(Iptree *tree, int depth, Node* parent = 0);
};


#endif /* RCPP_IMPTREE_NODE_H */
