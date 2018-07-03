#ifndef RCPP_IMPTREE_NODE_H
#define RCPP_IMPTREE_NODE_H

#include <Rcpp.h>
#include "utils.h"
using namespace Rcpp;

struct ProbInterval {
  int obs;
  IntegerVector freq;
  NumericVector lower;
  NumericVector upper;
};

enum EntropyCorrection {NO = 0, ABELLAN, STROBL};

class Node {

private:
  Node *parent;
  std::vector<Node*> children;
  
  std::vector<int> obsidx;
  std::vector<int> attidx;
  
  NumericVector lower;
  NumericVector upper;
  virtual NumericVector maxEntropy(ProbInterval probint, bool exact);
  virtual NumericVector minEntropy(ProbInterval probint);
  virtual double correctionEntropy(NumericVector probs, int n, EntropyCorrection ec);
  virtual ProbInterval probabilityInterval(IntegerVector observations);
  
  NumericVector calcTValue(IntegerVector classvals, IntegerMatrix matx, 
                           std::vector<int> vidx, double gamma,
                           EntropyCorrection ec, bool exact = true);

public:
  Node(Node *parent = 0);
  ~Node();
  inline bool hasParent() const {return (!!(this->parent));}
  Node* getChild(int i) {return children.at(i);}
  int size() const {return children.size();}
};

class IDMNode : Node {
  
private:
  double s_;
  
  NumericVector maxEntropy(ProbInterval probint, bool exact = true);
  NumericVector minEntropy(ProbInterval probint);
  double correctionEntropy(NumericVector probs, int n, EntropyCorrection ec);
  ProbInterval probabilityInterval(IntegerVector observations);
  IntegerVector minInSet(NumericVector array, LogicalVector set);
  
public:
  IDMNode(Node* parent = 0, double s = 1.0);
};

class NPINode : Node {
  
private:
  
  NumericVector maxEntropy(ProbInterval probint, bool exact = true);
  NumericVector minEntropy(ProbInterval probint);
  
  NumericVector maxEntropyApprox(ProbInterval probint);
  NumericVector maxEntropyExact(ProbInterval probint);
  
  double correctionEntropy(NumericVector probs, int n, EntropyCorrection ec);
  ProbInterval probabilityInterval(IntegerVector observations);
  
public:
  NPINode(Node* parent = 0);
};


#endif /* RCPP_IMPTREE_NODE_H */
