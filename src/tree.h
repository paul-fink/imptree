#ifndef RCPP_IMPTREE_TREE_H
#define RCPP_IMPTREE_TREE_H


#include <Rcpp.h>
#include "node.h"
#include "enums.h"
#include "utils.h"
using namespace Rcpp;

class Node;

struct Config {
  double s;
  double gamma;
  int minbucket;
  int maxdepth;
  EntropyCorrection::Enum ec;
  SplitMetric::Enum sm;
  IpType::Enum ip;
};
struct Data {
  IntegerMatrix data;
  IntegerVector nlevels;
  int classidx;
};

class Iptree {
private:
  Node* root;
  Config cfg;
  Data data;
public:
  inline Config* getConfig() {return &cfg;}
  inline Data* getData() {return &data;}
};

#endif /*RCPP_IMPTREE_TREE_H*/