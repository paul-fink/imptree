#include "node.h"

using namespace Rcpp;

Node::Node(Node* parent) : parent(parent)
{
}


IDMNode::IDMNode(Node *parent) : Node(parent) {
}

NumericVector Node::calcTValue(IntegerVector classvals, IntegerMatrix matx, std::vector<int> vidx, double gamma, EntropyCorrection ec, double s, bool exact) {
  
  ProbInterval rprobi = probabilityInterval(classvals, s);
  double maxEntBase = correctionEntropy(maxEntropy(rprobi, exact), rprobi.obs, ec, s);
  double minEntBase = correctionEntropy(minEntropy(rprobi), rprobi.obs, ec, s);
  double maxEntPoss = correctionEntropy(rep((1.0 / rprobi.lower.size()) , rprobi.lower.size()), rprobi.obs, ec, s);
  
  NumericVector tvalues(vidx.size());
  for(int i = 0; i < vidx.size(); ++i) {
    IntegerMatrix::Column col = matx(_, vidx.at(i));
    IntegerVector uc = unique(col);
    NumericVector maxEnt(uc.size());
    NumericVector minEnt(uc.size());
    for(int j = 0; j < uc.size(); ++j) {
      IntegerVector vals;
      for(int k = 0, r = 0; k < col.size(); ++k) {
        if(col[k] == uc[j]) {
          vals[r++] = classvals[k];
        }
      }
      ProbInterval lprobi = probabilityInterval(vals, s);
      maxEnt[j] = correctionEntropy(maxEntropy(lprobi, exact), lprobi.obs, ec, s) * lprobi.obs / rprobi.obs;
      minEnt[j] = correctionEntropy(minEntropy(lprobi), lprobi.obs, ec, s) * lprobi.obs / rprobi.obs;
    }
    double lowerEnt = sum(minEnt);
    double upperEnt = sum(maxEnt);
    tvalues[i] = calcT(upperEnt, lowerEnt, maxEntBase, minEntBase, maxEntPoss, gamma);
  };
  
  return tvalues;
}