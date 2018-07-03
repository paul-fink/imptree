#include "node.h"

using namespace Rcpp;

Node::Node(Node* parent) : parent(parent)
{
}

NumericVector Node::calcTValue(IntegerVector classvals, IntegerMatrix matx, std::vector<int> vidx, double gamma, EntropyCorrection ec, bool exact) {
  
  ProbInterval rprobi = probabilityInterval(classvals);
  double maxEntBase = correctionEntropy(maxEntropy(rprobi, exact), rprobi.obs, ec);
  double minEntBase = correctionEntropy(minEntropy(rprobi), rprobi.obs, ec);
  double maxEntPoss = correctionEntropy(rep((1.0 / rprobi.lower.size()) , rprobi.lower.size()), rprobi.obs, ec);
  
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
      ProbInterval lprobi = probabilityInterval(vals);
      maxEnt[j] = correctionEntropy(maxEntropy(lprobi, exact), lprobi.obs, ec) * lprobi.obs / rprobi.obs;
      minEnt[j] = correctionEntropy(minEntropy(lprobi), lprobi.obs, ec) * lprobi.obs / rprobi.obs;
    }
    double lowerEnt = sum(minEnt);
    double upperEnt = sum(maxEnt);
    tvalues[i] = calcT(upperEnt, lowerEnt, maxEntBase, minEntBase, maxEntPoss, gamma);
  };
  
  return tvalues;
}