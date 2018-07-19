#include <Rcpp.h>
#include "tree.h"
#include "evaluation.h"

// [[Rcpp::export]]
Rcpp::XPtr<Iptree> treebuilder_cpp(const Rcpp::IntegerMatrix & data, const Rcpp::List & config) {
  Iptree * tree = new Iptree(data, config);
  tree -> growTree();
  
  Rcpp::XPtr<Iptree> ptree( tree, true );
  return ptree;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix traindata_cpp(Rcpp::XPtr<Iptree> ptree) {
  return ptree->getData()->data;
}

// [[Rcpp::export]]
bool hasRoot_cpp(Rcpp::XPtr<Iptree> ptree) {
  return ptree->hasRoot();
}

// [[Rcpp::export]]
Rcpp::List predict_cpp(Rcpp::XPtr<Iptree> ptree, 
                   const Rcpp::IntegerMatrix & newdata,
                   const Rcpp::List & evalconfig) {
  
  Evaluation evalresult = ptree->evaluate(newdata, evalconfig);
  
  return Rcpp::List::create(Rcpp::Named("probintlist", evalresult.probIntervalList()),
                            Rcpp::Named("classes", evalresult.predictions()),
                            Rcpp::Named("evaluation", evalresult.summary())
  );
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix treeInformation_cpp(Rcpp::XPtr<Iptree> ptree) {
  
  Rcpp::IntegerVector tmpres = Rcpp::IntegerVector(3);
  tmpres[0] = ptree->getDepth();
  tmpres[1] = ptree->getNumberofLeaves();
  tmpres[2] = ptree->getNumberOfNodes();
  Rcpp::IntegerMatrix result = Rcpp::IntegerMatrix(3,1, tmpres.begin());
  Rcpp::colnames(result) = Rcpp::CharacterVector::create("Depth", "No. Leaves", "No. Nodes");
  return result;
}
