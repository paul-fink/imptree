#include <Rcpp.h>
#include "translation.h"
#include "node.h"
#include "structs.h"
#include "evaluation.h"

// [[Rcpp::export]]
Rcpp::XPtr<Node> treebuilder_cpp(const Rcpp::IntegerMatrix & data, const Rcpp::List & config) {
  
  std::shared_ptr<Data> datap = std::make_shared<Data>(data);
  std::shared_ptr<Config> configp = std::make_shared<Config>();
  configp->s = Rcpp::as<double>(config["s"]);
  configp->gamma = Rcpp::as<double>(config["gamma"]);
  configp->tbase = Rcpp::as<double>(config["tbase"]);
  configp->minbucket = Rcpp::as<int>(config["minbucket"]);
  configp->maxdepth = Rcpp::as<int>(config["depth"]);
  configp->ec = static_cast<EntropyCorrection>(Rcpp::as<int>(config["correction"]));
  configp->sm = static_cast<SplitMetric>(Rcpp::as<int>(config["splitmetric"]));
  configp->ip = static_cast<IpType>(Rcpp::as<int>(config["iptype"]));
  
  Node* root = Node::createNode(datap, configp, 0, 0);
  std::vector<int> splitset;
  for(int i = 0; i < data.ncol(); ++i) {
    if(i != datap->classidx) {
      splitset.push_back(i);
    }
  }
  root->setSplitSet(splitset);
  for(int i = 0; i < data.nrow(); ++i) {
    root->addSplitObs(i);
  }
  root->makeChildren();
  
  Rcpp::XPtr<Node> proot( root, true );
  return proot;
}

// [[Rcpp::export]]
bool hasRoot_cpp(Rcpp::XPtr<Node> prootnode) {
  return !prootnode->hasParent();
}

// [[Rcpp::export]]
Rcpp::List predict_cpp(Rcpp::XPtr<Node> prootnode, 
                   const Rcpp::IntegerMatrix & newdata,
                   const Rcpp::List & evalconfig) {
  
  Evaluation evalresult = prootnode->evaluate(newdata, evalconfig);
  
  return Rcpp::List::create(Rcpp::Named("probintlist") = evalresult.probIntervalList(),
                            Rcpp::Named("classes") = evalresult.predictions(),
                            Rcpp::Named("evaluation") = evalresult.summary()
  );
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix treeInformation_cpp(Rcpp::XPtr<Node> prootnode) {
  
  Rcpp::IntegerVector tmpres = Rcpp::IntegerVector(3);
  std::vector<int> * depths = new std::vector<int>();
  prootnode->addDepth(depths);
  std::vector<int>::iterator mresult = std::max_element(depths->begin(), depths->end());
  tmpres[0] = *mresult;
  delete depths;
  tmpres[1] = prootnode->numLeaves();
  tmpres[2] = prootnode->numNodes();
  
  Rcpp::IntegerMatrix result = Rcpp::IntegerMatrix(3,1, tmpres.begin());
  Rcpp::rownames(result) = Rcpp::CharacterVector::create("Depth", "No. Leaves", "No. Nodes");
  return result;
}

// [[Rcpp::export]]
void treePrint_cpp(Rcpp::XPtr<Node> prootnode, const int nsmall, const std::string &sep) {
   prootnode->printNode(-1, nsmall, sep);
}

// [[Rcpp::export]]
Rcpp::List getNode_cpp(Rcpp::XPtr<Node> prootnode, Rcpp::IntegerVector idxs) {
  
  std::vector<int> stdidxs = Rcpp::as< std::vector<int> >(idxs);
  std::reverse(stdidxs.begin(), stdidxs.end());
  //int idxvback = stdidxs.back();
  stdidxs.pop_back();
  return prootnode->getNodeByIndex(stdidxs);
}