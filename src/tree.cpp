// [[Rcpp::plugins(cpp11)]]
#include "tree.h"

Iptree::Iptree(const Rcpp::IntegerMatrix & data, const Rcpp::List & config)
{
  // Init the data
  data_ = new Data(data);
  
  cfg_->s = Rcpp::as<double>(config["s"]);
  cfg_->gamma = Rcpp::as<double>(config["gamma"]);
  cfg_->tbase = Rcpp::as<double>(config["tbase"]);
  cfg_->minbucket = Rcpp::as<int>(config["minbucket"]);
  cfg_->maxdepth = Rcpp::as<int>(config["maxdepth"]);
  cfg_->ec = static_cast<EntropyCorrection>(Rcpp::as<int>(config["entropycorrection"]));
  cfg_->sm = static_cast<SplitMetric>(Rcpp::as<int>(config["splitmetric"]));
  cfg_->ip = static_cast<IpType>(Rcpp::as<int>(config["iptype"]));
}

Iptree::~Iptree()
{
  if(root_) delete root_;
  if(data_) delete data_;
}

void Iptree::growTree() {
  
  // If root node already exists, do nothing
  if(root_) {
    return; 
  }
  
  // create new root
  root_ = Node::createNode(getConfig()->ip, this, 0, 0);
  // grow tree
  root_->makeChildren();
}


Evaluation Iptree::evaluate(const Rcpp::IntegerMatrix & newdata, const Rcpp::List & evalconfig) {
  
  int nObs = newdata.rows();
  std::vector<ProbInterval> probInts = std::vector<ProbInterval>(nObs);
  
  for(int i = 0; i < nObs; ++i) {
    Rcpp::IntegerMatrix::ConstRow row = newdata.row(i);
    probInts.push_back(root_->classify(row));
  }
  double utility = Rcpp::as<double>(evalconfig["utility"]);
  Dominance dom = static_cast<Dominance>(Rcpp::as<int>(evalconfig["dominance"]));
  Data observation = Data(newdata);
  
  Evaluation eval = Evaluation(utility, dom, probInts, newdata);
  return eval;
}

/**
 * Number of nodes in tree
 * @return positive int with number of nodes in full tree structure
 */
int Iptree::getNumberOfNodes() const {
  if(nullptr == root_) {
    return 0;
  }
  return root_->numNodes();
}

/**
 * Number of leaves in tree
 * @return positive int of number of leaves in full tree structure
 */
int Iptree::getNumberofLeaves() const {
  if(nullptr == root_) {
    return 0;
  }
  return root_->numLeaves();
}

/**
 * Depth of tree
 *
 * @return maximal depth of tree
 */
int Iptree::getDepth() const {
  if(nullptr == root_) {
    return 0;
  }
  std::vector<int> * depths = new std::vector<int>();
  root_->addDepth(depths);
  std::vector<int>::iterator result = std::max_element(depths->begin(), depths->end());
  
  return *result;
}

void Iptree::printTree(const int nsmall, const std::string &sep) const {
  if(nullptr == root_) {
    Rcpp::Rcout << "Tree does not have a root node";
  }
  this->root_->printNode(-1, nsmall, sep);
  
}

