#include "node.h"

using namespace Rcpp;

Node::Node(Iptree* tree, int depth, Node* parent) : parent_(parent), tree_(tree), depth_(depth), splitvaridx_(-1)
{
}

Node* Node::createNode(IpType::Enum ipt, Iptree *tree, int depth, Node *parent) {
  switch(ipt) {
  case IpType::idm:
    return new IDMNode(tree, depth, parent);
  case IpType::npi:
  case IpType::npiapprox:
    return new NPINode(tree, depth, parent);
  default:
    warning("Other IPType is not implemented");
  }
  return 0;
}

void Node::setSplitVariable(const int idx) {
  splitvaridx_ = idx;
  std::vector<int>::iterator position = std::find(splitset_.begin(), splitset_.end(), idx);
  if (position != splitset_.end()) {
    splitset_.erase(position);
  }
}

void Node::makeChildren() {
  if(children_.size() >0 || splitvaridx_ < 0) return;
  IntegerVector nlevels = tree_->getData()->data.attr("nlevels");
  int childs = nlevels[splitvaridx_];
  for(int i = 0; i < childs; ++i) {
    Node* child = Node::createNode(tree_->getConfig()->ip,tree_, ++depth_, this);
    children_.push_back(child);
  }
  for(int i=0; i < obsidxs_.size(); ++i) {
    int obsidx = obsidxs_.at(i);
    int splitval = tree_->getData()->data(obsidx, splitvaridx_);
    children_.at(splitval)->addSplitObs(obsidx);
  }
}

IntegerVector Node::getNodeObservations(const int variableIndex) {
  IntegerMatrix allObsAllVar = tree_->getData()->data;
  IntegerVector allObsByVar = allObsAllVar(_,variableIndex);
  IntegerVector obsidxRcpp = Rcpp::wrap(obsidxs_);
  IntegerVector nodeOnsByVar = allObsByVar[obsidxRcpp];
  return nodeOnsByVar;
}

NumericVector Node::calcTValue(std::vector<int> vidx) {
  
  // access all config variables
  double gamma = tree_->getConfig()->gamma;
  int minbucket = tree_->getConfig()->minbucket;
  bool exact = (tree_->getConfig()->ip != IpType::npiapprox);
  
  // calculate the entropies in the root node
  IntegerVector classvals = getNodeObservations(tree_->getData()->classidx);
  probInt_ = probabilityInterval(classvals);
  double maxEntBase = correctionEntropy(maxEntropy(probInt_, exact), probInt_.obs);
  double minEntBase = correctionEntropy(minEntropy(probInt_), probInt_.obs);
  double maxEntPoss = correctionEntropy(rep((1.0 / probInt_.lower.size()) , probInt_.lower.size()), probInt_.obs);
  
  // Number of levels
  IntegerVector nlevels = tree_->getData()->nlevels;
  int splitVarListSize = splitset_.size();
  
  // Initialise vectors
  NumericVector tvalues(splitVarListSize);
  std::vector<int> toRemoveFromVarList;
  
  for(int i = 0; i < splitVarListSize; ++i) {
  
    // Index of split variable (candidate)
    int splitVarIndex = splitset_.at(i);
    
    // Observations for that variable in the node
    IntegerVector col = getNodeObservations(splitVarIndex);
    
    // Unique values
    IntegerVector uc = unique(col);
    // number of levels of variable
    int splitVarNlevels = nlevels[splitVarIndex];
    
    // if not all levels are present, then variable is
    // no longer a split candidate and added to 
    // list to remove from further candidates
    if(uc.size() < splitVarNlevels) {
      toRemoveFromVarList.push_back(splitVarIndex);
      tvalues.insert(i, R_PosInf);
      continue;
    }
    
    // Initialise vectors for entropies in subnodes
    NumericVector maxEnt(splitVarNlevels);
    NumericVector minEnt(splitVarNlevels);
    IntegerVector splitVarLevels = seq_len(splitVarNlevels) - 1;
    
    // Bool storing if sufficient obs are in subnodes
    bool sufficientObs = true;
    // Iterate over all subnodes
    for(int j = 0; j < splitVarNlevels; ++j) {
      // create the vector of class values in subnode
      IntegerVector vals;
      for(int k = 0, r = -1; k < col.size(); ++k) {
        if(col[k] == splitVarLevels[j]) {
          vals.insert(++r, classvals[k]);
        }
      }
      // Disqualify variable as candidate if subnode has
      // too few observations: Added it to list to remove 
      // and signal to outer loop via 'sufficientObs'
      if(vals.size() < minbucket) {
        toRemoveFromVarList.push_back(splitVarIndex);
        sufficientObs = false;
        break;
      }
      // Perform entropy calculation in the subnode
      ProbInterval lprobi = probabilityInterval(vals);
      // weight the entropy values with the relative frequencies of falling into node
      maxEnt.insert(j, correctionEntropy(maxEntropy(lprobi, exact), lprobi.obs) * lprobi.obs / probInt_.obs);
      minEnt.insert(j, correctionEntropy(minEntropy(lprobi), lprobi.obs) * lprobi.obs / probInt_.obs);
    }
    
    // Proceed if still enough observations in subnode
    if(sufficientObs) {
      double lowerEnt = sum(minEnt);
      double upperEnt = sum(maxEnt);
      tvalues.insert(i, calcT(upperEnt, lowerEnt, maxEntBase, minEntBase, maxEntPoss, gamma));
    } else {
      tvalues.insert(i, R_PosInf);
    }
  }


  // Remove already disqualified variables
  if(toRemoveFromVarList.size() > 0) {
    std::vector<int> splitset_new(splitset_.size());
    std::vector<int>::iterator it = std::set_difference (splitset_.begin(), splitset_.end(),
                                                         toRemoveFromVarList.begin(), toRemoveFromVarList.end(),
                                                         splitset_new.begin());
    splitset_new.resize(it-splitset_new.begin());
    splitset_ = splitset_new;
  }
  
  // Obtain the index of lowest t-value (move next 5 lines to different function)
  double minTvalue = min(tvalues);
  IntegerVector ids = seq_along(tvalues) - 1;
  IntegerVector minids = ids[tvalues == minTvalue];
  int splitIdx = sample(minids, 1)[0];
  setSplitVariable(splitIdx);
  
  return tvalues;
}

