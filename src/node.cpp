#include "node.h"

Node::Node(Iptree* tree, int depth, Node* parent) : 
  parent_(parent), tree_(tree), depth_(depth)
{
}

Node::~Node() {
  for(uint i = 0; i < children_.size(); ++i) {
    delete (Node *) children_[i];
  }
}

Node* Node::createNode(IpType ipt, Iptree *tree, int depth, Node *parent) {
  switch(ipt) {
  case IpType::idm:
    return new IDMNode(tree, depth, parent);
  case IpType::npi:
  case IpType::npiapprox:
    return new NPINode(tree, depth, parent);
  default:
    Rcpp::warning("Other IPType is not implemented");
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

  // Abandon if there are already children present or max depth was reached
  if(children_.empty() || depth_ == tree_->getConfig()->maxdepth) {
    return; 
  }
  
  // Calculate the index of the splitVariable
  int splitIdx = calcSplitVariable();
  
  // If there is none found, abandon
  if(splitIdx < 0) {
    return;
  }
  // set the splitvarindex and remove from the list of possible split vars
  setSplitVariable(splitIdx);
  
  // Get all possible split points
  int childs = tree_->getData()->nlevels[splitvaridx_];
  // Create all childs
  for(uint i = 0; i < childs; ++i) {
    Node* child = Node::createNode(tree_->getConfig()->ip,tree_, ++depth_, this);
    children_.push_back(child);
  }
  
  // Assign the observations to the childs
  for(uint i = 0; i < obsidxs_.size(); ++i) {
    int obsidx = obsidxs_[i];
    int splitval = tree_->getData()->data(obsidx, splitvaridx_);
    getChild(splitval)->addSplitObs(obsidx);

  }
  // Go into recursion
  for(uint i = 0; i < size(); ++i) {
    getChild(i)->makeChildren();
  }
}

Rcpp::IntegerVector Node::getNodeObservations(const int variableIndex) {
  Rcpp::IntegerMatrix allObsAllVar = tree_->getData()->data;
  Rcpp::IntegerVector allObsByVar = tree_->getData()->data(Rcpp::_,variableIndex);
  Rcpp::IntegerVector obsidxRcpp = Rcpp::wrap(obsidxs_);
  Rcpp::IntegerVector nodeObsByVar = allObsByVar[obsidxRcpp];
  return nodeObsByVar;
}

int Node::calcSplitVariable() {
  
  // access all config variables
  double gamma = tree_->getConfig()->gamma;
  int minbucket = tree_->getConfig()->minbucket;
  bool exact = (tree_->getConfig()->ip != IpType::npiapprox);
  
  // calculate the entropies in the root node
  Rcpp::IntegerVector classvals = getNodeObservations(tree_->getData()->classidx);
  probInt_ = probabilityInterval(classvals);
  double maxEntBase = correctionEntropy(maxEntropy(probInt_, exact), probInt_.obs);
  double minEntBase = correctionEntropy(minEntropy(probInt_), probInt_.obs);
  double maxEntPoss = correctionEntropy(Rcpp::rep((1.0 / probInt_.lower.size()) , 
                                                  probInt_.lower.size()), probInt_.obs);
  
  // Number of levels
  int splitVarListSize = splitset_.size();
  
  // Initialise vectors
  Rcpp::NumericVector tvalues(splitVarListSize);
  std::vector<int> toRemoveFromVarList = std::vector<int>();
  
  for(uint i = 0; i < splitVarListSize; ++i) {
  
    // Index of split variable (candidate)
    int splitVarIndex = splitset_[i];
    
    // Observations for that variable in the node
    Rcpp::IntegerVector col = getNodeObservations(splitVarIndex);
    
    // Unique values
    Rcpp::IntegerVector uc = unique(col);
    // number of levels of variable
    int splitVarNlevels = tree_->getData()->nlevels[splitVarIndex];
    
    // if not all levels are present, then variable is
    // no longer a split candidate and added to 
    // list to remove from further candidates
    if(uc.size() < splitVarNlevels) {
      toRemoveFromVarList.push_back(splitVarIndex);
      tvalues.insert(i, R_PosInf);
      continue;
    }
    
    // Initialise vectors for entropies in subnodes
    Rcpp::NumericVector maxEnt(splitVarNlevels);
    Rcpp::NumericVector minEnt(splitVarNlevels);
    Rcpp::IntegerVector splitVarLevels = Rcpp::seq_len(splitVarNlevels) - 1;
    
    // Bool storing if sufficient obs are in subnodes
    bool sufficientObs = true;
    // Iterate over all subnodes
    for(int j = 0; j < splitVarNlevels; ++j) {
      // create the vector of class values in subnode
      Rcpp::IntegerVector vals;
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
  if(!toRemoveFromVarList.empty()) {
    std::vector<int> splitset_new(splitset_.size());
    std::vector<int>::iterator it = std::set_difference (splitset_.begin(), splitset_.end(),
                                                         toRemoveFromVarList.begin(), toRemoveFromVarList.end(),
                                                         splitset_new.begin());
    splitset_new.resize(it-splitset_new.begin());
    splitset_ = splitset_new;
  }
 
  // get the minimal t-value 
  double minTvalue = min(tvalues);
  int splitIdx = -1;
  // do nothing if minimal t-value is in range of nogo-split
  if (minTvalue < tree_->getConfig()->tbase) {
    // get the index with lowest t-value; draw one randomly if multiple
    Rcpp::IntegerVector ids = seq_along(tvalues) - 1;
    Rcpp::IntegerVector minids = ids[tvalues == minTvalue];
    splitIdx = sample(minids, 1)[0];
  }
  
  return splitIdx;
}

ProbInterval Node::classify(Rcpp::IntegerVector observation) {
  if(splitvaridx_ < 0) {
    return probInt_;
  }
  return children_.at(observation[splitvaridx_])->classify(observation);
}

/**
 * Returns number of leaves in tree structure below node
 *
 * @return number of leaves as positive int
 */
int Node::numLeaves() const {
  
  int num = 0;
  if (splitvaridx_ < 0) {
    return 1;
  } else {
    for (Node * child : children_) {
      num += child->numLeaves();
    }
  }
  return num;
}

/**
 * Returns number of nodes in tree structure below node
 *
 * @return number of nodes as positive int
 */
int Node::numNodes() const {
  
  int no = 1;
  if (splitvaridx_ > -1) {
    for (Node * child : children_) {
      no += child->numNodes();
    }
  }
  return no;
}


void Node::addDepth(std::vector<int> * depths) const {
  
  if(splitvaridx_ > -1) {
    for (Node * child : children_) {
      child->addDepth(depths);
    }
  } else {
    depths->push_back(depth_);
  }
}


void Node::printNode(int parentIdx, const int nsmall, const std::string & sep) const {
  
  int nodedepth = depth_;
  
  while(nodedepth > 0) {
    Rcpp::Rcout << "  ";
    --nodedepth;
  }
  //Print the depth
  Rcpp::Rcout << "(" << depth_ << ") ";
  
  // Print type of node
  if(nullptr == parent_) {
    Rcpp::Rcout << "root: ";
  } else {
    int parent_splitIdx = parent_->splitvaridx_;
    Rcpp::CharacterVector parent_labels = Rcpp::as<Rcpp::CharacterVector>(tree_->getData()->labels.at(parent_splitIdx));
    Rcpp::Rcout << tree_->getData()->varnames[parent_splitIdx] << "=" << parent_labels[parentIdx] << ": ";
  }
  
  //Print size of observations and probabilities
  Rcpp::Rcout << "n=" << obsidxs_.size() << " (" << probInt_.to_string(nsmall, sep) << ")";
  
  // Print end of line (stared for leave) and recurse
  if(splitvaridx_ > -1) {
    Rcpp::Rcout << std::endl;
    for (size_t i = 0; i < size(); ++i) {
      children_[i]->printNode(i, nsmall, sep);
    }
  } else {
    Rcpp::Rcout << " *" << std::endl;
  }
}

ProbInterval getIndexProbInterval(std::vector<int> idxs) {
  
  
  if(idxs.size() > 0) {
    return getIndexProbInterval(idxs);  
  }
}