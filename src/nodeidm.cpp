#include "node.h"

IDMNode::IDMNode(Iptree* tree, int depth, Node* parent) : Node(tree, depth, parent)
{
  s_ = tree_->getConfig() -> s;
}

Rcpp::IntegerVector IDMNode::minInSet(Rcpp::NumericVector array, Rcpp::LogicalVector set) {
	
/* Initianlizing the minimal values with a not reachable one in the array */
  int nmin = 0;
  int imin1 = -1; double min1 = 2.0; 
  int imin2 = -1; double min2 = 2.0;
	bool samemin = true;
	for (int i = 0 ; i < array.size() ; ++i ) {
/* Only considering those which are allowed according to the set */
		if(set[i]) {
/* In case a new minimum is identified, the old minimum in 'min1'
** is passed to 'min2' and 'min1' gets the new one */
			if ( fcmp(array[i], min1) < 0) {
				min2 = min1;
				min1 = array[i];
				nmin = 1;
				imin2 = imin1;
				imin1 = i;
			} else if( fcmp(array[i], min1) == 0) {
			  ++nmin;
			} else if( fcmp(array[i], min2) < 0) {
/* In case a the value is greater than the minimum, 
** but smaller than the second minimal value, then 
** it is assigned to 'min2' */
				min2 = array[i];
			  imin2 = i;
			  samemin = false;
			}
		}
	}
	if(samemin) {
	  imin2 = imin1;
	}
/* return the minimum and second minimal */
  return Rcpp::IntegerVector::create(
    Rcpp::Named("min1", imin1), 
    Rcpp::Named("min2", imin2),
    Rcpp::Named("nmin", nmin)
	);
}


/* Functions called by R, see description above */

Rcpp::NumericVector IDMNode::maxEntropy(const ProbInterval &probint, const bool /*exact*/) {
  
  Rcpp::NumericVector lower(clone(probint.lower));
  Rcpp::NumericVector upper(probint.upper);
  int lsize = lower.size();
  Rcpp::LogicalVector set(lsize, true);
	double minv;
	int nmin, imin, ismin;
	
// Generating a set of index which shall be considered for adjustment:
// Only those may be adjusted, where the lower < upper
	for (int i = 0; i < lsize; ++i) {
		if(fcmp(lower[i], upper[i]) == 0) {
			set[i] = false;
		}
	}
	
  // Summing up all lower values
	double suml = Rcpp::sum(lower);
  // we only proceed into the adjustment, when the sum is lower than 1,
  // i.e. not allready a probability distribution
	while (fcmp(suml, 1.0) < 0) {

    // obtainig the minimal and second minimal value, as well as the
    // number of times the minimal value is attained
    Rcpp::IntegerVector mins = minInSet(lower, set);
		imin = mins["min1"];
		ismin = mins["min2"];
		nmin = mins["nmin"];
		minv = lower[imin];

		for (int j = 0 ; j < lsize; ++j ) {
      // adjustment takes place only for minimal values
			if(fcmp(lower[j], minv) == 0) {
			  
			  lower[j] += min(Rcpp::NumericVector::create((upper[j] - lower[j]),
                                           ((1.0 - suml) / nmin),
                                           ((ismin != imin) ? (lower[ismin] - minv) : 1.0) ));
        // adjusting the set
				if(fcmp(lower[j], upper[j]) == 0) {
					set[j] = false;
				}
			}
		}
		// updating the sum value
		suml = Rcpp::sum(lower);
	}
	return lower;
}


Rcpp::NumericVector IDMNode::minEntropy(const ProbInterval &probint) {

  Rcpp::NumericVector lower(Rcpp::clone(probint.lower));
  Rcpp::NumericVector upper(probint.upper);
  // get the index with the (first) maximum
	int index = Rcpp::which_max(lower);
  // if valid index then set the value of the lower boundary to the 
  // value of the upper one
	if ( index > -1 && index < lower.size() ) {
		lower[index] = upper[index];
	}
	return lower;
}

double IDMNode::correctionEntropy(Rcpp::NumericVector probs, const int n) {
  if(s_ > 0 && n > 0) {
    double ent = entropy(probs);
    EntropyCorrection ec = tree_->getConfig()->ec;
    switch(ec) {
    case EntropyCorrection::abellan:
      ent += (s_ * log2(probs.size())) / (n + s_);
      break;
    case EntropyCorrection::strobl:
      ent += ((probs.size() + 1) / (2 * n + s_));
      break;
    default:;
    }
    return ent;
  }
  return -1;
}

ProbInterval IDMNode::probabilityInterval(Rcpp::IntegerVector observations) {
  Rcpp::IntegerVector frequency = Rcpp::table(observations);
  ProbInterval prob;
  Rcpp::NumericVector gupper(frequency);
  Rcpp::NumericVector glower(frequency);
  prob.obs = Rcpp::sum(frequency);
  prob.freq = frequency;
  prob.upper = (gupper + s_) / (prob.obs + s_);
  prob.lower = glower / (prob.obs + s_);
  return prob;
}
