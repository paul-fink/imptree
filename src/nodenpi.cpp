#include "node.h"

NPINode::NPINode(Iptree* tree, int depth, Node* parent) : Node(tree, depth, parent)
{
}

/*
 ** get the index with the (first) maximum value
 ** in an array based on a selection defined in set
 */
int NPINode::maxIndexInSet(Rcpp::NumericVector array, Rcpp::LogicalVector set) {
  
  int index = -1;
  int max = -1;
  
  for(int i = 0; i < array.size(); i++) {
    if(set[i] && array[i] > max) {
      max = array[i];
      index = i;
    }
  }
  return index;
}

Rcpp::NumericVector NPINode::maxEntropy(const ProbInterval &probint, const bool exact) {
  if(exact) return maxEntropyExact(probint);
  return maxEntropyApprox(probint);
}


Rcpp::NumericVector NPINode::maxEntropyApprox(const ProbInterval &probint) {
	
	Rcpp::IntegerVector freq(probint.freq);
  int ksize = probint.freq.size();
  Rcpp::NumericVector prob(ksize, 0.0);
  Rcpp::IntegerVector ks(ksize, 0);
  for(int i = 0; i < ksize; ++i) {
    ++ks[freq[i]];
  }
  int k0 = ks[0];
  int k1 = ks[1];
  int krem = ksize - k0 - k1;
  int mass = 0;

	int nobs = probint.obs;

	int ni;
	if( krem < k0) {
		for(int i = 0; i < ksize; ++i) {
			ni = freq[i];
			if(ni == 0 || ni == 1) {
				prob[i] = ((krem + (double)k1) / (nobs * (k0 + k1)));
			} else {
				prob[i] = ((ni - 1.0) / nobs);
			}
		}
	} else {
		mass = krem - k0;
	  prob = pmax(freq - 1.0, 1.0) / nobs;
	  
		int j = 1, kj, kj1;
		while (mass > 0) {
			
			kj = ks[j];
			kj1 = ks[j + 1];
			if((kj + kj1) < mass) {
				for(int i = 0; i < ksize; ++i){
					ni = freq[i];
					if(ni == j || ni == j + 1) {
						prob[i] += 1.0/nobs;
						mass--;
					}
				}
			} else {
				for(int i = 0; i < ksize; ++i) {
					ni = freq[i];
					if(ni == j || ni == j + 1) {
						prob[i] += (mass/((double)nobs * (kj + kj1)));
					}
				}
				mass = 0;
			}
			++j;
			if(j == ksize) {
			  Rcpp::warning("After all iterations (%i) not all mass has been assigned!\n Remaining mass is: %f\n", ksize, mass / (nobs * 1.0));	
			  break;
			} 
		}
	}
	return prob;
}

Rcpp::NumericVector NPINode::maxEntropyExact(const ProbInterval &probint) {

  Rcpp::IntegerVector freq(probint.freq);
  int ksize = probint.freq.size();
  
  Rcpp::IntegerVector ks(ksize, 0);
  Rcpp::NumericVector prob(ksize, 0.0);
  
  for(int i = 0; i < ksize; ++i) {
    ++ks[freq[i]];
  }
  int k0 = ks[0];
  int k1 = ks[1];
  int k01 = k0 + k1;
  int krem = ksize - k01;
	
	int nobs = probint.obs;
	

	if(krem == 0) {
	  prob = 1.0 / ksize;
	  return prob;
	} else if (k0 > krem) {
	  
    prob = (freq - 1.0) / nobs;
		int beta = (k01) / (krem + k1);
		int h = (k01) % (krem + k1);
		
		if( h < (k1 + 1)) {
			for(int i = 0; i < ksize; ++i) {
			  int j = 0;
			  if(freq[i] <= 1) {
				  if(j < (beta * (krem - 1))) {
					  prob[i] = 1.0 / (nobs * beta);
				    ++j;
				  } else if(j < k01) {
					  prob[i] = (k1 + 1.0) / (nobs * (beta * (k1 + 1.0) + h));
				    ++j;
				  } else {
				    Rcpp::warning("Something is wrong in calculation");
				  }
				}
			}	
		} else {
			for(int i = 0; i < ksize; ++i) {
			  int j = 0;
			  if(freq[i] <= 1) {
				  if(j < (h * (beta + 1))) {
					  prob[i] = 1.0 / (nobs * (beta + 1.0));
				    ++j;
				  } else if(j < k01) {
					  prob[i] = 1.0 / (nobs * beta);
				    ++j;
				  } else {
				    Rcpp::warning("Something is wrong in calculation");
				  }
				}
			}
		}
		return prob;
	} else {
		int mass = krem - k0;
		prob = Rcpp::pmax(freq - 1.0, 1.0) / nobs;
		
		int j = 1, Acc, W, ni;
		while(mass > 0) {
			if(ks[j] + ks[j + 1] < mass) {
				for(int i = 0; i < ksize; ++i) {
					ni = freq[i];
					if((ni == j) || (ni == (j + 1))) {
						prob[i] += 1.0 / nobs;
					}
				}
				mass -= (ks[j] + ks[j + 1]);
			} else {
				W = min(Rcpp::IntegerVector::create((mass + 1 + ks[j]), (ks[j] + ks[j + 1])));
				Acc = W;
				for(int i = 0; i < ksize; ++i){
					ni = freq[i];
					if(((ni == j) || (ni == (j + 1))) && Acc > 0) {
						prob[i] += (mass / (nobs * (double)W));
						--Acc;
					}
				}
				mass = 0.0;
			}
			++j;
			if(j == ksize) {
			  Rcpp::warning("After all iterations (%i) not all mass has been assigned!\n Remaining mass is: %f\n", ksize, mass / (nobs * 1.0));	
			  break;
			}
		}
		return prob;
	}
	return Rcpp::NumericVector();
}

Rcpp::NumericVector NPINode::minEntropy(const ProbInterval &probint) {
	
	int ksize = probint.freq.size();
	int nobs = probint.obs;
	Rcpp::NumericVector lower(ksize);
	Rcpp::NumericVector upper(ksize);
	
	lower = Rcpp::pmax(probint.freq - 1, 0);
	upper = Rcpp::pmin(probint.freq + 1, 1);
	
	Rcpp::LogicalVector set(ksize, true);
	
	int diff, idx, j, mass = nobs - sum(lower);
	
	while(mass > 0) {
		idx = maxIndexInSet(lower, set);
		diff = upper[idx] - lower[idx];
		if(diff < mass) {
			lower[idx] = upper[idx];
			set[idx] = false;
			mass -= diff;
		} else {
			lower[idx] = lower[idx] + mass;
			mass = 0;
		}
		++j;
		if(j == ksize) {
		  Rcpp::warning("After all iterations (%i) not all mass has been assigned!\n Remaining mass is: %f\n", ksize, mass / (nobs * 1.0));	
		  break;
		}
	}
  return lower / nobs;
}

double NPINode::correctionEntropy(Rcpp::NumericVector probs, const int n) {
  if(n > 0) {
    double ent = entropy(probs);
    EntropyCorrection ec = tree_->getConfig()->ec;
    switch(ec) {
    case EntropyCorrection::strobl:
      ent += ((probs.size() - 1) / (2 * n));
      break;
    default:;
    }
    return ent;
  }
  return -1;
}


ProbInterval NPINode::probabilityInterval(Rcpp::IntegerVector observations) {
  Rcpp::IntegerVector frequency = Rcpp::table(observations);
  ProbInterval prob;
  Rcpp::NumericVector gupper(frequency);
  Rcpp::NumericVector glower(frequency);
  prob.obs = sum(frequency);
  prob.freq = frequency;
  prob.upper = Rcpp::pmin(gupper + 1.0, 1.0) / (prob.obs);
  prob.lower = Rcpp::pmax(glower - 1.0, 0.0) / (prob.obs);
  return prob;
}
