#include "utils.h"

/* fuctions to be called from R*/
void maxEntropyNPIa(int[], int*, double[]);
void maxEntropyNPIe(int[], int*, double[]);
void minEntropyNPIa(int[], int*, double[]);

/* helper functions */
void getKs(int[], int[], int, int);

/*
*
*  Function bodies follow
*
*/


/* FROM HERE ARE THE FUNCTIONS DEFINED*/

void getKs(int *karray, int *obs, int dim, int nobs) {

	int i;  
	for( i = 0; i < dim; i++) {
		++karray[obs[i]];
	}
}

void maxEntropyNPIa(int *obs, int *dim, double *prob) {
	
	int i, ni, j, kj, kj1;
	int k0, k1, K_s = 0;
	int K = *dim;
	int nobs = iArraySum(obs, K);
	int nobs1 = nobs+1;
	int karray[nobs1];
	double mass;
	
	for(i = 0; i < nobs1; i++) { karray[i] = 0; }
	getKs(karray, obs, K, nobs1);
	k0 = karray[0];
	k1 = karray[1];
	K_s = K - k0 - k1;
	
	if( K_s < k0) {
		for(i = 0; i < K; i++) {
			ni = obs[i];
			if(ni == 0 || ni == 1) {
				prob[i] = ((K_s + (double)k1) / (nobs * (k0 + k1)));
			} else {
				prob[i] = ((ni - 1.0) / nobs);
			}
		}
	} else {
		mass = K_s - k0;
		for(i = 0; i < K; i++) {
			ni = obs[i];
			if(ni == 0 || ni == 1) {
				prob[i] = (1.0/nobs);
			} else {
				prob[i] = ((ni - 1.0) / nobs);
			}
		}
		j = 1;
		while (fcmp(mass, 0.0) > 0) {
			
			kj = karray[j];
			kj1 = karray[j + 1];
			if((kj + kj1) < mass) {
				for(i = 0; i < K; i++){
					ni = obs[i];
					if(ni == j || ni == j + 1) {
						prob[i] += 1.0/nobs;
						mass--;
					}
				}
			} else {
				for(i = 0; i < K; i++) {
					ni = obs[i];
					if(ni == j || ni == j + 1) {
						prob[i] += (mass/(nobs * (kj + kj1)));
					}
				}
				mass = 0.0;
			}
			j++;
		}
	}
}

void maxEntropyNPIe(int *obs, int *dim, double *prob) {

	int i, beta, h, ni, j; /*, kj, kj1; */
	int k0, k1, k01, K_s = 0;
	int K = *dim;
	int nobs = iArraySum(obs, K);
	int nobs1 = nobs+1;
	int karray[nobs1];
	int order[K];
	double mass, o, W, Acc, obsd[K];
	
	for(i = 0; i < K; i++) { 
		order[i] = i;
		o = obs[i] + 0.0;
		obsd[i] = o;
	}
	for(i = 0; i < nobs1; i++) { karray[i] = 0; }
	getKs(karray, obs, K, nobs1);
	k0 = karray[0];
	k1 = karray[1];
	k01 = k0 + k1;
	K_s = K - k01;
	if(K_s == 0) {
		for(i = 0; i < K; i++) {
			prob[i] = (1.0/K);
		}
	} else if (k0 > K_s) {
		beta = (k01)/(K_s + k1);
		h = (k01) - beta * (K_s + k1);
		rsort_with_index(obsd, order, K);

		if( h < (k1 + 1)) {
			for(i = 0; i < K; i++) {
				if(i < (beta * (K_s - 1))) {
					prob[i] = 1.0/(nobs * beta);
				} else if(i < k01) {
					prob[i] = (k1 + 1.0)/(nobs * (beta * (k1 + 1.0) + h));
				} else {
					prob[i] = (obsd[i]-1.0)/nobs;
				}
			}	
		} else {
			for(i = 0; i < K; i++) {
				if(i < (h * (beta + 1))) {
					prob[i] = 1.0/(nobs * (beta + 1.0));
				} else if(i < k01) {
					prob[i] = 1.0/(nobs * beta);
				} else {
					prob[i] = (obsd[i]-1.0)/nobs;
				}
			}
		}
		re_sort_by_index(prob, order, K);
	} else {
		mass = K_s - k0;
		for(i = 0; i < K; i++) {
			ni = obs[i];
			if(ni == 1 || ni == 0) {
				prob[i] = 1.0/nobs;
			} else {
				prob[i] = (ni-1.0)/nobs;
			}
		}
		j = 1;
		while(fcmp(mass, 0.0) > 0) {
			if(karray[j]+karray[j+1] < mass) {
				for(i = 0; i < K; i++) {
					ni = obs[i];
					if((ni == j) || (ni == (j + 1))) {
						prob[i] += 1.0/nobs;
					}
				}
				mass -= (karray[j] + karray[j + 1]);
			} else {
				W = fmin2((mass + 1 + karray[j]), (karray[j] + karray[j + 1]));
				Acc = W;
				for(i = 0; i < K; i++){
					ni = obs[i];
					if(((ni == j) || (ni == (j + 1))) && fcmp(Acc, 0) > 0) {
						prob[i] += (mass/(nobs * W));
						Acc--;
					}
				}
				mass = 0.0;
			}
			j++;
		}
	}
}

void minEntropyNPIa(int *obs, int *dim, double *prob) {
	
	int i, ni, idx;
	int K = (int) *dim;
	int nobs = iArraySum(obs, K);
	int set[K], upper[K], lower[K];
	int mass, diff;
	
	for(i = 0; i < K; i++) {
		set[i] = 1;
		ni = obs[i];
		if(ni == nobs) {
			upper[i] = nobs;
			lower[i] = nobs - 1;
 		} else {
			upper[i] = ni + 1;
			if(ni == 0) {
				lower[i] = 0;		
			} else {
				lower[i] = ni - 1;
			}			
		}
	}
	mass = nobs - iArraySum(lower, K);
	for(i = 0; i < (K + 1); i++) {
		if(mass > 0) {
			idx = maxIndexInSet(lower, set, K);
			diff = upper[idx] - lower[idx];
			if(diff < mass) {
				lower[idx] = upper[idx];
				set[idx] = 0;
				mass -= diff;
			} else {
				lower[idx] = lower[idx] + mass;
				mass = 0;
			}
/* Trigger a warning if not all mass assigend after all iteration steps */	
			if(i == K && mass > 0) {
				warning("After all iterations (%i) not all mass has been assigned!\n Remaining mass is: %f\n", K, mass/(nobs * 1.0));	
			}
		} else {
			i = K;
		}

	}
	for(i = 0; i < K; i++) {
		prob[i] = lower[i] / (nobs * 1.0);
	}
}


/* function to calculate the corrected entropy */

