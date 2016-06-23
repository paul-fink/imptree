#include "utils.h"
#include "entropyNPI.c"

/* fuction to be called from R*/
void tests_R(int*, int*);

/* main function for testing */
int tests(int*, int*);

/* basic test functions */
int test_getKs(void);
int test_sort_resort_with_index(void);
int test_npi_a(void);
int test_npi_1(void);
int test_npi_2(void);
int test_npi_3(void);
int test_npi_4(void);
int test_npi_5(void);
int test_npi_6(void);

void tests_R(int *fails, int *all) {
	
	tests(fails, all);
	
}

int tests(int *fails, int *all) {
	
	int overall_tests = 0, failed_tests = 0;
	overall_tests++;
	failed_tests += test_getKs();
	overall_tests++;
	overall_tests++;
	failed_tests += test_sort_resort_with_index();
	overall_tests++;
	failed_tests += test_npi_a();
	overall_tests++;
	failed_tests += test_npi_1();
	overall_tests++;
	failed_tests += test_npi_2();
	overall_tests++;
	failed_tests += test_npi_3();
	overall_tests++;
	failed_tests += test_npi_4();
	overall_tests++;
	failed_tests += test_npi_5();
	overall_tests++;
	failed_tests += test_npi_5();
	overall_tests++;
	failed_tests += test_npi_6();
/* print results*/		
	if(failed_tests > 0) {
		error("YOU FAILED %i TEST(S)!!\n", failed_tests);
	}
	*fails = failed_tests;
	*all = overall_tests;
	return overall_tests;
}

int test_getKs(void) {
	int obs[7] = {0,3,0,2,3,0,0};
	int nobs = 8;
	int nobs_all = nobs+1;
	int true_ks[9] = {4,0,1,2,0,0,0,0,0};
	int dim = 7;	
	int htest = 0, test = 0;
	int i;
	int karray[nobs_all];
	for(i = 0; i < nobs_all; i++) {
		karray[i] = 0;
	}

	getKs(karray, obs, dim, nobs_all);

	for(i = 0; i < nobs_all; i++) {
		htest = abs(fcmp(karray[i], true_ks[i]));
		if(htest > 0) {
			warning("Values differ for 'getKs()' failed at index %i: expected %i, had %i\n", i+1, true_ks[i], karray[i]);
		}  
		test |= htest;    
	}       
	if (test > 0) {
		warning("test for 'getKs()' failed\n");
	}
	return test;  
}

int test_sort_resort_with_index(void) {
	int i;
	double v2o[6] = {2.0,1.3,2.1,4.5,1.0,1.0};
	double true_vr[6] = {2.0,1.3,2.1,4.5,1.0,1.0};
	double true_vo[6] = {1.0,1.0,1.3,2.0,2.1,4.5};
	int ind2o[6] = {0,1,2,3,4,5};
	int true_indo[6] = {4,5,1,0,2,3};
	int test = 0, htest = 0, tests = 0;

	rsort_with_index(v2o, ind2o, 6);
	for(i = 0; i < 6; i++) {
		htest = abs(fcmp(ind2o[i], true_indo[i]));
		if(htest > 0) {
			warning("Values differ for 'rsort_with_index()' failed at index %i: expected %i, had %i\n", i+1, true_indo[i], ind2o[i]);
		}
		test |= htest;
		htest = abs(fcmp(v2o[i], true_vo[i]));
		if(htest > 0) {
			warning("Values differ for 'rsort_with_index()' failed at index %i: expected %f, had %f\n", i+1, true_vo[i], v2o[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'rsort_with_index()' failed\n");
		tests++;
	}
	test = 0;
	re_sort_by_index(v2o, ind2o, 6);
	for(i = 0; i < 6; i++){
		htest |= abs(fcmp(ind2o[i], true_indo[i]));
		if(htest > 0) {
			warning("Values differ for 're_sort_by_index()' failed at index %i: expected %i, had %i\n", i+1, true_indo[i], ind2o[i]);
		}  
		test |= htest;
		htest = abs(fcmp(v2o[i], true_vr[i]));
		if(htest > 0) {
			warning("Values differ for 're_sort_by_index()' failed at index %i: expected %f, had %f\n", i+1, true_vr[i], v2o[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 're_sort_by_index()' failed\n");
		tests++;
	}
	return tests;  
}


int test_npi_a(void) {
	
	int i, htest = 0, test = 0;	
	int dim = 7;
	double pbmax[7] = {0,0,0,0,0,0,0};	
	double pbmin[7] = {0,0,0,0,0,0,0};
	int obs1[7] = {0,3,0,2,3,0,0};	
	double true_pbmax[7] = {0.09375,0.25,0.09375,0.125,0.25,0.09375,0.09375};
	double true_pbmin[7] = {0.0,0.5,0.0,0.125,0.375,0.0,0.0};

	maxEntropyNPIa(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++){
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) { 
			warning("max entropy values differ for 'npi_a()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_a()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_a()' failed\n");
	}
	return test;

}


int test_npi_1(void) {

	int i, htest = 0, test = 0;
	int dim = 9;
	double pbmax[9] = {0,0,0,0,0,0,0,0,0};
	double pbmin[9] = {0,0,0,0,0,0,0,0,0};
	int obs1[9] = {1, 2, 0, 0, 1, 2, 0, 0, 2};
	double true_pbmax[9] = {0.09375,0.125,0.125,0.125,0.09375,0.125,0.09375,0.09375,0.125};
	double true_pbmin[9] = {0.0,0.375,0.0,0.0,0.0,0.375,0.0,0.0,0.25};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_1()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_1()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_1()' failed\n");
	}
	return test;
}


int test_npi_2(void) {

	int i, htest = 0, test = 0;
	int dim = 14;
	double pbmax[14] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	double pbmin[14] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	int obs1[14] = {0,0,1,2,0,0,2,0,0,1,2,0,0,0};
	double true_pbmax[14] = {0.0625,0.0625,3.0/56,0.125,0.0625,3.0/56,0.125,0.0625,3.0/56,3.0/56,0.125,3.0/56,3.0/56,3.0/56};
	double true_pbmin[14] = {0.0,0.0,0.0,0.375,0.0,0.0,0.375,0.0,0.0,0.0,0.25,0.0,0.0,0.0};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_2()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_2()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_2()' failed\n");
	}
	return test;
}


int test_npi_3(void) {

	int i, htest = 0, test = 0;
	int dim = 7;
	double pbmax[7] = {0,0,0,0,0,0,0};
	double pbmin[7] = {0,0,0,0,0,0,0};
	int obs1[7] = {3,0,3,0,0,0,2};
	double true_pbmax[7] = {0.25, 0.0625, 0.25, 0.0625, 0.125, 0.125, 0.125};
	double true_pbmin[7] = {0.5, 0.0, 0.375, 0.0, 0.0, 0.0, 0.125};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_3()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}       
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_3()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_3()' failed\n");
	}       
	return test;
}


int test_npi_4(void) {

	int i, htest = 0, test = 0;
	int dim = 9;
	double pbmax[9] = {0,0,0,0,0,0,0,0,0};
	double pbmin[9] = {0,0,0,0,0,0,0,0,0};
	int obs1[9] = {6,1,0,3,6,0,1,1,2};
	double true_pbmax[9] = {0.25,0.075,0.05,0.1,0.25,0.05,0.075,0.075,0.075};
	double true_pbmin[9] = {7.0/20,0.0,0.0,0.2,7.0/20,0.0,0.0,0.0,0.1};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_4()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_4()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_4()' failed\n");
	}
	return test;
}


int test_npi_5(void) {

	int i, htest = 0, test = 0;
	int dim = 7;
	double pbmax[7] = {0,0,0,0,0,0,0};
	double pbmin[7] = {0,0,0,0,0,0,0};
	int obs1[7] = {5,0,5,4,0,5,5};
	double true_pbmax[7] = {0.1875, 1.0/24, 0.1875, 0.1875, 1.0/24, 0.1875, 4.0/24};
	double true_pbmin[7] = {0.25, 0.0, 0.25, 3.0/24, 0.0, 5.0/24, 4.0/24};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {
		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_5()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_5()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_5()' failed\n");
	}
	return test;
}


int test_npi_6(void) {

	int i, htest = 0, test = 0;
	int dim = 10;
	double pbmax[10] = {0,0,0,0,0,0,0,0,0,0};
	double pbmin[10] = {0,0,0,0,0,0,0,0,0,0};
	int obs1[10] = {0,0,0,1,1,1,0,1,0,1};
	double true_pbmax[10] = {0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1};
	double true_pbmin[10] = {0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0};

	maxEntropyNPIe(obs1, &dim, pbmax);
	minEntropyNPIa(obs1, &dim, pbmin);

	for(i = 0; i < dim; i++) {

		htest = abs(fcmp(pbmax[i], true_pbmax[i]));
		if (htest > 0) {
			warning("max entropy values differ for 'npi_6()' failed at index %i: expected %f, had %f\n", i+1, true_pbmax[i], pbmax[i]);
		}
		test |= htest;
		htest = abs(fcmp(pbmin[i], true_pbmin[i]));
		if (htest > 0) {
			warning("min entropy values differ for 'npi_6()' failed at index %i: expected %f, had %f\n", i+1, true_pbmin[i], pbmin[i]);
		}
		test |= htest;
	}
	if(test > 0) {
		warning("test for 'npi_6()' failed\n");
	}
	return test;
}
