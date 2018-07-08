## Dummy R file
library(Rcpp)
#library(RcppArmadillo)

sourceCpp("src/utils.cpp")
maxIndexInSet(b,a)

cppFunction('Rcpp::List doSo(Rcpp::IntegerVector vx, arma::mat m) {
  arma::uvec idx = Rcpp::as<arma::uvec>(vx) - 1;
  int ms = idx.size();
  arma::mat m2 =m.rows(idx);
  arma::mat m3 = m2.rows(arma::uvec("0"));
  Rcpp::IntegerVector t = Rcpp::table(Rcpp::as<Rcpp::IntegerVector>(Rcpp::wrap((arma::mat) m2.rows(arma::uvec("0")))));
  int s = Rcpp::table(vx).size();
  Rcpp::List a = Rcpp::List::create(Rcpp::Named("vx") = vx, Rcpp::Named("m") = m, Rcpp::Named("subset") = m2);
  return Rcpp::List::create(Rcpp::Named("alist") = a, Rcpp::Named("sosse") = t, Rcpp::Named("l") = seq_along(vx)-1);
}
', depends = "RcppArmadillo")
a<- matrix(sample(10, size=100,re=T), ncol=5)
v <- c(2)
doSo(v,a)


cppFunction('
Rcpp::NumericVector doThat(Rcpp::IntegerMatrix m, Rcpp::IntegerVector obs) {
//  int splitidx = 2;
//  IntegerVector nlevels = m.attr("nlevels");
//  std::vector< std::vector<int> > splitobs = std::vector< std::vector<int> >(nlevels[splitidx]);
//  for(int i=0; i < obs.size(); ++i) {
//    int obsidx = obs[i];
//    int splitval = m(obsidx, splitidx);
//    splitobs.at(splitval).push_back(obsidx);
//  }
//  for(int j=0; j < splitobs.size(); ++j) {
//    IntegerVector out = Rcpp::wrap(splitobs.at(j));
//    Rcpp::Rcout << out << std::endl;
//  }
  Rcpp::NumericVector nv;
  nv.insert(0, R_PosInf);
//  nv.insert(1, 2.0);
//  nv.insert(2, 3.1);
//  nv.insert(3, -3.0);
//  nv.insert(4, 3.1);
  double maxval = Rcpp::max(nv);
  Rcpp::IntegerVector ids = Rcpp::seq_along(nv) -1;
  Rcpp::IntegerVector maxids = ids[nv==maxval];
  int res = Rcpp::sample(maxids, 1)[0];
  Rcpp::Rcout << maxids << std::endl;
  Rcpp::Rcout << res << std::endl;
  return nv;
}
')
m<- matrix(c(sample(10, size=2000,re=T),sample(5, size=4000,re=T), sample(3, size=2000,re=T), sample(2, size=2000, re=T)), ncol=5)
attr(m, "nlevels") <-c(10,5,5,3,2)
str(m)
obs <- sample(2000, 100)
dres <- split(obs, m[obs,3])
doThat(m, obs)
