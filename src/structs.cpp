#include <Rcpp.h>
#include "structs.h"

// Constructor for Data object
Data::Data(const Rcpp::IntegerMatrix & mat) : data(mat)
{
  classidx = data.attr("classidx");
  nlevels = data.attr("nlevels");
  labels = data.attr("labels");
  varnames = data.names();
}

std::string ProbInterval::to_string(const int nsmall, const std::string &sep) const {
  
  size_t ncat = freq.size();
  std::ostringstream out;
  out << std::fixed << std::setprecision(nsmall);
  for(size_t i = 0; i < ncat - 1; ++i) {
    out << "[" << lower[i] << ";" << upper[i] << "]" << sep;
  }
  out << "[" << lower[ncat] << ";" << upper[ncat] << "]";
  return out.str();
}
