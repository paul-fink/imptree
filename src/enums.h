#ifndef RCPP_IMPTREE_ENUMS_H
#define RCPP_IMPTREE_ENUMS_H

enum class EntropyCorrection {
  no = 0,
  abellan,
  strobl,
};

enum class SplitMetric {
  entropyMax = 0,
  entropyRange,
}; 

enum class IpType {
  idm = 0,
  npi,
  npiapprox,
};

enum class Dominance {
  interval = 0,
  maximality
};

#endif /*RCPP_IMPTREE_ENUMS_H*/