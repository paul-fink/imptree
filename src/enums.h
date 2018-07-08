#ifndef RCPP_IMPTREE_ENUMS_H
#define RCPP_IMPTREE_ENUMS_H

namespace EntropyCorrection {
enum Enum {
  no = 0,
  abellan,
  strobl,
};
}

namespace SplitMetric {
enum Enum {
  entropyMax,
  entropyRange,
}; 
}

namespace IpType {
enum Enum {
  idm,
  npi,
  npiapprox,
};
}

#endif /*RCPP_IMPTREE_ENUMS_H*/