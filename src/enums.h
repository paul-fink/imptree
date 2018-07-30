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

namespace IpTypeLookup {
  inline const char* toString(IpType v) {
    switch (v) {
      case IpType::idm:   return "IDM";
      case IpType::npi:   return "NPI";
      case IpType::npiapprox: return "NPIapprox";
    }
  }
}

enum class Dominance {
  interval = 0,
  maximality
};

#endif /*RCPP_IMPTREE_ENUMS_H*/