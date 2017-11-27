//
// Created by Dede on 11.09.2016.
//

#ifndef CFTREE_UTIL_HPP
#define CFTREE_UTIL_HPP


#include <numeric>
#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <math.h>

namespace Utility{
class Util {
public:
  static double innerProduct(Rcpp::List x,
                             Rcpp::List y);
  static double vectorLengthEuclid(Rcpp::List x);
  static double vectorLengthManhatten(Rcpp::NumericVector x);
  static Rcpp::NumericMatrix multiply(Rcpp::NumericVector x);
private:
  Util() {}
};

}

#endif //CFTREE_UTIL_HPP
