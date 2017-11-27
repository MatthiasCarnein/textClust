//
// Created by Dede on 11.09.2016.
//

#include "Util.hpp"
namespace Utility{
double Util::innerProduct(Rcpp::List x,
                                 Rcpp::List y) {
  double erg=0;
  for(int i = 0;i<x.size();i++){
    erg = erg+ ((double)x[i]*(double)y[i]);
  }
  return erg;
}

double Util::vectorLengthEuclid(Rcpp::List x) {
  double val = innerProduct(x,x);
  return sqrt(val);
}

double Util::vectorLengthManhatten(Rcpp::NumericVector x) {
  Rcpp::NumericVector result;
  result = abs(x);
  double erg = std::accumulate(result.begin(),result.end(), 0.0);
  return erg;
}

Rcpp::NumericMatrix Util::multiply(Rcpp::NumericVector x){
  Rcpp::NumericMatrix m = Rcpp::NumericMatrix(x.size(),x.size());
  for(int i = 0;i<x.size();i++){
    for(int j = 0; j<x.size();j++){
      m(i,j)=x[i]*x[j];
    }
  }
  return(m);
}
}
