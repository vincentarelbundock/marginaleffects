// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// [[Rcpp::export]]
SEXP eigenTCrossProd(const Eigen::Map<Eigen::MatrixXd> A, const Eigen::Map<Eigen::MatrixXd> B){
  Eigen::MatrixXd C = A * B.transpose();
  return Rcpp::wrap(C);
}

