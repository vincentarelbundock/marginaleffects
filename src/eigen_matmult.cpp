// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// [[Rcpp::export]]
SEXP eigenMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::VectorXd> B){
  Eigen::VectorXd C = A * B;
  return Rcpp::wrap(C);
}
