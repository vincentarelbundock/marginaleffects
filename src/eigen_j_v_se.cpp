// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// [[Rcpp::export]]
Eigen::VectorXd eigen_J_V_SE(const Eigen::Map<Eigen::MatrixXd> J,
                           const Eigen::Map<Eigen::MatrixXd> V) {
  // Matrix multiplication: J %*% V
  Eigen::MatrixXd JV = J * V;

  // Elementwise product: (J %*% V) * J
  Eigen::MatrixXd JV_elementwise = JV.cwiseProduct(J);

  // Row sums
  Eigen::VectorXd row_sums = JV_elementwise.rowwise().sum();

  // Square root of row sums
  return row_sums.array().sqrt();
}

