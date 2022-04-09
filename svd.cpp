// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]

#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>

/**
 * @brief Simon Funk's Matrix Factorization.
 *
 * Approximate Y as U*V^T where U and V each have @p nFeatures columns.
 *
 * @param coo_i User indexes of the rating matrix Y.
 * @param coo_j Movie indexes of the rating matrix Y.
 * @param coo_x Ratings in the rating matrix Y.  Note Y is a sparse matrix, where
 *     a zero represents no rating given.
 * @param nFeatures the number of features to use, i.e. the number of columns in U and V.
 * @steps Number of epochs. Each epoch refines the U and V estimates by iterating
 *     through all known ratings once.
 * @regCoef Regularization coefficient, prevents overfitting.
 * @learningRate learning rate of gradient descent.
 *  
 * @return An @c RCpp::list object containing U and V.
 * 
 * @see https://sifter.org/~simon/journal/20061211.html
 * @see https://github.com/ludovikcoba/rrecsys/
 */
// [[Rcpp::export]]
Rcpp::List funkCpp(
    Rcpp::NumericVector coo_i,
    Rcpp::NumericVector coo_j,
    Rcpp::NumericVector coo_x,
    int nFeatures,
    int steps,
    double regCoef,
    double learningRate
)
{
  int nUsers = Rcpp::max(coo_i)+1;
  int nItems = Rcpp::max(coo_j)+1;
  int nRatings = coo_x.size();
  
  arma::mat U(nUsers, nFeatures, arma::fill::randu);
  arma::mat V(nItems, nFeatures, arma::fill::randu);
  
  U *= sqrt(0.5/nFeatures);
  V *= sqrt(0.5/nFeatures);
  
  Rcpp::Rcout << "nUsers:" << nUsers << ", ";
  Rcpp::Rcout << "nItems:" << nItems << ", ";
  Rcpp::Rcout << "nRatings:" << nRatings << std::endl;
  
  
  Progress p(steps, true);
  for (int ss = 0; ss < steps; ss++) {
    
    // Kill program if user has requested it (Ctrl+C in most consoles)
    Rcpp::checkUserInterrupt();
    
    // iterate over known ratings
    for (int r = 0; r < nRatings; r++) {
      int i = coo_i[r]; // user index
      int j = coo_j[r]; // item index
      double err = coo_x[r] - arma::dot(U.row(i), V.row(j)); // prediction error
      
      // update features
      U.row(i) += learningRate * (err*V.row(j) - regCoef*U.row(i));
      V.row(j) += learningRate * (err*U.row(i) - regCoef*V.row(j));
    }
    p.increment();
  }
  Rcpp::Rcout << std::endl;
  
  Rcpp::List ret;
  ret["U"] = U;
  ret["V"] = V;
  return ret;
}
