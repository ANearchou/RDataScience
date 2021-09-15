// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>                        
using namespace Rcpp;

//' Cosine Similarities
//'
//'
//' Given a User-Item Sparse Matrix, it will calculate a User-User Sparse Matrix of Cosine Similarities
//' 
//' @param X sp_mat 
// [[Rcpp::export]]
arma::sp_mat cosine_similarity_mat(arma::sp_mat X){
  
  arma::mat Xm(X);
  arma::mat Y = Xm * arma::trans(Xm); 
  arma::sp_mat out ( Y / (arma::sqrt(arma::diagvec(Y)) * arma::trans(arma::sqrt(arma::diagvec(Y)))) );
  return out;
  
}


//' Cosine Similarities
//'
//'
//' Given a User-Item Sparse Matrix, it will calculate a User-User Sparse Matrix of Cosine Similarities
//' 
//' @param X sp_mat 
// [[Rcpp::export]]
arma::sp_mat cosine_similarity(arma::sp_mat X){
  
  arma::sp_mat Y = X * arma::trans(X);
  arma::sp_mat Z = arma::sqrt(arma::diagvec(Y)) * arma::trans(arma::sqrt(arma::diagvec(Y)));
  arma::sp_mat z_inverse(Z);
  z_inverse.transform([](double val) {return (1.0/val);});

  arma::sp_mat out = Y % z_inverse;
  return out; 

}