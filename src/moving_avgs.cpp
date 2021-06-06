#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector sma(NumericVector x, int n){

    int m = x.length();
    NumericVector ma(m-n+1);
    for (int i = 0; i<m-n+1; ++i ){
        double s = 0;
        for (int j = i; j<i+n; ++j){
            s = s + x[j];
        }
        ma[i] = s/n;
    }
    return(ma);
    
}



// [[Rcpp::export]]
NumericVector simple_moving_avg(DataFrame dt, int n){
    
    NumericVector x = dt["x"];
    int m = x.length();
    NumericVector ma(m);
    for (int i = 0; i<m-n+1; ++i ){
        double s = 0;
        for (int j = i; j<i+n; ++j){
            s = s + x[j];
        }
        ma[i] = s/n;
    }
    return(ma);
    
}


// [[Rcpp::export]]
NumericVector weighted_moving_avg(DataFrame dt, int n){
    
    int denominator = n*(n+1)/2;
    NumericVector x = dt["x"];
    int m = x.length();
    NumericVector ma(m);
    for (int i = 0; i<m-n+1; ++i ){
        double s = 0;
        for (int j = i; j<i+n; ++j){
            s = s + x[j] * abs((j-i-n)) / denominator;
        }
        ma[i] = s;
    }
    return(ma);
}