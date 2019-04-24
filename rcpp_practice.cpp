#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

//[[Rcpp::export]]
NumericVector rangeC(NumericVector x) {
  int n = x.size();
  NumericVector out(2);
  out[0] = x[0];
  out[1] = x[0];
  for(int i=1;i<n;++i){
    if(x[i]>out[1]) out[1] = x[i];
    if(x[i]<out[0]) out[0] = x[i];
  }
  return out;
}

//[[Rcpp::export]]
NumericVector diffC(NumericVector x){
  int n = x.size();
  NumericVector out(n-1);
  
  for(int i=0; i<n-1; ++i){
    out[i] = x[i+1] - x[i];
  }
  return out;
}

//[[Rcpp::export]]
NumericVector cumsumC(NumericVector x){
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i=1; i<n; ++i){
    out[i] = x[i] + out[i-1];
  }
  return out;
}

//[[Rcpp::export]]
bool allC(LogicalVector x){
  int n = x.size();
  
  for(int i=0; i<n; ++i){
    if(x[i]==false) return false;      
  }
  return true;
}

double varC(NumericVector x){
  int n = x.size();
  
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

