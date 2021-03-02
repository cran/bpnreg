#ifndef PACKAGENAME_ADD_H
#define PACKAGENAME_ADD_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(BH)]]
#include <boost/math/special_functions/bessel.hpp>

#include <iostream>
#include <math.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

Rcpp::List rho(arma::vec theta);
double theta_bar(arma::vec theta);
arma::vec eigen_val(arma::mat X);
arma::mat eigen_vec(arma::mat X);
arma::mat mvrnorm_arma_eigen(int n, arma::vec mu, arma::mat sigma);
NumericVector circ_coef_rcpp(double a1, double a2, double b1, double b2);
NumericVector hmodeci(NumericVector x, double cip);
double hmode(NumericVector x, double cip);
NumericVector hmodeciC(NumericVector x, double cip);
double hmodeC(NumericVector x, double cip);

#endif
