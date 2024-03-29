/*****************************/
/* Jiaxing Lin               */
/* 12-06-2017                */
/* log likelihood function   */
/*****************************/

#ifdef _OPENMP
#include <omp.h>
#endif
#include <Rcpp.h>
#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;
using Eigen::VectorXd;
using namespace std;
using namespace Rcpp;

#include "effScore.h"

//[[Rcpp::export]]
List effScore(double beta, Rcpp::NumericVector Params, Rcpp::NumericMatrix G,
              Rcpp::NumericMatrix Xmatrix, Rcpp::IntegerVector Kivec,
              /* 01-10-2023: changed bool to int */
              Rcpp::NumericVector Deltavec, int ntps, int nCores, int reScore) {
  // cast Rcpp Vector and Matrix to Eigen Vector and Matrix
  Eigen::Map<Eigen::VectorXd> params = as<Eigen::Map<Eigen::VectorXd>>(Params);
  Eigen::Map<Eigen::MatrixXd> xmatrix =
      as<Eigen::Map<Eigen::MatrixXd>>(Xmatrix);
  Eigen::Map<Eigen::VectorXi> kivec = as<Eigen::Map<Eigen::VectorXi>>(Kivec);
  Eigen::Map<Eigen::VectorXd> deltavec =
      as<Eigen::Map<Eigen::VectorXd>>(Deltavec);
  Eigen::Map<Eigen::MatrixXd> g = as<Eigen::Map<Eigen::MatrixXd>>(G);

  Eigen::VectorXd scoreStatic(g.cols());
  scoreStatic.fill(0);
  Eigen::MatrixXd UiFull(g.rows(), g.cols());

#ifdef _OPENMP
  omp_set_num_threads(nCores); // define number of threads.
#endif
#ifdef _OPENMP
#pragma omp parallel for
#endif

  /*
  01-13-2023: format using clang-format; add braces to for and ifelse blocks
  */
  for (int i = 0; i < g.cols(); i++) {
    Eigen::VectorXd g_tmp = g.col(i);
    Eigen::VectorXd USNPs(g_tmp.size());

    // default to return statistics only
    scoreStatic(i) = effScore_NF_S(beta, params, g_tmp, xmatrix, kivec,
                                   deltavec, ntps, reScore, USNPs);

    // options for return score matrix
    if (reScore) {
      UiFull.col(i) = USNPs;
    }
  }

  Rcpp::NumericVector sStatics(wrap(scoreStatic));

  if (reScore) {
    Rcpp::NumericMatrix uScore(wrap(UiFull));
    return List::create(Named("sStatics") = sStatics, Named("uScore") = uScore);
  } else {
    return List::create(Named("sStatics") = sStatics);
  }
}
