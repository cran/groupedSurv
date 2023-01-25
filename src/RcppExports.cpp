// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// alphaEst1
Rcpp::NumericVector alphaEst1(Rcpp::NumericVector dtimeFactor, Rcpp::NumericVector dtime, Rcpp::NumericVector delta);
RcppExport SEXP _groupedSurv_alphaEst1(SEXP dtimeFactorSEXP, SEXP dtimeSEXP, SEXP deltaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtimeFactor(dtimeFactorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtime(dtimeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type delta(deltaSEXP);
    rcpp_result_gen = Rcpp::wrap(alphaEst1(dtimeFactor, dtime, delta));
    return rcpp_result_gen;
END_RCPP
}
// betaEst
double betaEst(std::vector<std::string> fam_group, Rcpp::NumericVector alpha, Rcpp::NumericVector dtime, Rcpp::NumericVector delta, Rcpp::NumericVector g, double var, double lower, double upper, std::vector<std::string> f_ind, int m);
RcppExport SEXP _groupedSurv_betaEst(SEXP fam_groupSEXP, SEXP alphaSEXP, SEXP dtimeSEXP, SEXP deltaSEXP, SEXP gSEXP, SEXP varSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP f_indSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fam_group(fam_groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtime(dtimeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< double >::type var(varSEXP);
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type f_ind(f_indSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(betaEst(fam_group, alpha, dtime, delta, g, var, lower, upper, f_ind, m));
    return rcpp_result_gen;
END_RCPP
}
// effScore
List effScore(double beta, Rcpp::NumericVector Params, Rcpp::NumericMatrix G, Rcpp::NumericMatrix Xmatrix, Rcpp::IntegerVector Kivec, /* 01-10-2023: changed bool to int */               Rcpp::NumericVector Deltavec, int ntps, int nCores, int reScore);
RcppExport SEXP _groupedSurv_effScore(SEXP betaSEXP, SEXP ParamsSEXP, SEXP GSEXP, SEXP XmatrixSEXP, SEXP KivecSEXP, SEXP DeltavecSEXP, SEXP ntpsSEXP, SEXP nCoresSEXP, SEXP reScoreSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Params(ParamsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type G(GSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Xmatrix(XmatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type Kivec(KivecSEXP);
    Rcpp::traits::input_parameter< /* 01-10-2023: changed bool to int */               Rcpp::NumericVector >::type Deltavec(DeltavecSEXP);
    Rcpp::traits::input_parameter< int >::type ntps(ntpsSEXP);
    Rcpp::traits::input_parameter< int >::type nCores(nCoresSEXP);
    Rcpp::traits::input_parameter< int >::type reScore(reScoreSEXP);
    rcpp_result_gen = Rcpp::wrap(effScore(beta, Params, G, Xmatrix, Kivec, Deltavec, ntps, nCores, reScore));
    return rcpp_result_gen;
END_RCPP
}
// effScoreFam
List effScoreFam(double beta, double sigma2, std::vector<std::string> fam_group, Rcpp::NumericVector i_at, Rcpp::NumericVector i_dt, Rcpp::NumericVector i_Delta, Rcpp::NumericVector i_G, std::vector<std::string> f_ind, int m);
RcppExport SEXP _groupedSurv_effScoreFam(SEXP betaSEXP, SEXP sigma2SEXP, SEXP fam_groupSEXP, SEXP i_atSEXP, SEXP i_dtSEXP, SEXP i_DeltaSEXP, SEXP i_GSEXP, SEXP f_indSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type sigma2(sigma2SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fam_group(fam_groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type i_at(i_atSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type i_dt(i_dtSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type i_Delta(i_DeltaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type i_G(i_GSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type f_ind(f_indSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(effScoreFam(beta, sigma2, fam_group, i_at, i_dt, i_Delta, i_G, f_ind, m));
    return rcpp_result_gen;
END_RCPP
}
// grad_NF
Rcpp::NumericVector grad_NF(Rcpp::NumericVector Params, Rcpp::NumericMatrix Xmatrix, Rcpp::IntegerVector Kivec, Rcpp::NumericVector Deltavec, int ntps);
RcppExport SEXP _groupedSurv_grad_NF(SEXP ParamsSEXP, SEXP XmatrixSEXP, SEXP KivecSEXP, SEXP DeltavecSEXP, SEXP ntpsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Params(ParamsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Xmatrix(XmatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type Kivec(KivecSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Deltavec(DeltavecSEXP);
    Rcpp::traits::input_parameter< int >::type ntps(ntpsSEXP);
    rcpp_result_gen = Rcpp::wrap(grad_NF(Params, Xmatrix, Kivec, Deltavec, ntps));
    return rcpp_result_gen;
END_RCPP
}
// ll
double ll(std::vector<std::string> fam_group, Rcpp::NumericVector alpha, Rcpp::NumericVector dtime, Rcpp::NumericVector delta, Rcpp::NumericVector g, double beta, double var, std::vector<std::string> f_ind, int m);
RcppExport SEXP _groupedSurv_ll(SEXP fam_groupSEXP, SEXP alphaSEXP, SEXP dtimeSEXP, SEXP deltaSEXP, SEXP gSEXP, SEXP betaSEXP, SEXP varSEXP, SEXP f_indSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fam_group(fam_groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtime(dtimeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type var(varSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type f_ind(f_indSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(ll(fam_group, alpha, dtime, delta, g, beta, var, f_ind, m));
    return rcpp_result_gen;
END_RCPP
}
// logLike_NF
double logLike_NF(Rcpp::NumericVector Params, Rcpp::NumericMatrix Xmatrix, Rcpp::IntegerVector Kivec, Rcpp::NumericVector Deltavec, int ntps);
RcppExport SEXP _groupedSurv_logLike_NF(SEXP ParamsSEXP, SEXP XmatrixSEXP, SEXP KivecSEXP, SEXP DeltavecSEXP, SEXP ntpsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Params(ParamsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type Xmatrix(XmatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type Kivec(KivecSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Deltavec(DeltavecSEXP);
    Rcpp::traits::input_parameter< int >::type ntps(ntpsSEXP);
    rcpp_result_gen = Rcpp::wrap(logLike_NF(Params, Xmatrix, Kivec, Deltavec, ntps));
    return rcpp_result_gen;
END_RCPP
}
// varEst
double varEst(std::vector<std::string> fam_group, Rcpp::NumericVector alpha, Rcpp::NumericVector dtime, Rcpp::NumericVector delta, Rcpp::NumericVector g, double beta, double lower, double upper, std::vector<std::string> f_ind, int m);
RcppExport SEXP _groupedSurv_varEst(SEXP fam_groupSEXP, SEXP alphaSEXP, SEXP dtimeSEXP, SEXP deltaSEXP, SEXP gSEXP, SEXP betaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP f_indSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type fam_group(fam_groupSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dtime(dtimeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type g(gSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type f_ind(f_indSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(varEst(fam_group, alpha, dtime, delta, g, beta, lower, upper, f_ind, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_groupedSurv_alphaEst1", (DL_FUNC) &_groupedSurv_alphaEst1, 3},
    {"_groupedSurv_betaEst", (DL_FUNC) &_groupedSurv_betaEst, 10},
    {"_groupedSurv_effScore", (DL_FUNC) &_groupedSurv_effScore, 9},
    {"_groupedSurv_effScoreFam", (DL_FUNC) &_groupedSurv_effScoreFam, 9},
    {"_groupedSurv_grad_NF", (DL_FUNC) &_groupedSurv_grad_NF, 5},
    {"_groupedSurv_ll", (DL_FUNC) &_groupedSurv_ll, 9},
    {"_groupedSurv_logLike_NF", (DL_FUNC) &_groupedSurv_logLike_NF, 5},
    {"_groupedSurv_varEst", (DL_FUNC) &_groupedSurv_varEst, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_groupedSurv(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
