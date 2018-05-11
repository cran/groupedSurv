\name{groupedSurv-package}
\alias{groupedSurv-package}
\docType{package}
\title{
	Efficient Estimation of Grouped Survival Models Using the Exact Likelihood Function
}
\description{
The core of this \code{Rcpp}-based package is a set of functions to compute the efficient score statistics for grouped survival models. The functions are designed to analyze grouped time-to-event data with the optional inclusion of either baseline covariates or family structure of related individuals (e.g., trios). Functions for estimating the baseline hazards, frailty variance, nuisance parameters, and fixed effects are also provided. The functions encompass two processes for data with family structure: 1) evaluation of the multiple variable integration to compute the exact proportional-hazards-model-based likelihood and (2) estimation of the desired parameters using maximum likelihood. For data without family structure, only the latter step is performed. The integration is evaluated by the \code{Cuhre} algorithm from the \code{Cuba} library (Hahn, T. (2005). Cuba-a library for multidimensional numerical integration, Comput. Phys. Commun. 168, 78-95 <doi:10.1016/j.cpc.2005.01.010>), and the source files of the \code{Cuhre} function are included in this package. The maximization process is carried out using Brent's algorithm, with the \code{C++} code file from John Burkardt and John Denker (Brent, R., Algorithms for Minimization without Derivatives, Dover, 2002, ISBN 0-486-41998-3).

License: GPL (>= 2)}
\details{
\tabular{ll}{
Package: \tab groupedSurv\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2018-05-10\cr
License: \tab GPL-3\cr
}
Please refer to the individual function documentation or the included vignette for more information. The package vignette serves as a tutorial for using this package.
}

\author{
  Jiaxing Lin \email{jiaxing.lin@duke.edu}, Alexander Sibley, Tracy Truong, Kouros Owzar, Zhiguo Li; Contributors: Yu Jiang, Janice McCarthy, Andrew Allen
}

\references{
  Prentice, R.L. and Gloeckler, L.A. (1978). Regression analysis of grouped survival data with application to breast cancer data. Biometrics, 34:1, 57-67.\cr
	Ripatti, S. and Palmgren, J. (2000). Estimation of Multivariate Frailty Models Using Penalized Partial Likelihood. Biometrics, 56, 1016-1022.\cr
	Hahn, T. (2005). Cuba-a library for multidimensional numerical integration, Computer Physics Communications, 168, 78-95.\cr
}

\keyword{ package }



