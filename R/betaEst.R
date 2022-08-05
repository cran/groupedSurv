# Function to estimate beta
.grad_NF1 <- function(Params, Xmatrix, Kivec, Deltavec, ntps) {
  res <- .Call(
    "_groupedSurv_grad_NF", 
    PACKAGE = "groupedSurv", 
    Params, 
    Xmatrix, 
    Kivec,
    Deltavec, 
    ntps
  )
}

.logLike_NF1 <- function(Params, Xmatrix, Kivec, Deltavec, ntps) {
  .Call(
    "_groupedSurv_logLike_NF", 
    PACKAGE = "groupedSurv", 
    Params, 
    Xmatrix, 
    Kivec,
    Deltavec, 
    ntps
  )
}

betaEst <- function(x, Z = NULL, alpha, theta = NULL, gtime, delta) {

	if (sum(is.infinite(gtime)) >= 1) {
	  ntps <- nlevels(as.factor(gtime)) - 1
	} else {
	  ntps <- nlevels(as.factor(gtime))
	}
  
  if (!any(class(x) == "matrix")) {
    betaIG <- runif(1, 0, 1)
	} else {
		betaIG <- runif(ncol(x), 0, 1)
	}
  
	OK <- complete.cases(x, Z)
	x.NA  <- !is.na(x)
  x.rmNA <- x[OK] 
	Z.rmNA <- Z[OK, ]	 
	x <- cbind(x.rmNA, Z.rmNA)

  x <- as.matrix(x,ncol=ncol(x))
	betaIG <- c(alpha, betaIG, theta)
	
	# convert gtime to vector of integers
	ktime <- as.numeric(factor(gtime)) 

  Est <- optim(
    par = betaIG, 
    fn = .logLike_NF1, 
    gr = .grad_NF1, 
    Xmatrix = x, 
    Kivec = ktime[OK],
    Deltavec = delta[OK], 
    ntps = ntps, 
    method = "BFGS", 
    control = list(
      fnscale = -1
    )
  )$par
  
  betaest <- Est[(ntps + 1)]
}


