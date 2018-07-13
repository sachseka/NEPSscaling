#' Plausible values imputation using the PIAAC 2012 and PIAAC-L 2015 assessment data in the domains of literacy and numeracy
#'
#' This function estimates a four-dimensional latent regression item response model (first dimension: PIAAC 2012 literacy,
#' second dimension: PIAAC 2012 numeracy, third dimension: PIAAC-L 2015 literacy, fourth dimension: PIAAC-L 2015 numeracy)
#' for binary item response data considering partially missing covariate data. For more detailed information on the statistical
#' model and the estimation algorithm, see the PIAAC-L technical report on scaling (Carstensen, Gaasch & Rothaug, 2017).
#' @param path full path of the folder containing the data files ZA5845 and ZA5989_Persons_15 in Stata format.
#' @param X data frame containing the sequential ID (named \code{seqid}) and background variable from the PIAAC and PIAAC-L
#' Scientific Use Files. They can be numeric or factor variables and contain missing values coded as \code{NA}. With \code{X} set to \code{NULL} (default) an empty population model is estimated.
#' @param nopvs number of plausible values to draw for each respondent.
#' @param itermcmc number of MCMC iterations.
#' @param burnin number of burnin iterations.
#' @return list with \code{nopvs} elements, each containing a data frame of the sequential ID, plausible values for
#' each dimension and, if specified, imputed versions of \code{X}. Resulting plausible values are transformed onto the PIAAC 2012 scale (weighted means and standard deviations based on the SUF). Additionally each list element is saved as a
#' Stata file in the folder specified by \code{path}.
#' @references Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report.
#' Manuscript in preparation.
#' @importFrom readstata13 read.dta13 save.dta13
#' @importFrom stats model.matrix runif rnorm pnorm qnorm predict
#' @importFrom MASS mvrnorm
#' @importFrom rpart rpart rpart.control
#' @importFrom Hmisc wtd.mean wtd.var
#' @export
litnum1215 <- function(
  path,
  X = NULL,
  nopvs = 10,
  itermcmc = 22000,
  burnin = 2000
){

  t0 <- proc.time()
  Y <- data.matrix(LN1215[, -1])
  Xid <- LN1215[, 1, drop = FALSE]
  YOBS <- !is.na(Y)
  N <- nrow(Y)
  J <- ncol(Y)
  DIM <- 4
  J1dim <- J/DIM
  J1diminv <- 1/J1dim
  Jdim <- matrix(1:J, nrow = J1dim)
  THETA <- matrix(rnorm(N*DIM), nrow = N, ncol = DIM)
  iterpvs <- sort(sample((burnin + 1):itermcmc, nopvs))
  PVs <- vector("list", nopvs)
  names(PVs) <- paste("Iteration", iterpvs)
  if(is.null(X)){
    ANYXMIS <- FALSE
    XDM <- matrix(1, nrow = N)
  }else{
    if(!all(Xid$seqid %in% X$seqid)){
      stop(paste0("Input argument X must contain the respondent having sequential ID ",
        paste(Xid$seqid[which(!(Xid$seqid %in% X$seqid))], collapse = ", "), "!"))
    }
    X <- merge(Xid, X, by = "seqid", all.x = TRUE)
    X <- X[, -1, drop = FALSE]
    ANYXMIS <- any(is.na(X))
    if(ANYXMIS){
      XOBS <- !is.na(X)
      XMIS <- is.na(X)
      xmisord <- names(sort(colSums(XMIS)))[sort(colSums(XMIS)) > 0]
      for(k in xmisord){
        X[XMIS[, k], k] <- sample(X[XOBS[, k], k], sum(XMIS[, k]), replace = TRUE)
      }
      X2IMP <- data.frame(X, THETA)
      XcolsTHETA <- (ncol(X2IMP) - DIM + 1):ncol(X2IMP)
    }
    XDM <- model.matrix(~., X)
  }
  K1X <- ncol(XDM)
  XDMt <- t(XDM)
  XX <- crossprod(XDM)
  GAMMA <- matrix(rep(0, K1X*DIM), nrow = K1X)
  XGAMMA <- XDM%*%GAMMA
  SIGMA <- diag(DIM)
  ALPHA <- matrix(0, nrow = J, ncol = DIM)
  for(dim in 1:DIM){
    ALPHA[Jdim[, dim], dim] <- 1
  }
  BETA <- rep(0, J)
  XI <- rbind(rep(1, J), BETA)
  CovXi0 <- 100*diag(2)
  PrecXi0 <- solve(CovXi0)
  CovGamma0 <- 100*diag(K1X*DIM)
  PrecGamma0 <- solve(CovGamma0)
  NuSigma <- N + DIM
  LO <- matrix(c(-Inf, 0)[Y + 1], N, J)
  HI <- matrix(c(0, Inf)[Y + 1], N, J)
  ONES <- matrix(1, nrow = N, ncol = 1)
  cat("PVPIAACL is running...\n")
  cat("progress:\n")
  pb <- txtProgressBar(min = 0, max = itermcmc, style = 3)
  for(ii in 1:itermcmc){
    # (1)
    MU <- THETA%*%t(ALPHA) - ONES%*%BETA
    FA <- pnorm(LO - MU)
    FB <- pnorm(HI - MU)
    YLAT <- MU + qnorm(matrix(runif(N*J), nrow = N, ncol = J)*(FB - FA) + FA)
    # (2)
    for(dim in 1:DIM){
      XITEM <- cbind(THETA[, dim], -1)
      for(j in Jdim[, dim]){
        Covitem <- solve(crossprod(XITEM[YOBS[, j], ]) + PrecXi0)
        muitem <- Covitem%*%crossprod(XITEM[YOBS[, j], ], YLAT[YOBS[, j], j])
        XI[1, j] <- 0
        while(XI[1, j] <= 0){
          XI[, j] <- mvrnorm(1, muitem, Covitem)
        }
      }
    }
    ALPHALITMEAN <- (XI[1, Jdim[, 1]] + XI[1, Jdim[, 3]])/2
    ALPHANUMMEAN <- (XI[1, Jdim[, 2]] + XI[1, Jdim[, 4]])/2
    BETALITMEAN <- (XI[2, Jdim[, 1]] + XI[2, Jdim[, 3]])/2
    BETANUMMEAN <- (XI[2, Jdim[, 2]] + XI[2, Jdim[, 4]])/2
    ALPHALITR <- ALPHALITMEAN*(1/prod(ALPHALITMEAN))^J1diminv
    ALPHANUMR <- ALPHANUMMEAN*(1/prod(ALPHANUMMEAN))^J1diminv
    BETALITR <- BETALITMEAN - sum(BETALITMEAN)/J1dim
    BETANUMR <- BETANUMMEAN - sum(BETANUMMEAN)/J1dim
    ALPHA[Jdim[, 1], 1] <- ALPHA[Jdim[, 3], 3] <- ALPHALITR
    ALPHA[Jdim[, 2], 2] <- ALPHA[Jdim[, 4], 4] <- ALPHANUMR
    BETA[c(Jdim[, 1], Jdim[, 3])] <- BETALITR
    BETA[c(Jdim[, 2], Jdim[, 4])] <- BETANUMR
    # (3)
    for(i in 1:N){
      Covtheta <- solve(crossprod(ALPHA[YOBS[i, ], ]) + solve(SIGMA))
      mutheta <- Covtheta%*%(crossprod(ALPHA[YOBS[i, ], ], YLAT[i, YOBS[i, ]] +
        BETA[YOBS[i, ]]) + solve(SIGMA)%*%t(GAMMA)%*%XDM[i, ])
      THETA[i, ] <- mvrnorm(1, mutheta, Covtheta)
    }
    # (4)
    Covgamma <- solve(solve(SIGMA)%x%XX + PrecGamma0)
    mugamma <- Covgamma%*%((solve(SIGMA)%x%diag(K1X))%*%matrix(XDMt%*%THETA))
    GAMMA <- matrix(mvrnorm(1, mugamma, Covgamma), nrow = K1X)
    XGAMMA <- XDM%*%GAMMA
    # (5)
    VSigma <- crossprod(THETA - XGAMMA) + diag(DIM)
    SIGMA <- rwishart(NuSigma, chol2inv(chol(VSigma)))
    # save MCMC draws
    saveiter <- ii %in% iterpvs
    if(saveiter){
      whichpv <- which(names(PVs) == paste("Iteration", ii))
      PVs[[whichpv]] <- setNames(data.frame(Xid, THETA),
        c("seqid", paste0(rep(c("PVLit", "PVNum"), 2), whichpv,
          rep(c("_12", "_15"), each = 2))))
    }
    if(ANYXMIS){
      # (6)
      X2IMP[, XcolsTHETA] <- THETA
      X <- seqcart(X2IMP, xmisord, XOBS, XMIS, 5, 1e-04)
      X <- X[, -XcolsTHETA]
      XDM <- model.matrix(~., X)
      XDMt <- t(XDM)
      XX <- crossprod(XDM)
      if(saveiter){
        PVs[[whichpv]] <- cbind(PVs[[whichpv]], X)
      }
    }
    setTxtProgressBar(pb, ii)
  }
  close(pb)
  finalweight12 <- Xid
  finalweight12 <- merge(finalweight12,
    ZA5845[, c("seqid", "SPFWT0")], by = "seqid")[, -1]
  PVsLit12_SUF <- Xid
  PVsLit12_SUF <- merge(PVsLit12_SUF,
    ZA5845[, c("seqid", grep("PVLIT", names(ZA5845), value = TRUE))],
    by = "seqid")[, -1]
  PVsNum12_SUF <- Xid
  PVsNum12_SUF <- merge(PVsNum12_SUF,
    ZA5845[, c("seqid", grep("PVNUM", names(ZA5845), value = TRUE))],
    by = "seqid")[, -1]
  GMLit12_SUF <- mean(apply(PVsLit12_SUF, 2, wtd.mean, weights = finalweight12))
  GSdLit12_SUF <- mean(sqrt(apply(PVsLit12_SUF, 2, wtd.var,
    weights = finalweight12)))
  GMNum12_SUF <- mean(apply(PVsNum12_SUF, 2, wtd.mean,
    weights = finalweight12))
  GSdNum12_SUF <- mean(sqrt(apply(PVsNum12_SUF, 2, wtd.var,
    weights = finalweight12)))
  PVsLit12_est <- sapply(PVs, `[[`, 2)
  PVsNum12_est <- sapply(PVs, `[[`, 3)
  PVsLit15_est <- sapply(PVs, `[[`, 4)
  PVsNum15_est <- sapply(PVs, `[[`, 5)
  GMLit12_est <- mean(apply(PVsLit12_est, 2, wtd.mean, weights = finalweight12))
  GSdLit12_est <- mean(sqrt(apply(PVsLit12_est, 2, wtd.var, weights = finalweight12)))
  GMNum12_est <- mean(apply(PVsNum12_est, 2, wtd.mean, weights = finalweight12))
  GSdNum12_est <- mean(sqrt(apply(PVsNum12_est, 2, wtd.var, weights = finalweight12)))
  ALit <- GSdLit12_SUF/GSdLit12_est
  BLit <- -GMLit12_est*ALit + GMLit12_SUF
  ANum <- GSdNum12_SUF/GSdNum12_est
  BNum <- -GMNum12_est*ANum + GMNum12_SUF
  PVsLit12_est_t <- apply(PVsLit12_est, 2, function(x) x*ALit + BLit)
  PVsNum12_est_t <- apply(PVsNum12_est, 2, function(x) x*ANum + BNum)
  PVsLit15_est_t <- apply(PVsLit15_est, 2, function(x) x*ALit + BLit)
  PVsNum15_est_t <- apply(PVsNum15_est, 2, function(x) x*ANum + BNum)
  for(pv in 1:nopvs){
    PVs[[pv]][, 2] <- PVsLit12_est_t[, pv]
    PVs[[pv]][, 3] <- PVsNum12_est_t[, pv]
    PVs[[pv]][, 4] <- PVsLit15_est_t[, pv]
    PVs[[pv]][, 5] <- PVsNum15_est_t[, pv]
    save.dta13(data = PVs[[pv]], file = paste0(path, "litnum1215_", pv, ".dta"))
  }
  t1 <- proc.time()
  ext <- round(round(t1[3] - t0[3]))
  cat("litnum1215 completed :)\n")
  if(ext < 7200){
    cat(paste("Your analysis took", round(ext/60),
      "minutes to execute"))
  }else{
    cat(paste("Your analysis took", round(ext/3600, 1),
      "hours to execute"))
  }
  return(PVs)

}
