#' Plausible values imputation using the PIAAC 2012 and PIAAC-L 2015 assessment data in the domains of literacy and numeracy
#'
#' This function estimates a four-dimensional latent regression item response model (first dimension: PIAAC 2012 literacy,
#' second dimension: PIAAC 2012 numeracy, third dimension: PIAAC-L 2015 literacy, fourth dimension: PIAAC-L 2015 numeracy)
#' for binary item response data considering partially missing covariate data. For more detailed information on the statistical
#' model and the estimation algorithm, see the PIAAC-L technical report on scaling (Carstensen, Gaasch & Rothaug, 2017).
#' @param Y test responses
#' @param X covariates
#' @param npvs number of plausible values to draw for each respondent.
#' @param itermcmc number of MCMC iterations.
#' @param burnin number of burnin iterations.
#' @param est.alpha logical, should alphas be estimated or fixed to 0.5 for ordinal items
#' @param thin numeric, thinning interval
#' @param tdf degrees of freedom of multivariate-t proposal distribution for category cutoff
#' parameters for ordinal items.
#' @param cartctrl1 minimum number of observations in any terminal CART node during covariates
#' imputation cycles.
#' @param cartctrl2 complexity parameter. Any CART split that does not decrease the overall
#' lack of fit by a factor of \code{cartctrl2} is not attempted during covariates imputation
#' cycles.
#' @param SC string, NEPS starting cohort
#' @param domain string, competence domain
#' @param waves vector of strings, waves of the competence testings
#' @return list with \code{npv} elements, each containing a data frame of the ID, plausible values for
#' each dimension and, if specified, imputed versions of \code{X}.
#' @references Carstensen, C. H., Gaasch, J.-C., & Rothaug, E. (2017). Scaling PIAAC-L cognitive data: technical report.
#' Manuscript in preparation.
#' @importFrom stats model.matrix runif rnorm pnorm qnorm predict
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvt dmvt dmvnorm
#' @importFrom ucminf ucminf
#' @importFrom rpart rpart rpart.control
#' @export
plausible_values_mdlrm <- function(
    Y,
    X = NULL,
    npv = 10,
    est.alpha = TRUE,
    itermcmc,
    burnin,
    thin = 1,
    tdf = 10,
    cartctrl1 = 5,
    cartctrl2 = 1e-04,
    SC,
    domain,
    waves
){
    if (!is.null(X)) {
        X <- data.frame(X)
        ID_t <- X$ID_t
        X <- data.matrix(X[, -which(names(X)=='ID_t')])
    }
    Y <- data.frame(Y)
    YPL1 <- Y + 1
    YPL2 <- Y + 2

    N <- nrow(Y)
    J <- ncol(Y)
    if (SC == "SC6") {
        if (domain == "RE") {
            DIM <- 2
            Jdim <- c(30, 35)
            jdim <- list(1:30, 31:65)
            jdim2 <- c(rep(1, 30), rep(2, 35))
        } else if (domain == "MA") {
            DIM <- 2
            Jdim <- c(21, 63)
            jdim <- list(1:21, 22:84)
            jdim2 <- c(rep(1, 21), rep(2, 63))
        }
    }
    Jdiminv <- 1/Jdim

    Q <- apply(Y, 2, function(x){
        length(unique(x[!is.na(x)]))
    })
    QMI2 <- Q - 2
    ITEMBIN <- ifelse(Q == 2, TRUE, FALSE)
    POSITEMORD <- which(Q != 2)

    YOBS <- !is.na(Y)

    THETA <- matrix(rnorm(N*DIM), nrow = N, ncol = DIM)
    YLAT <- matrix(0, nrow = N, ncol = J)

    if(is.null(X)){
        ANYXMIS <- FALSE
        XDM <- matrix(1, nrow = N)
    }else{
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
        ALPHA[jdim[[dim]], dim] <- 1
    }
    BETA <- rep(0, J)
    XI <- rbind(rep(1, J), BETA)
    TAU <- lapply(Q, function(x){
        if(x == 2){
            NULL
        } else {
            rep(0, x - 2)
        }
    })
    KAPPA <- lapply(Q, function(x){
        if(x == 2){
            c(-1e+05, 0, 1e+05)
        } else {
            c(-1e+05, 0, cumsum(exp(rep(0, x - 2))), 1e+05)
        }
    })

    Gamma <- matrix(0, nrow = itermcmc, ncol = G*K1X)
    Sigma2 <- vector("numeric", itermcmc)
    Alpha <- array(0, c(itermcmc, J, DIM))
    Beta <- matrix(0, nrow = itermcmc, ncol = J)
    Kappa <- matrix(0, nrow = itermcmc, ncol = sum(QMI2))
    accTau <- rep(0, J)
    names(accTau) <- colnames(Y)
    Theta <- array(0, c(itermcmc, N, DIM))

    shapeSigma20 <- 1
    scaleSigma20 <- 1
    scaleSigma20inv <- 1/scaleSigma20
    shapeSigma2 <- Ng/2 + shapeSigma20
    CovXi0 <- 100*diag(2)
    PrecXi0 <- solve(CovXi0)
    CovGamma0 <- 100*diag(K1X*DIM)
    PrecGamma0 <- solve(CovGamma0)
    NuSigma <- N + DIM
    ONES <- matrix(1, nrow = N, ncol = 1)

    savepvs <- sort(sample((burnin+1):itermcmc, npv))
    datalist <- vector('list', npv)
    names(datalist) <- paste0('Iteration', savepvs)

    for(ii in 1:itermcmc){
        for(iii in 1:thin){
            # (1)
            MU <- THETA%*%t(ALPHA) - ONES%*%BETA
            for(j in 1:J){
                FA <- pnorm(KAPPA[[j]][YPL1[, j]] - MU[, j])
                FB <- pnorm(KAPPA[[j]][YPL2[, j]] - MU[, j])
                YLAT[, j] <- MU[, j] + qnorm(runif(N)*(FB - FA) + FA)
            }
            # (2)
            for(dim in 1:DIM){
                XITEM <- cbind(THETA[, dim], -1)
                for(j in jdim[[dim]]){
                    Covitem <- solve(crossprod(XITEM[YOBS[, j], ]) + PrecXi0)
                    muitem <- Covitem%*%crossprod(XITEM[YOBS[, j], ], YLAT[YOBS[, j], j])
                    XI[1, j] <- 0
                    while(XI[1, j] <= 0){
                        XI[, j] <- mvrnorm(1, muitem, Covitem)
                    }
                }
                ALPHA[jdim[[dim]], dim] <- XI[1, jdim[[dim]]]*(1/prod(XI[1, jdim[[dim]]]))^Jdiminv[dim]
                ALPHA[Q == 2, dim] <- 1
                if (!est.alpha) ALPHA[POSITEMORD, dim] <- 0.5
                BETA[jdim[[dim]]] <- XI[2, jdim[[dim]]] - sum(XI[2, jdim[[dim]]])/Jdim[dim]
            }
            # (3)
            for(j in POSITEMORD){
                maxTau <- ucminf(par = TAU[[j]], fn = lposttau, Yj = Y[YOBS[, j], j], Qj = QMI2[j],
                                 alpha = ALPHA[j, jdim2[j]], beta = BETA[j], Theta = THETA[YOBS[, j], jdim2[j]], hessian = 1)
                hatTau <- maxTau$par
                InvhessTau <- solve(maxTau$hessian)
                TAUC <- rmvt(1, delta = hatTau, sigma = InvhessTau, df = tdf)
                ratio <- min(1, exp(
                    -lposttau(TAUC, Y[YOBS[, j], j], QMI2[j], ALPHA[j, jdim2[j]], BETA[j], THETA[YOBS[, j], jdim2[j]]) +
                        lposttau(TAU[[j]], Y[YOBS[, j], j], QMI2[j], ALPHA[j, jdim2[j]], BETA[j], THETA[YOBS[, j], jdim2[j]]) -
                        dmvt(TAUC, delta = hatTau, sigma = InvhessTau, df = tdf, log = TRUE) +
                        dmvt(TAU[[j]], delta = hatTau, sigma = InvhessTau, df = tdf, log = TRUE)
                ))
                if(runif(1) < ratio){
                    TAU[[j]] <- TAUC
                    KAPPA[[j]][3:Q[j]] <- cumsum(exp(TAUC))
                }
            }
            # (4)
            for(i in 1:N){
                Covtheta <- solve(crossprod(ALPHA[YOBS[i, ], ]) + solve(SIGMA))
                mutheta <- Covtheta%*%(crossprod(ALPHA[YOBS[i, ], ], YLAT[i, YOBS[i, ]] +
                                                     BETA[YOBS[i, ]]) + solve(SIGMA)%*%t(GAMMA)%*%XDM[i, ])
                THETA[i, ] <- mvrnorm(1, mutheta, Covtheta)
            }
            # (5)
            Covgamma <- solve(solve(SIGMA)%x%XX + PrecGamma0)
            mugamma <- Covgamma%*%((solve(SIGMA)%x%diag(K1X))%*%matrix(XDMt%*%THETA))
            GAMMA <- matrix(mvrnorm(1, mugamma, Covgamma), nrow = K1X)
            XGAMMA <- XDM%*%GAMMA
            # (6)
            VSigma <- crossprod(THETA - XGAMMA) + diag(DIM)
            SIGMA <- rwishart(NuSigma, chol2inv(chol(VSigma)))

            if(ANYXMIS){
                # (7)
                X2IMP[, XcolsTHETA] <- THETA
                X <- seqcart(X2IMP, xmisord, XOBS, XMIS, cartctrl1, cartctrl2)
                X <- X[, -XcolsTHETA]
                XDM <- model.matrix(~., X)
                XDMt <- t(XDM)
                XX <- crossprod(XDM)
                if(saveiter){
                    PVs[[whichpv]] <- cbind(PVs[[whichpv]], X)
                }
            }

            Gamma[ii, ] <- c(GAMMA)
            Sigma2[ii] <- SIGMA
            Alpha[ii, , ] <- ALPHA
            Beta[ii, ] <- BETA
            Kappa[ii, ] <- unlist(lapply(KAPPA[!ITEMBIN], function(x){
                return(x[-c(1, 2, length(x))])
            }))
            Theta[ii, , ] <- THETA
            # save MCMC draws
            if (ii %in% savepvs) {
                sel <- which(names(datalist) == paste0('Iteration', ii))
                if (!is.null(X)) {
                    datalist[[sel]] <- data.frame(ID_t = ID_t, X)
                    for (w in seq(length(waves))) {
                        datalist[[sel]][[paste0("PV", waves[w])]] <- THETA[, w]
                    }
                } else {
                    datalist[[sel]] <- data.frame(paste0("PV", waves[1]) = THETA[, 1])
                    for (w in seq(2, length(waves))) {
                        datalist[[sel]][[paste0("PV", waves[w])]] <- THETA[, w]
                    }
                }
            }
        }
    }
    bi <- 1:burnin
    names(datalist) <- NULL
    EAPs <- data.frame(ID_t = ID_t, EAP = t(apply(Theta[-bi, ,], 1, colMeans)))
    regr.coeff <- colMeans(Gamma[-bi,])
    VAR <- mean(apply(Theta[-bi, ,], 1, function(x) apply(x, 2, var)))

    out <- list(datalist = datalist, EAP = EAPs, regr.coeff = regr.coeff, VAR = VAR)

    return(out)

}
