#' Plausible values imputation for longitudinal data
#'
#' This function estimates a multidimensional item response model (nominal and
#' ordinal data). Missing values in the response data are ignored, CART is used
#' to impute missing values in the covariate data.
#' @param Y test responses
#' @param X covariates
#' @param npv number of plausible values to draw for each respondent.
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
#' @importFrom stats model.matrix runif rnorm pnorm qnorm predict
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvt dmvt dmvnorm
#' @importFrom ucminf ucminf
#' @importFrom rpart rpart rpart.control
#' @importFrom stats var
#' @importFrom utils setTxtProgressBar txtProgressBar

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
        X <- X[, -which(names(X)=='ID_t')]
    }
    Y <- data.frame(Y)
    YPL1 <- Y + 1
    YPL2 <- Y + 2

    N <- nrow(Y)
    J <- ncol(Y)
    if (SC == "SC6") {
        if (domain == "RE") {
            DIM <- 2
            Jdim <- c(30, 36)
            jdim <- list(1:30, 31:66)
            jdim2 <- c(rep(1, 30), rep(2, 36))
        } else if (domain == "MA") {
            DIM <- 2
            Jdim <- c(21, 52)
            jdim <- list(1:21, 22:73)
            jdim2 <- c(rep(1, 21), rep(2, 52))
        }
        wave <- "w9"
    }
    if (SC == "SC5") {
        if (domain == "RE") {
            DIM <- 2
            Jdim <- c(28, 21)
            jdim <- list(1:28, 29:49)
            jdim2 <- c(rep(1, 28), rep(2, 21))
        }
        if (domain == "MA") {
            DIM <- 2
            Jdim <- c(20, 52)
            jdim <- list(1:20, 21:72)
            jdim2 <- c(rep(1, 20), rep(2, 52))
        }
        wave <- "w12"
    }
    if (SC == "SC4") {
        if (domain == "RE") {
            DIM <- 3
            Jdim <- c(31, 41, 36)
            jdim <- list(1:31, 32:72, 73:108)
            jdim2 <- c(rep(1, 31), rep(2, 41), rep(3, 36))
        }
        if (domain == "MA") {
            DIM <- 3
            Jdim <- c(22, 31, 52)
            jdim <- list(1:22, 23:53, 54:105)
            jdim2 <- c(rep(1, 22), rep(2, 31), rep(3, 52))
        }
        wave <- "w10"
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
    if (domain == "MA") {
        if (SC %in% c("SC4", "SC5", "SC6")) {
            BETA[xsi.fixed$long$MA[[SC]][,1]] <- xsi.fixed$long$MA[[SC]][, 2]
        }
    } else if (domain == "RE") {
        if (SC %in% c("SC4", "SC5", "SC6")) {
            BETA[xsi.fixed$long$RE[[SC]][,1]] <- xsi.fixed$long$RE[[SC]][, 2]
        }
    }
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

    Gamma <- matrix(0, nrow = itermcmc, ncol = K1X*DIM)
    colnames(Gamma) <- paste0(rep(colnames(XDM), DIM), rep(1:DIM, each=ncol(XDM)))
    Sigma2 <- matrix(0, nrow = itermcmc, ncol = DIM)
    Alpha <- array(0, c(itermcmc, J, DIM))
    Beta <- matrix(0, nrow = itermcmc, ncol = J)
    # Kappa <- matrix(0, nrow = itermcmc, ncol = sum(QMI2))
    # accTau <- rep(0, J)
    # names(accTau) <- colnames(Y)
    Theta <- array(0, c(itermcmc, N, DIM))

    CovXi0 <- 100*diag(2)
    PrecXi0 <- solve(CovXi0)
    CovGamma0 <- 100*diag(K1X*DIM)
    PrecGamma0 <- solve(CovGamma0)
    NuSigma <- N + DIM
    ONES <- matrix(1, nrow = N, ncol = 1)

    savepvs <- sort(sample((burnin+1):itermcmc, npv))
    datalist <- vector('list', npv)
    names(datalist) <- paste0('Iteration', savepvs)

    pb <- txtProgressBar(min = 0, max = itermcmc, style = 3)
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
            if (domain == "MA") {
                if (SC %in% c("SC4", "SC5", "SC6")) {
                    BETA[xsi.fixed$long$MA[[SC]][, 1]] <- xsi.fixed$long$MA[[SC]][, 2]
                }
            } else if (domain == "RE") {
                if (SC %in% c("SC4", "SC5", "SC6")) {
                    BETA[xsi.fixed$long$RE[[SC]][, 1]] <- xsi.fixed$long$RE[[SC]][, 2]
                }
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
            }

            Gamma[ii, ] <- c(GAMMA)
            Sigma2[ii, ] <- diag(SIGMA)
            Alpha[ii, , ] <- ALPHA
            Beta[ii, ] <- BETA
            # Kappa[ii, ] <- unlist(lapply(KAPPA[!ITEMBIN], function(x){
            #     return(x[-c(1, 2, length(x))])
            # }))
            Theta[ii, , ] <- THETA
            # save MCMC draws
            if (ii %in% savepvs) {
                sel <- which(names(datalist) == paste0('Iteration', ii))
                if (!is.null(X)) {
                    datalist[[sel]] <- data.frame(ID_t = ID_t, X)
                    for (w in seq(length(waves))) {
                        datalist[[sel]][[paste0("PV", waves[[w]])]] <- THETA[, w]
                    }
                } else {
                    datalist[[sel]] <- data.frame(THETA[, 1])
                    names(datalist[[sel]]) <- paste0("PV", waves[[1]])
                    for (w in seq(2, length(waves))) {
                        datalist[[sel]][[paste0("PV", waves[[w]])]] <- THETA[, w]
                    }
                }
            }
        }
        setTxtProgressBar(pb, ii)
    }
    close(pb)
    bi <- 1:burnin
    names(datalist) <- NULL
    if (!is.null(X))
        EAPs <- data.frame(ID_t = ID_t, EAP = apply(Theta[-bi,,], c(2,3), mean))
    else
        EAPs <- data.frame(EAP = apply(Theta[-bi,,], c(2,3), mean))
    alpha <- apply(Alpha[-bi, ,], c(2,3), mean)
    if (ncol(Gamma) > 1)
        regr.coeff <- colMeans(Gamma[-bi, ])
    else
        regr.coeff <- mean(Gamma[-bi, ])
    VAR <- colMeans(Sigma2[-bi, ])

    out <- list(datalist = datalist, EAP = EAPs, regr.coeff = regr.coeff, VAR = VAR, alpha = alpha)

    return(out)

}
