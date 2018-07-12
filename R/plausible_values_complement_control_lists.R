## complement control lists for missing data handling
## and plausible values estimation

complement_control_lists <- function (c_EAP, c_WLE, c_nmi, c_C, c_T) {
    res <- list()

  # default values
    control_EAP <- FALSE
    control_WLE <- FALSE
    control_nmi <- 10L
    control_Bayes = list(itermcmc = 10000, burnin = 2000, thin = 1, tdf = 10,
                        cartctrl1 = 5, cartctrl2 = 0.0001)
    control_ML = list(ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                       theta.model=FALSE, np.adj=8, na.grid = 5)

    if (!is.null(c_EAP)) control_EAP <- c_EAP
    if (!is.null(c_WLE)) control_WLE <- c_WLE
    if (!is.null(c_nmi)) control_nmi <- c_nmi
    control_Bayes[names(c_C)] <- c_C
    control_ML[names(c_T)] <- c_T

    res[['EAP']] <- control_EAP
    res[['WLE']] <- control_WLE
    res[['nmi']] <- control_nmi
    res[['Bayes']] <- control_Bayes
    res[['ML']] <- control_ML


    return(res)
}
