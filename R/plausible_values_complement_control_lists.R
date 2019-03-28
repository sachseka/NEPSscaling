## complement control lists for missing data handling
## and plausible values estimation

complement_control_lists <- function (c_EAP, #c_WLE,
                                      c_C, c_T) {
    res <- list()

  # default values
    control_EAP <- FALSE
    #control_WLE <- FALSE
    control_Bayes = list(itermcmc = 10000, burnin = 2000, thin = 1, tdf = 10,
                        cartctrl1 = 5, cartctrl2 = 0.0001, est.alpha = FALSE)
    control_ML = list(nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                       theta.model = FALSE, np.adj = 8, na.grid = 5,
                      itermcmc = 100, burnin = 50, thin = 1, tdf = 10,
                      cartctrl1 = 5, cartctrl2 = 0.0001)

    if (!is.null(c_EAP)) control_EAP <- c_EAP
    #if (!is.null(c_WLE)) control_WLE <- c_WLE
    control_Bayes[names(c_C)] <- c_C
    control_ML[names(c_T)] <- c_T

    res[['EAP']] <- control_EAP
    #res[['WLE']] <- control_WLE
    res[['Bayes']] <- control_Bayes
    res[['ML']] <- control_ML


    return(res)
}
