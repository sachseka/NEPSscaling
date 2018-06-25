## complement control lists for missing data handling
## and plausible values estimation

complement_control_lists <- function (c_EAP, c_WLE, c_I, c_M, c_C, c_T, nc) {
    res <- list()

  # default values
    control_EAP <- FALSE
    control_WLE <- FALSE
    if (!is.null(nc)) {
        control_IND <- list(vars = NULL, pca.data = FALSE, varex = 0.90)
        control_MMI <- list(nmi = 10L, method = NULL, where = NULL,
                            visitSequence = NULL, post = NULL, blots = NULL,
                            defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                            maxit = 10, printFlag = FALSE, seed = NA,
                            data.init = NULL)
    } else {
        control_IND <- control_MMI <- NULL
    }
    control_CART = list(itermcmc = 10000, burnin = 2000, thin = 1, tdf = 10,
                        cartctrl1 = 5, cartctrl2 = 0.0001)
    control_TAM = list(ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                       theta.model=FALSE, np.adj=8, na.grid = 5)

    if (!is.null(c_EAP)) control_EAP <- c_EAP
    if (!is.null(c_WLE)) control_WLE <- c_WLE
    control_IND[names(c_I)] <- c_I
    control_MMI[names(c_M)] <- c_M
    control_CART[names(c_C)] <- c_C
    control_TAM[names(c_T)] <- c_T

    res[['EAP']] <- control_EAP
    res[['WLE']] <- control_WLE
    res[['IND']] <- control_IND
    res[['MMI']] <- control_MMI
    res[['CART']] <- control_CART
    res[['TAM']] <- control_TAM


    return(res)
}
