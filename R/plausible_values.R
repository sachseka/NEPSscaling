#' Function for estimating plausible values with NEPS competence data and NEPS
#' scaled item parameters
#'
#' @param SC           numeric. The starting cohort used for the analysis is
#' indicated by an integer value (e.g., starting cohort one = 1).
#' @param domain       string. The competence domain of interest is indicated
#' by the domain abbreviation as used in the variable names (see Fuß, D., Gnambs,
#' T., Lockl, K., & Attig, M., 2016).
#' @param wave         numeric. The wave of competence testing is indicated by
#' an integer value (e.g., wave one = 1).
#' @param method       string. The method used for missing data handling in covariates
#' (IND: missing indicators, MMI: multiple multiple imputation, CART: sequential
#' classification and regression trees).
#' @param path         file path leading to the location of the competence data
#' @param filetype     filetype of competence data (the NEPS RDC provides SPSS
#' and Stata data files)
#' @param bgdata       data frame containing background variables. Categorical
#' variables have to be specified as factors. If \code{bgdata = NULL}, plausible
#' values are estimated without a background model.
#' @param npv          number of plausible values to be estimated; it defaults
#' to 10.
#' @param nvalid       minimum number of responses test takers had to give for
#' inclusion into the estimation process. NEPS default is at least three.
#' @param longitudinal logical. TRUE indicating that a model based on linked
#' item difficulties (without correction for position effects) is to be
#' estimated. Defaults to FALSE.
#' @param rotation     logical. TRUE indicating that the competence scores
#' should be corrected for the rotation design of the booklets. Defaults to
#' TRUE. If both longitudinal and rotation are TRUE, rotation is set to FALSE
#' automatically.
#' @param control      list of additional options. If \code{EAP = TRUE} or
#' \code{WLE = TRUE}, the EAPs or WLEs will be returned as well. Furthermore,
#' additional control options for IND are \code{varex} (the amount of
#' variance explained by the retained PCA factors), \code{pca.data} (a logical;
#' if TRUE, no plausible  values are estimated, but the data to estimate them
#' is returned) and \code{vars} (a vector of variable names that are not to be
#' included in the PCA). Additional control options for MMI consist of \code{nmi}
#' (an integer value specifying the number of multiple imputations; 10 by default)
#' and further options for mice::mice(). Analogously, CART options and TAM options
#' (used for plausible values estimation in methods IND and MMI) can be specified.
#' For further information see the documentations of LaRA::mglrm() for CART and
#' TAM::tam.pv() for TAM.
#'
#' @return \code{plausible_values()} returns an object of class \code{pv.obj}
#' containing \code{npv} plausible values with the data set used
#' for estimating them as well as WLEs and EAPs and their respective standard
#' error for the specified domain, wave and starting cohort of the NEPS.
#'
#' @references Albert, J. H. (1992). Bayesian estimation of normal ogive item
#' response curves using Gibbs sampling. \emph{Journal of Educational
#' Statistics}, \emph{17}(3), 251-269.
#' @references Azur, M. J., Stuart, E. A., Frangakis, C., & Leaf, P. J. (2011).
#' Multiple imputation by chained equations: what is it and how does it work?
#' \emph{International Journal of Methods in Psychiatric Research, 20}(1), 40-49.
#' @references Blossfeld, H.-P., Rossbach, H.-G., & von Maurice,  J. (Eds.) (2011).
#' Education as a Lifelong Process - The German National Educational Panel Study
#' (NEPS). \emph{Zeitschrift fuer Erziehungswissenschaft, Sonderheft 14}.
#' @references Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for
#' missing data via sequential regression trees. \emph{American Journal of
#' Epidemiology}, \emph{172}(9), 1070-1076.
#' @references Mislevy, R. J. (1991). Randomization-based inference about latent
#' variables from complex samples. \emph{Psychometrika, 56}(2), 177-196.
#' @references Pohl, S., & Carstensen, C. H. (2012). NEPS technical report -
#' Scaling the data of the competence tests.
#' @references Scharl, A., Carstensen, C. H., & Gnambs, T. (2018).
#' \emph{Estimating Plausible Values with NEPS Data: An Example Using Reading
#' Competence in Starting Cohort 6 (NEPS Survey Paper No. XX)}. Bamberg: Leibniz
#' Institute for Educational Trajectories, National Educational Panel Study.
#' @references Tanner, M. A., & Wong, W. H. (1987). The calculation of posterior
#' distributions by data augmentation. \emph{Journal of the American Statistical
#' Association}, \emph{82}(398), 528-549.
#' @references Weirich, S., Haag, N., Hecht, M., Boehme, K., Siegle, T., & Luedtke,
#' O. (2014). Nested multiple imputation in large-scale assessments.
#' \emph{Large-Scale Assessments in Education, 2}(1), 9.
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' library(NEPStools)
#' library(foreign)
#' ## read in data object for conditioning variables
#' data(bg_data)
#' bg_data$gender <- as.factor(bg_data$gender)
#' ## define path to NEPS competence data
#' path <- 'Usr/NEPS-data/'
#' setwd(path)
#' ## save simulated target competencies to path
#' data(xTargetCompetencies_sim)
#' write.dta(dataframe = xTargetCompetencies_sim, file = 'SC6_xTargetCompetencies_sim.dta')
#' ## estimate default number of 10 plausible values
#' ## note: the example background data is completely observed!
#' result <- plausible_values(SC = 'SC6'
#'                            , domain = 'RE'
#'                            , wave = 'w3'
#'                            , bgdata = bg_data
#'                            , path = path
#'                            , method = 'IND'
#'                            , filetype = 'Stata'
#'                            )
#' }
#'
#' @note For an extensive example, see the accompanying NEPS Survey Paper.
#' The function will only work with NEPS data. To access NEPS data see
#' https://www.neps-data.de/en-us/datacenter/dataaccess.aspx.
#'
#' @importFrom stats prcomp
#'
#' @export

plausible_values <- function(SC,
    domain = c('MA','RE','SC','IC','LI','EF','NR','NT','OR','ST','BA','CD', 'GR'),
    wave,
    method = c("ML","Bayes"),
    path,
    filetype = c('SPSS','Stata'),
    bgdata = NULL,
    npv = 10L,
    longitudinal = FALSE,
    rotation = TRUE,
    nvalid = 3L,
    control = list(EAP = FALSE, WLE = FALSE, nmi = 10L,
                   Bayes = list(itermcmc = 10000, burnin = 2000, est.alpha = TRUE, thin = 1, tdf = 10,
                               cartctrl1 = 5, cartctrl2 = 0.0001),
                   ML = list(ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                              theta.model=FALSE, np.adj=8, na.grid = 5))
){
    .<-NULL
    # check and prepare arguments
    if (missing(SC)) stop("Starting cohort must be specified.")
    SC <- paste0("SC", SC)
    if (!longitudinal && missing(wave)) {
        stop("Wave must be specified for cross-sectional modeling.")
    } else
        wave <- paste0("w", wave)
    domain <- toupper(domain)
    domain <- match.arg(domain)
    method <- match.arg(method)
    filetype <- match.arg(filetype)
    if (missing(path)) stop("Path must be specified.")
    if (!is.character(path) || !grepl("/$",path)) stop("Path must be a character string and end in '/'.")
    if (longitudinal && (SC == "SC6" | SC == "SC5") & domain %in% c("IC", "SC", "BA", "EF")){
        longitudinal <- FALSE
        message(paste("No longitudinal data for:", SC, domain,"."))
        }
    if(longitudinal) {
        type <- 'long'
        if (SC == "SC6" && domain %in% c("RE", "MA")) {
            waves <- c("_w3", "_w9")
        }
        if (SC == "SC5" && domain %in% c("RE", "MA")) {
            waves <- c("_w1", "_w12")
        }
        if (SC == "SC4" && domain %in% c("RE", "MA", "IC")) {
            waves <- c("_w2", "_w7")
        }
    } else {
        type <- 'cross'
        waves <- ""
    }
    if(nvalid < 0) stop("nvalid must be non-negative.")
    if(is.null(item_labels[[SC]][[domain]][[wave]]))
        stop(paste("No competence data availabe for", SC, domain, wave,"."))

    # get competence data for SC and domain
    files <- list.files(path = path)
    if (filetype == 'SPSS') {
        data <- tryCatch({haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))},
                         error=function(e){
                             stop(paste0("Path ", path, " does not lead to competence files OR wrong file formats!"))
                         })
    } else {
        data <- tryCatch({haven::read_dta(file = paste0(path, files[grep('xTargetCompetencies', files)]))},
                         error=function(e){
                             stop(paste0("Path '", path, "' does not lead to competence files OR wrong file formats!"))
                         })
    }
    data <- data[order(data$ID_t), ]

    # selection of test takers
    if (longitudinal) {
        # taking part in waves xy
        if (SC == "SC6" & domain %in% c("RE", "MA"))
            sel <- names(data) %in% c(item_labels[[SC]][[domain]][["w3"]], item_labels[[SC]][[domain]][["w9"]])
        if (SC == "SC5" & domain %in% c("RE", "MA"))
            sel <- names(data) %in% c(item_labels[[SC]][[domain]][["w1"]], item_labels[[SC]][[domain]][["w12"]])
        if (SC == "SC4" & domain == "RE")
            sel <- names(data) %in% c(item_labels[[SC]][[domain]][["w2"]], item_labels[[SC]][[domain]][["w7"]])
        if (SC == "SC4" & domain == "MA")
            sel <- names(data) %in% c(item_labels[[SC]][[domain]][["w1"]], item_labels[[SC]][[domain]][["w7"]])
        if (SC == "SC4" & domain == "IC")
            sel <- names(data) %in% c(item_labels[[SC]][[domain]][["w1"]], item_labels[[SC]][[domain]][["w7"]])
        data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
    } else {
        data <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
        # reading has been tested twice for different samples in SC6:
        # estimating simultaneously might cause problems because of different
        # information available for background model (large quantities of missing data)
        if (SC == 'SC6' & domain == 'RE') {
            if (wave == 'w3') {
                data <- data[data$wave_w3 == 1, ]
            } else if (wave == 'w5') {
                data <- data[data$wave_w3 == 0 & data$wave_w5 == 1, ]
            }
        }
    }

    if(is.null(bgdata)) {
        ID_t <- data[, 'ID_t', drop = FALSE]
    } else {
        if (!is.data.frame(bgdata)) {
            stop('bgdata must be a data.frame.')
        }
        if (is.null(bgdata$ID_t)) {
            stop('ID_t must be included in bgdata.')
        }
        bgdata <- bgdata[order(bgdata$ID_t), ]

        if (nvalid > 0) {
            bgdata <- bgdata[bgdata$ID_t %in% data$ID_t, ]
            data <- data[data$ID_t %in% bgdata$ID_t, ]
            bgdata <- bgdata[order(bgdata$ID_t), ]
        } else {
            # append subjects in background data that did not take the competence tests
            data <- data[data$ID_t %in% bgdata$ID_t, ]
            data <- suppressWarnings(dplyr::bind_rows(data, bgdata[!(bgdata$ID_t %in% data$ID_t), 'ID_t', drop = FALSE]))
            data <- data[order(data$ID_t), ]
        }
        ID_t <- bgdata[, "ID_t", drop = FALSE]

        # list of categorical variables
        fac <- unlist(lapply(bgdata, is.factor))
        categorical <- names(bgdata)[fac]
    }
    n.valid <- dplyr::bind_cols(data[, 'ID_t', drop = FALSE], data.frame(valid = rep(0, nrow(data))))
    n.valid$valid <- rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]]))

    if (longitudinal) {
        rotation <- FALSE
        if (method == "ML")
            Q <- qmat(SC, domain)
    } else {
        Q <- NULL
        if (rotation) {

            # TODO: position auf Mehr-Dim- und >2-Kategorien-Fälle anpassen
            position <- data.frame(ID_t = data$ID_t, position = rep(NA, nrow(data)))

            # construct facet to correct for rotation design
            if (SC == 'SC1') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC2') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC3') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC4') {
                stop('Sorry, cross-sectional is not yet implemented.')
            } else if (SC == 'SC5') {
                if (wave == 'w1') {
                    position[, 'position'] <- data[, 'tx80211_w1']
                    position[!is.na(position$position) & (position$position == 126), 'position'] <- 0 # reading first
                    position[!is.na(position$position) & (position$position == 127), 'position'] <- 1 # maths first
                } else if (wave == 'w5') {
                    position[, 'position'] <- data[, 'tx80211_w5']
                    position[!is.na(position$position) & (position$position %in% c(330,332,334)), 'position'] <- 0 # sc first
                    position[!is.na(position$position) & (position$position %in% c(331,333,335)), 'position'] <- 1 # ict first
                }
            } else if (SC == 'SC6') {
                if (wave == 'w3') {
                    position[, 'position'] <- data[, 'tx80211_w3']
                    position[!is.na(position$position) & (position$position %in% c(122,124)), 'position'] <- 0 # maths first
                    position[!is.na(position$position) & (position$position %in% c(123,125)), 'position'] <- 1 # reading first
                } else if (wave == 'w5') {
                    position[, 'position'] <- data[, 'tx80211_w5']
                    position[!is.na(position$position) & position$position == 247, 'position'] <- 0 # science first
                    position[!is.na(position$position) & position$position == 248, 'position'] <- 1 # ict first
                    # reading first (i.e. "new" SC6 sample) should be marked with 249 (not found in SUF)
                    if (domain == "RE")
                        position[is.na(position$position), 'position'] <- 3 # reading first; should not occur for IC/SC tests
                }
            }
            # possible NAs in position variable treated as third group
            position[is.na(position$position), "position"] <- 2
            # format position effect information
            position <- position[, 2, drop = FALSE]
            rotation <- length(unique(position$position)) > 1 # T: with rotation, F: without rotation
        }
    }

    # test data
    `%>%` <- dplyr::`%>%`
    if (longitudinal) {
        resp <- list()
        if (SC == "SC6" & domain == "RE") {
            resp[[1]] <- data[, names(data) %in% c("ID_t", item_labels[[SC]][[domain]][["w3"]])]
            resp[[2]] <- data[, names(data) %in% c("ID_t", item_labels[[SC]][[domain]][["w9"]])]
        } else {
            for (i in seq(length(item_labels[[SC]][[domain]])))
                resp[[i]] <- data[, names(data) %in% c("ID_t", item_labels[[SC]][[domain]][[i]])]
        }
        resp <- resp %>% Reduce(function(df1,df2) dplyr::full_join(df1,df2,by="ID_t"), .)
        resp$ID_t <- NULL
    } else
        resp <- data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]]

    # check for Partial Credit Items
    PCM <- max(apply(resp, 2, max, na.rm = TRUE)) > 1

    # complement control lists
    res <- complement_control_lists(control$EAP, control$WLE, control$nmi,
                                    control$Bayes, control$ML)
    control$EAP <- res$EAP
    control$WLE <- res$WLE
    control$nmi <- res$nmi
    control$Bayes <- res$Bayes
    control$ML <- res$ML



    # estimation with ML
    if (method == "ML") {

        # multiple imputation of missing covariate data
        if (!is.null(bgdata) && any(is.na(bgdata))){
            imp <- CART(X = bgdata, itermcmc = control$Bayes$itermcmc,
                        burnin = control$Bayes$burnin, nmi = control$nmi,
                        thin = control$Bayes$thin, cartctrl1 = control$Bayes$cartctrl1,
                        cartctrl2 = control$Bayes$cartctrl2)
        } else imp <- NULL

        if(PCM) {
            res <- adjustments_PCM(resp, SC, if (longitudinal) gsub("_", "", waves) else wave, domain)
            resp <- res$resp
            ind <- res$ind

            if (rotation) {
                res <- matrices_with_rotation(resp, position, ind)
                A <- res$A
                B <- res$B
                resp <- res$resp2
            } else {
                B <- TAM::designMatrices(modeltype = 'PCM',
                                         Q = Q,
                                         resp = resp)$B
                B[ind, , ] <- 0.5 * B[ind, , ]
            }
        } else {
            if (rotation) {
            res <- TAM::designMatrices.mfr(resp = resp, facets = position,
                                           formulaA = ~ 0 + item + position,
                                           constraint = "cases", progress = FALSE)
            A <- res$A$A.3d
            B <- res$B$B.3d
            resp <- res$gresp$gresp.noStep
            }
        }

        pvs <- list()
        EAP.rel <- list()
        regr.coeff <- list()
        variance <- list()
        eap <- replicate(control$nmi,
                         matrix(c(ID_t$ID_t, rep(0, 2*length(waves)*nrow(resp))),
                                ncol = (1 + 2*length(waves)),
                                nrow = nrow(resp)),
                         simplify = FALSE)
        for (i in 1:control$nmi) {
            if (!is.null(imp)) {
                bgdatacom <- imp[[i]]
                bgdatacom$ID_t <- NULL
                bgdatacom <- apply(bgdatacom, 2, as.numeric)
            }

            # repeat {
#
#                 mod <- tryCatch(TAM::tam.mml(resp = resp,
#                                              Y = if (is.null(bgdata)) NULL else if (is.null(imp)) bgdata[, -which(names(bgdata) == "ID_t")] else bgdatacom,
#                                              xsi.fixed = if (!longitudinal && SC == "SC6" && domain == "RE" && wave == "w5") item_diff_SC6_RE_w3 else NULL,
#                                              A = if (rotation) A else NULL, B = if (PCM) B else NULL, Q = Q, verbose = FALSE),
#                                 error=function(e){return(NA)})
#                 if(any(is.na(mod)) && !is.null(bgdata)) {
#                     bgdatacom <- CART(X = bgdata, itermcmc = control$Bayes$itermcmc,
#                                       burnin = control$Bayes$burnin, nmi = 1,
#                                       thin = control$Bayes$thin, cartctrl1 = control$Bayes$cartctrl1,
#                                       cartctrl2 = control$Bayes$cartctrl2)[[1]]
#                     bgdatacom$ID_t <- NULL
#                     bgdatacom <- apply(bgdatacom, 2, as.numeric)
#                 } else {
#                     break
#                 }
#             }
            mod <- TAM::tam.mml(resp = resp,
                                Y = if (is.null(bgdata)) NULL else if (is.null(imp)) bgdata[, -which(names(bgdata) == "ID_t")] else bgdatacom,
                                xsi.fixed = if (!longitudinal && SC == "SC6" && domain == "RE" && wave == "w5") item_diff_SC6_RE_w3 else NULL,
                                A = if (rotation) A else NULL, B = if (PCM || rotation) B else NULL, Q = Q, verbose = FALSE)

            pmod <- TAM::tam.pv(mod, nplausible = npv, ntheta = control$ML$ntheta, normal.approx = control$ML$normal.approx,
                                samp.regr = control$ML$samp.regr, theta.model = control$ML$theta.model,
                                np.adj = control$ML$np.adj, na.grid = control$ML$na.grid)
            eap[[i]][, -1] <- as.matrix(mod$person[, grep("EAP", names(mod$person))])
            colnames(eap[[i]]) <- c('ID_t',  paste0(rep(c('eap','se'), length(waves)), rep(waves, each = 2)))
            pvs[[i]] <- TAM::tampv2datalist(pmod,
                                            Y = if (is.null(bgdata) || is.null(imp)) ID_t else cbind(ID_t, bgdatacom),
                                            pvnames = paste0("PV", waves))
            EAP.rel[[i]] <- mod$EAP.rel
            regr.coeff[[i]] <- mod$beta
            variance[[i]] <- mod$variance
        }
        if (control$WLE && !longitudinal) {
            wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
            wle <- matrix(wmod$theta, nrow = length(wmod$theta), ncol = 1)
            wle <- cbind(ID_t, wle, wmod$error)
            colnames(wle) <- c('ID_t', 'wle', 'se')
        } else if (control$WLE && longitudinal) {
            wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
            wle <- do.call(cbind, list(ID_t, wmod[grep("theta|error", colnames(wmod))]))
            colnames(wle) <- c('ID_t',  paste0(rep(c('wle','se'), length(waves)), rep(waves, each = 2)))
        }
        datalist <- list()
        d <- 1
        for (i in 1:control$nmi) {
            for (j in 1:npv) {
                datalist[[d]] <- pvs[[i]][[j]]
                d <- d + 1
            }
        }
        pvs <- NULL
        ind <- sample(1:length(datalist), npv)
        datalist <- datalist[ind]
        for (i in 1:npv) {
            datalist[[i]] <- datalist[[i]][, -which(colnames(datalist[[i]]) %in% c('pid', 'pweights', 'test_position'))]
            datalist[[i]] <- datalist[[i]] %>% dplyr::select(ID_t, dplyr::everything())
        }
    }



    # estimation using MCMC
    if (method == "Bayes") {

        # collapse categories with N < 200
        if(PCM) {
            res <- adjustments_PCM(resp, SC, if (longitudinal) gsub("_", "", waves) else wave, domain)
            resp <- res$resp
        }

        if (rotation) {
            S <- position[, 1] + 1
        } else {
            S <- NULL
        }

        if (!is.null(bgdata)) bgdata <- data.frame(bgdata)
        resp <- data.frame(resp)
        if (longitudinal){
            datalist <- plausible_values_mdlrm(Y = resp, X = bgdata, npv = npv,
                                               itermcmc = control$Bayes$itermcmc, burnin = control$Bayes$burnin,
                                               thin = control$Bayes$thin, tdf = control$Bayes$tdf,
                                               est.alpha = control$Bayes$est.alpha,
                                               cartctrl1 = control$Bayes$cartctrl1,
                                               cartctrl2 = control$Bayes$cartctrl2,
                                               SC = SC, domain = domain, waves = waves)
        } else {
            datalist <- plausible_values_mglrm(Y = resp, X = bgdata, S = S, npv = npv,
                            itermcmc = control$Bayes$itermcmc, burnin = control$Bayes$burnin,
                            thin = control$Bayes$thin, tdf = control$Bayes$tdf,
                            est.alpha = control$Bayes$est.alpha,
                            cartctrl1 = control$Bayes$cartctrl1,
                            cartctrl2 = control$Bayes$cartctrl2)
        }
        if (is.null(bgdata)) {
            for (i in seq(length(datalist$datalist))) {
                datalist$datalist[[i]]$ID_t <- ID_t$ID_t
                datalist$datalist[[i]] <- datalist$datalist[[i]] %>% dplyr::select(ID_t, dplyr::everything())
            }
        }
    }


    # linear transformation of PVs to pre-defined scale
    if (longitudinal) {
        if (method == "Bayes") {
            VAR <- datalist$VAR
            MEAN <- colMeans(datalist$EAP[, grep("EAP", names(datalist$EAP))])
        } else {
            VAR <- colMeans(do.call(rbind, lapply(variance, FUN = function (x) diag(x))))
            MEAN <- colMeans(do.call(rbind, lapply(eap, FUN = function (x) colMeans(x[, seq(2, (1+2*length(waves)), 2)]))))
        }
        names(VAR) <- gsub("_", "", waves)
        names(MEAN) <- gsub("_", "", waves)
    } else {
        if (method == "Bayes") {
            VAR <- sum(datalist$VAR)
            MEAN <- mean(datalist$EAP$EAP)
        } else {
            VAR <- mean(unlist(variance))
            MEAN <- mean(sapply(eap, FUN = function(x) mean(x[, 2])))
        }
    }
    pv <- scale_pv(pv = if (method == "Bayes") datalist$datalist else datalist,
                   SC = SC, domain = domain, type = type, wave = wave,
                   VAR = VAR, MEAN = MEAN)

    # output
    res <- list()
    res[['SC']] <- SC
    res[['domain']] <- domain
    res[['wave']] <- wave
    res[['method']] <- method
    res[['type']] <- type
    res[['rotation']] <- ifelse(rotation, 'Corrected For Test Position',
                                'No Correction For Test Position')
    res[['nvalid']] <- nvalid
    res[['model']] <- if (method == "ML") ifelse(PCM, 'Partial Credit Model', 'Rasch Model') else "Graded Response Model"
    res[['n.valid']] <- n.valid
    res[['npv']] <- npv
    res[['control']] <- control
    if (rotation)
        res[['position']] <- data.frame(ID_t, position)
    if (longitudinal){
        res[["variance.theta"]] <- sapply(meanvar[[SC]][[domain]], FUN = function(x) x[[type]][[2]])
        res[["mean.theta"]] <- sapply(meanvar[[SC]][[domain]], FUN = function(x) x[[type]][[1]])
    } else {
        res[["variance.theta"]] <- meanvar[[SC]][[domain]][[wave]][[type]][2]
        res[["mean.theta"]] <- meanvar[[SC]][[domain]][[wave]][[type]][1]
    }
    res[["variance.PV"]] <- VAR
    res[["mean.PV"]] <- MEAN
    res[['pv']] <- pv

    if (method == "Bayes") {
        if (control$EAP) res[["eap"]] <- datalist$EAP
        res[["regr.coeff"]] <- datalist$regr.coeff
        if (control$Bayes$est.alpha) res[["alpha"]] <- datalist$alpha
    } else if (method == "ML") {
        if (control$EAP) {
            res[['eap']] <- eap
        }
        if (control$WLE)
            res[['wle']] <- wle
        res[['EAP.rel']] <- EAP.rel
        res[["regr.coeff"]] <- regr.coeff
    }
    class(res) <- "pv.obj"
    return(res)
}
