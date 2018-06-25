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
    method = c('IND','MMI','CART'),
    path,
    filetype = c('SPSS','Stata'),
    bgdata = NULL,
    npv = 10L,
    longitudinal = FALSE,
    rotation = TRUE,
    nvalid = 3L,
    control = list(EAP = FALSE, WLE = FALSE,
                   IND = list(varex = 0.90, pca.data = FALSE, vars = NULL),
                   MMI = list(nmi = 10L, method = NULL, where = NULL,
                              visitSequence = NULL, post = NULL,
                              defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                              maxit = 10, printFlag = FALSE, seed = NA,
                              blots = NULL, data.init = NULL),
                   CART = list(itermcmc = 10000, burnin = 2000, thin = 1, tdf = 10,
                               cartctrl1 = 5, cartctrl2 = 0.0001),
                   TAM = list(ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                              theta.model=FALSE, np.adj=8, na.grid = 5))
){
    # check and prepare arguments
    SC <- paste0("SC", SC)
    wave <- paste0("w", wave)
    domain <- toupper(domain)
    method <- toupper(method)
    method <- match.arg(method)
    filetype <- match.arg(filetype)
    if (!is.character(path) || !grepl("/$",path)) stop("Path must be a character string and end in '/'.")
    if(longitudinal) {type <- 'long'} else {type <- 'cross'}
    if(nvalid < 0) stop("nvalid must be non-negative.")

    # get item difficulties for respective starting cohort and domain
    xsi <- item_difficulties[[SC]][[domain]][[wave]][[type]]
    if (is.null(xsi)) {stop(paste('\nItem difficulties cannot be retrieved for', SC, domain, wave,
                                  '\nPlease check function arguments or see pv_impl()',
                                  '\nfor further information on implementation.'))}

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
    rm(files)
    data <- data[order(data$ID_t), ]

    # selection of test takers
    data <- data[rowSums(!is.na(data[, names(data) %in% rownames(xsi)])) >= nvalid, ]
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

    if(is.null(bgdata)) {
        if (method != 'CART') {
            changedFrom <- method
            method <- 'COM'
        }
        ID_t <- data[, 'ID_t', drop = FALSE]
    } else {
        if (!is.data.frame(bgdata)) {
            stop('bgdata must be a data.frame.')
        }
        if (is.null(bgdata$ID_t)) {
            stop('ID_t must be included in bgdata.')
        }
        bgdata <- bgdata[order(bgdata$ID_t), ]

        # change to appropriate missing data method
        missingInbgdata <- any(is.na(bgdata))
        if (!missingInbgdata & method == 'MMI') {
            method <- 'COM'
            changedFrom <- 'MMI'
            message("method == 'MMI': no missing data in bgdata. No multiple imputation performed. Method changed to 'COM'.")
        } else {
            changedFrom <- 'NULL'
        }

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

        # list of categorical variables
        fac <- unlist(lapply(bgdata, is.factor))
        categorical <- names(bgdata)[fac]
    }
    n.valid <- dplyr::bind_cols(data[, 'ID_t', drop = FALSE], data.frame(valid = rep(0, nrow(data))))
    n.valid$valid <- rowSums(!is.na(data[, names(data) %in% rownames(xsi)]))

    if (longitudinal) {
        if (rotation)
            message('Longitudinal competence estimates are not corrected for item position. rotation = FALSE')
        rotation <- FALSE
    } else {
        if (rotation) {

            position <- data.frame(ID_t = data$ID_t, position = rep(NA, nrow(data)))

            # construct facet to correct for rotation design
            if (SC == 'SC1') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC2') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC3') {
                stop('Sorry, not yet implemented.')
            } else if (SC == 'SC4') {
                stop('Sorry, not yet implemented.')
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
                        position[is.na(position$position), 'position'] <- 2 # reading first; should not occur for IC/SC tests
                    # assumption: all missing cases after filtering for valid cases on IC/SC variables actually belong to original sample
                }
            }
            # if (any(is.na(position$position))) { # distribute missing persons randomly
            #     position[is.na(position$position), 'position'] <-
            #         sample(unique(position$position[!is.na(position$position)]), sum(is.na(position$position)), replace = T)
            # }
            if (any(is.na(position$position))) {
                position <- position[!is.na(position$position), ]
                data <- data[data$ID_t %in% position$ID_t, ]
                if (!is.null(bgdata))
                    bgdata <- bgdata[bgdata$ID_t %in% position$ID_t, ]
            }

            # format position effect information
            position <- position[, 2, drop = FALSE]
            rotation <- length(unique(position$position)) > 1 # T: with rotation, F: without rotation
            if (!rotation) {message('There is no rotation design.')}
        } else {
            message("Cross-sectional IRT model without test position correction corresponds to longitudinal model.")
        }
    }

    # test data
    resp <- data[, names(data) %in% rownames(xsi)]
    if (method == 'IND') {
        # prepare criterion scaling
        wle <- all.wle[[SC]][[wave]][[type]][[domain]]
        if (is.null(wle))
            stop(paste('There is no competence data for:', SC, domain, wave))
        wle.data <- data[, c('ID_t', wle)]
    }
    rm(data)

    # check for Partial Credit Items
    PCM <- max(apply(resp, 2, max, na.rm = TRUE)) > 1

    # complement control lists
    res <- complement_control_lists(control$EAP, control$WLE, control$IND,
                                    control$MMI, control$CART, control$TAM,
                                    ncol(bgdata))
    control$EAP <- res$EAP
    control$WLE <- res$WLE
    control$IND <- res$IND
    control$MMI <- res$MMI
    control$CART <- res$CART
    control$TAM <- res$TAM
    rm(res)

    # estimation with missing indicator
    if (method == 'IND') {

        ID_t <- bgdata[, 'ID_t', drop = FALSE]

        # coding
        # all variables subjected to PCA
        if (is.null(control$IND$vars)) {
            bgdatac <- criterion_scaling(bgdata, wle, SC, wave, wle.data, categorical)
            if (length(categorical) > 0) {
                bgdatad <- dummy_coding(bgdata, categorical, ID_t)
                bgdata <- merge(bgdatac[, -which(names(bgdatac) == wle)], bgdatad)
                rm(bgdatac,bgdatad)
            } else {
                bgdata <- bgdatac[, -which(names(bgdatac) == wle)]
                rm(bgdatac)
            }
            # specified variables treated separately
        } else {
            if (all(control$IND$vars %in% names(bgdata))) {
                # prepare variables for PCA
                for_pca <- categorical[!(categorical %in% control$IND$vars)]
                bgdata.pca <- bgdata[, !(names(bgdata) %in% control$IND$vars)]
                if (length(for_pca) > 0) {
                    bgdatac <- criterion_scaling(bgdata.pca, wle, SC, wave, wle.data, for_pca)
                    bgdatad <- dummy_coding(bgdata.pca, for_pca, ID_t)
                    bgdata.pca <- merge(bgdatac[, -which(names(bgdatac) == wle)], bgdatad)
                    bgdatad <- bgdatac <- NULL
                } else {
                    bgdatac <- criterion_scaling(bgdata.pca, wle, SC, wave, wle.data, NULL)
                    bgdata.pca <- bgdatac[, -which(names(bgdatac) == wle)]
                    bgdatac <- NULL
                }
                # prepare specified variables
                categorical <- categorical[categorical %in% control$IND$vars]
                bgdata.vars <- bgdata[, c('ID_t', control$IND$vars)]
                if (length(categorical) > 0) {
                    bgdata.vars.c <- criterion_scaling(bgdata.vars, wle, SC, wave, wle.data, categorical)
                    bgdata.vars.d <- dummy_coding(bgdata.vars, categorical, ID_t)
                    bgdata.vars <- merge(bgdata.vars.c[, -which(names(bgdata.vars.c) == wle)], bgdata.vars.d)
                    bgdata.vars.c <- bgdata.vars.d <- NULL
                } else {
                    bgdata.vars.c <- criterion_scaling(bgdata.vars, wle, SC, wave, wle.data, NULL)
                    bgdata.vars <- bgdata.vars.c[, -which(names(bgdata.vars.c) == wle)]
                    bgdata.vars.c <- NULL
                }
                bgdata <- bgdata.pca
                bgdata.pca <- NULL
            } else {
                stop('control$IND$vars: specified variable not in background data.')
            }
        }
        rm(wle,wle.data,categorical,for_pca)

        # pca
        bgdata.pc <- prcomp(x = bgdata[, -which(names(bgdata) == 'ID_t')],
                            retx = TRUE, center = TRUE, scale. = TRUE)
        # choose components until criterion (variance explained) is met
        pc.varex <- (bgdata.pc$sdev^2)/sum(bgdata.pc$sdev^2)
        npc <- 1
        while (npc <= length(pc.varex)) {
            if (sum(pc.varex[1:npc]) >= control$IND$varex) {
                break
            }
            npc <- npc + 1
        }
        bgdata.pca <- bgdata.pc$x[, 1:npc]

        if (!is.null(control$IND$vars)) {
            bgdata.pca <- cbind(bgdata.pca, bgdata.vars[, -which(names(bgdata.vars) == 'ID_t')])
        }
        rm(bgdata.pc,pc.varex,npc,bgdata.vars)

        if (!control$IND$pca.data) {

            if(PCM) {
                res <- adjustments_PCM(resp, SC, wave, domain)
                resp <- res$resp
                ind <- res$ind

                if (rotation) {
                    mod <- PCM_with_rotation(xsi, resp, bgdata.pca, position, ind)
                } else {
                    Q <- matrix(1, nrow = ncol(resp), ncol = 1)
                    Q[ind, ] <- 0.5 * Q[ind, ]
                    mod <- TAM::tam.mml(resp = resp, Y = bgdata.pca,
                                        irtmodel = 'PCM2', xsi.fixed = xsi,
                                        Q = Q, verbose = FALSE)
                }
            } else {
                if (rotation) {
                    mod <- TAM::tam.mml.mfr(resp = resp, irtmodel = '1PL',
                                            formulaA = ~ 0 + item + position,
                                            Y = bgdata.pca, xsi.fixed = xsi,
                                            facets = position, verbose = FALSE)
                } else {
                    mod <- TAM::tam.mml(resp = resp, Y = bgdata.pca,
                                        irtmodel = '1PL', xsi.fixed = xsi,
                                        verbose = FALSE)
                }
            }
            rm(resp, xsi, position)
            bgdata.pca <- cbind(ID_t, bgdata.pca)

            if (control$WLE) {
                wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
                wle <- matrix(wmod$theta, nrow = length(wmod$theta), ncol = 1)
                wle <- cbind(ID_t, wle, wmod$error)
                colnames(wle) <- c('ID_t', 'wle', 'se')
            }
            pmod <- TAM::tam.pv(mod, nplausible = npv,
                                ntheta = control$TAM$ntheta,
                                normal.approx = control$TAM$normal.approx,
                                samp.regr = control$TAM$samp.regr,
                                theta.model = control$TAM$theta.model,
                                np.adj = control$TAM$np.adj,
                                na.grid = control$TAM$na.grid)
            datalist <- TAM::tampv2datalist(pmod, Y = bgdata, pvnames = 'PV')
            for (d in seq(length(datalist))) {
                datalist[[d]] <- datalist[[d]][, -which(colnames(datalist[[d]]) %in% c('pid', 'pweights'))]
            }
            if (control$EAP) {
                eap <- cbind(ID_t, mod$person[, c('EAP', 'SD.EAP')])
                colnames(eap) <- c('ID_t', 'eap', 'se')
            }
            EAP.rel <- mod$EAP.rel
        }
    }



    # estimation with MI as missing data handling
    if (method == 'MMI') {

        # multiple imputation of missing covariate data
        ID_t <- bgdata[, 'ID_t', drop = FALSE]
        bgdata <- bgdata[, -which(names(bgdata) == 'ID_t')]
        imp <- mice::mice(bgdata, m = control$MMI$nmi, maxit = control$MMI$maxit,
                          printFlag = control$MMI$printFlag,
                          method = control$MMI$method, where = control$MMI$where,
                          visitSequence = control$MMI$visitSequence,
                          post = control$MMI$post, defaultMethod = control$MMI$defaultMethod,
                          blots = control$MMI$blots, seed = control$MMI$seed,
                          data.init = control$MMI$data.init)

        if(PCM) {
            res <- adjustments_PCM(resp, SC, wave, domain)
            resp <- res$resp
            ind <- res$ind

            if (rotation) {
                res <- matrices_with_rotation(resp, position, ind)
                A <- res$A
                B <- res$B
                resp <- res$resp2
            } else {
                Q <- matrix(1, nrow = ncol(resp), ncol = 1)
                Q[ind, ] <- 0.5*Q[ind, ]
            }
            rm(ind,res)
        }

        pvs <- list()
        EAP.rel <- list()
        if (control$EAP)
            eap <- matrix(0, ncol = 2*control$MMI$nmi, nrow = nrow(resp))
        icol <- 1
        for (i in 1:control$MMI$nmi) {
            bgdatacom <- mice::complete(imp, i)
            if (length(categorical) > 0) {
                bgdatacom.d <- dummy_coding(bgdatacom, categorical, ID_t)
                bgdatacom <- cbind(bgdatacom[, -which(names(bgdatacom) %in% categorical)], bgdatacom.d)
            }
            bgdatacom$ID_t <- NULL
            bgdatacom <- apply(bgdatacom, 2, as.numeric)

            repeat {

                if (PCM) {
                    if (rotation) {
                        mod <- tryCatch(TAM::tam.mml(resp = resp, Y = bgdatacom, xsi.fixed = xsi,
                                                     A = A, B = B, verbose = FALSE),
                                        error=function(e){
                                            return(NA)
                                        })
                    } else {
                        mod <- tryCatch(TAM::tam.mml(resp = resp, Y = bgdatacom, irtmodel = 'PCM2',
                                                     xsi.fixed = xsi, Q = Q, verbose = FALSE),
                                        error=function(e){
                                            return(NA)
                                        })
                    }
                } else {
                    if (rotation) {
                        mod <- tryCatch(TAM::tam.mml.mfr(resp = resp, irtmodel = '1PL',
                                                         formulaA = ~ 0 + item + position, Y = bgdatacom,
                                                         xsi.fixed = xsi, facets = position,
                                                         verbose = FALSE),
                                        error=function(e){
                                            return(NA)
                                        })
                    } else {
                        mod <- tryCatch(TAM::tam.mml(resp = resp, Y = bgdatacom, irtmodel = '1PL',
                                                     xsi.fixed = xsi, verbose = FALSE),
                                        error=function(e){
                                            return(NA)
                                        })
                    }
                }

                if(any(is.na(mod))) {
                    bgdatacom <- mice::complete(mice::mice(bgdata, m = 1, maxit = control$MMI$maxit, printFlag = control$MMI$printFlag,
                                                           method = control$MMI$method, where = control$MMI$where,
                                                           visitSequence = control$MMI$visitSequence,
                                                           post = control$MMI$post, defaultMethod = control$MMI$defaultMethod,
                                                           blots = control$MMI$blots, seed = control$MMI$seed,
                                                           data.init = control$MMI$data.init))
                    if (length(categorical) > 0) {
                        bgdatacom.d <- dummy_coding(bgdatacom, categorical, ID_t)
                        bgdatacom <- cbind(bgdatacom[, -which(names(bgdatacom) %in% categorical)], bgdatacom.d)
                    }
                    bgdatacom$ID_t <- NULL
                    bgdatacom <- apply(bgdatacom, 2, as.numeric)
                } else {
                    break
                }
            }
            pmod <- TAM::tam.pv(mod, nplausible = npv, ntheta = control$TAM$ntheta, normal.approx = control$TAM$normal.approx,
                                samp.regr = control$TAM$samp.regr, theta.model = control$TAM$theta.model,
                                np.adj = control$TAM$np.adj, na.grid = control$TAM$na.grid)
            if (control$EAP)
                eap[, icol:(icol+1)] <- as.matrix(mod$person[, c('EAP', 'SD.EAP')])
            icol <- icol + 2
            pvs[[i]] <- TAM::tampv2datalist(pmod, Y = cbind(ID_t, bgdatacom),
                                            pvnames = 'PV')
            EAP.rel[[i]] <- mod$EAP.rel
        }
        rm(icol,imp,bgdata,bgdatacom,resp,xsi)
        if (control$WLE) {
            wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
            wle <- matrix(wmod$theta, nrow = length(wmod$theta), ncol = 1)
            wle <- cbind(ID_t, wle, wmod$error)
            colnames(wle) <- c('ID_t', 'wle', 'se')
        }
        if (control$EAP) {
            eap <- cbind(ID_t, eap)
            colnames(eap) <- c('ID_t', paste0(rep(c('eap.','se.'), control$MMI$nmi),
                                              rep(seq(1,control$MMI$nmi), each = 2)))
        }
        datalist <- list()
        d <- 1
        for (i in 1:control$MMI$nmi) {
            for (j in 1:npv) {
                datalist[[d]] <- pvs[[i]][[j]]
                d <- d + 1
            }
        }
        pvs <- NULL
        ind <- sample(1:length(datalist), npv)
        datalist <- datalist[ind]
        for (i in 1:npv) {
            names(datalist[[i]])[length(datalist[[i]])] <- 'PV'
            datalist[[i]] <- datalist[[i]][, -which(colnames(datalist[[i]]) %in% c('pid', 'pweights'))]
        }
    }



    # estimation using sequential CART and GRM
    if (method == 'CART') {

        # collapse categories with N < 200
        if(PCM) {
            res <- adjustments_PCM(resp, SC, wave, domain)
            resp <- res$resp
        }

        if (rotation) {
            S <- position[, 1] + 1
        } else {
            S <- NULL
        }

        if (!is.null(bgdata)) bgdata <- data.frame(bgdata)
        resp <- data.frame(resp)
        datalist <- plausible_values_mglrm(Y = resp, X = bgdata, S = S, npv = npv,
                        itermcmc = control$CART$itermcmc, burnin = control$CART$burnin,
                        thin = control$CART$thin, tdf = control$CART$tdf,
                        cartctrl1 = control$CART$cartctrl1,
                        cartctrl2 = control$CART$cartctrl2)
        if (is.null(bgdata)) {
            for (i in seq(length(datalist)))
                datalist[[i]] <- data.frame(ID_t = ID_t, PV = datalist[[i]]$PV)
        }
        rm(bgdata,S,position,resp)
    }



    # estimation with complete background data, without coding
    if (method == 'COM') {

        if (!is.null(bgdata)) {
            ID_t <- bgdata[, 'ID_t', drop = FALSE]
            if (changedFrom == 'MMI') {
                if (length(categorical) > 0) {
                    bgdata.d <- dummy_coding(bgdata, categorical, ID_t)
                    bgdata <- cbind(bgdata[, -which(names(bgdata) %in% categorical)], bgdata.d)
                }
            }
            bgdata <- bgdata[, -which(names(bgdata) == 'ID_t')]
            bgdata <- apply(bgdata, 2, as.numeric)
        }

        if(PCM) {
            res <- adjustments_PCM(resp, SC, wave, domain)
            resp <- res$resp
            ind <- res$ind

            if (rotation) {
                mod <- PCM_with_rotation(xsi, resp, bgdata, position, ind)
            } else {
                Q <- matrix(1, nrow = ncol(resp), ncol = 1)
                Q[ind, ] <- 0.5 * Q[ind, ]
                mod <- TAM::tam.mml(resp = resp, Y = bgdata,
                                    irtmodel = 'PCM2', xsi.fixed = xsi,
                                    Q = Q, verbose = FALSE)
            }
        } else {
            if (rotation) {
                mod <- TAM::tam.mml.mfr(resp = resp, irtmodel = '1PL',
                                        formulaA = ~ 0 + item + position,
                                        Y = bgdata, xsi.fixed = xsi,
                                        facets = position, verbose = FALSE)
            } else {
                mod <- TAM::tam.mml(resp = resp, Y = bgdata,
                                    irtmodel = '1PL', xsi.fixed = xsi,
                                    verbose = FALSE)
            }
        }
        rm(resp, xsi, position)

        if(control$WLE) {
            wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
            wle <- matrix(wmod$theta, nrow = length(wmod$theta), ncol = 1)
            wle <- cbind(ID_t, wle, wmod$error)
            colnames(wle) <- c('ID_t', 'wle', 'se')
        }
        pmod <- TAM::tam.pv(mod, nplausible = npv, ntheta = control$TAM$ntheta,
                            normal.approx = control$TAM$normal.approx,
                            samp.regr = control$TAM$samp.regr,
                            theta.model = control$TAM$theta.model,
                            np.adj = control$TAM$np.adj,
                            na.grid = control$TAM$na.grid)
        if (!is.null(bgdata)) {
            datalist <- TAM::tampv2datalist(pmod, Y = cbind(ID_t, bgdata),
                                            pvnames = 'PV')
        } else {
            datalist <- TAM::tampv2datalist(pmod, Y = ID_t, pvnames = 'PV')
        }
        if (control$EAP) {
            eap <- cbind(ID_t, mod$person[, c('EAP', 'SD.EAP')])
            colnames(eap) <- c('ID_t', 'eap', 'se')
        }
        for (d in seq(length(datalist))) {
            datalist[[d]] <- datalist[[d]][, -which(colnames(datalist[[d]]) %in% c('pid', 'pweights'))]
        }
        EAP.rel <- mod$EAP.rel
    }

    # output
    res <- list()
    res[['SC']] <- SC
    res[['domain']] <- domain
    res[['wave']] <- wave
    res[['method']] <- method
    if (changedFrom != "NULL")
        res[["changedFrom"]] <- changedFrom
    res[['type']] <- type
    res[['rotation']] <- ifelse(rotation, 'Corrected For Test Position',
                                'No Correction For Test Position')
    res[['nvalid']] <- nvalid
    res[['model']] <- ifelse(PCM, 'Partical Credit Model', 'Rasch Model')
    res[['n.valid']] <- n.valid
    res[['npv']] <- npv
    res[['control']] <- control

    if (method == 'IND') {
        if (control$IND$pca.data) {
            bgdata.pca <- cbind(ID_t, bgdata.pca)
            res[['pca.data']] <- bgdata.pca
            res[['coded.data']] <- bgdata
        } else {
            if (control$EAP) {
                res[['eap']] <- eap
                res[['eap.uncorrected']] <- eaps[[SC]][[domain]][[wave]][[type]]
            }
            if (control$WLE) {
                res[['wle']] <- wle
            }
            res[['pv']] <- datalist
            res[['pca.data']] <- bgdata.pca
            res[['EAP.rel']] <- EAP.rel
        }
    } else if (method == 'CART') {
        res[['pv']] <- datalist
    } else if (method %in% c('MMI', 'COM')) {
        if (control$EAP) {
            res[['eap']] <- eap
            res[['eap.uncorrected']] <- eaps[[SC]][[domain]][[wave]][[type]]
        }
        if (control$WLE) {
            res[['wle']] <- wle
        }
        res[['pv']] <- datalist
        res[['EAP.rel']] <- EAP.rel
    }
    class(res) <- "pv.obj"
    return(res)
}
