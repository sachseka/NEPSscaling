#' Function for estimating plausible values with NEPS competence data and NEPS
#' scaled item parameters
#'
#' @param SC           numeric. The starting cohort used for the analysis is
#' indicated by an integer value (e.g., starting cohort one = 1).
#' @param domain       character. The competence domain of interest is indicated
#' by the domain abbreviation as used in the variable names (see Fuß, D., Gnambs,
#' T., Lockl, K., & Attig, M., 2016).
#' @param wave         numeric. The wave of competence testing is indicated by
#' an integer value (e.g., wave one = 1).
#' @param method       character. Should estimation method be fully Bayesian
#' ("Bayes") or maximum likelihood-bases ("ML")?
#' @param path         character; file path leading to the location of the
#' competence data
#' @param filetype     filetype of competence data (the NEPS RDC provides SPSS
#' and Stata data files)
#' @param bgdata       data frame containing background variables. Categorical
#' variables have to be specified as factors. If \code{bgdata = NULL}, plausible
#' values are estimated without a background model. Missing data in the covariates
#' is imputed using sequential classification and regression trees.
#' @param npv          numeric; number of plausible values to be estimated;
#' defaults to 10.
#' @param nvalid       numeric; minimum number of responses test takers had to
#' give for inclusion into the estimation process. NEPS default is at least three.
#' @param longitudinal logical. TRUE indicating that a multidimensional model with
#' the measurement points as its dimensions is to be estimated. Defaults to FALSE.
#' @param rotation     logical. TRUE indicating that the competence scores
#' should be corrected for the rotation design of the booklets. Defaults to
#' TRUE. If both longitudinal and rotation are TRUE, rotation is set to FALSE
#' automatically.
#' @param control      list of additional options. If \code{EAP = TRUE}, the EAPs
#' will be returned as well. Furthermore, additional control options for "ML"
#' (again in form of a list) are \code{nmi} (the number of multiple imputations
#' if there is missing data in the covariates) and arguments concerning the ML
#' estimation given to TAM::tam.pv() (therefore, see TAM for further control
#' options). Analogously, control options for "Bayes" (in form of a list) are
#' "itermcmc" (number of MCMC iterations), "burnin" (number of burn-in iterations),
#' "est.alpha" (whether item discriminations for polytomous items are to be fixed
#' or estimated freely), "thin" (thinning interval in MCMC), "tdf" (df of multiv.
#' t proposal distr. for category cut-offs for ordinal items), "cartctrl1" (min.
#' number of observations in any terminal CART node) and "cartctrl2" (min.
#' decrease of overall lack of fit by each CART split). NOTE that "Bayes" control
#' options are also applied to the CART algorithm for "ML" estimation.
#'
#' @return \code{plausible_values()} returns an object of class \code{pv.obj}
#' containing a list of \code{npv} plausible values with the data set used
#' for estimating them as well as the specified function arguments.
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
#' @references Scharl, A., Carstensen, C. H., & Gnambs, T. (2019).
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
#' result <- plausible_values(SC = 6
#'                            , domain = 'RE'
#'                            , wave = 3
#'                            , bgdata = bg_data
#'                            , path = path
#'                            , method = 'Bayes'
#'                            , filetype = 'Stata'
#'                            )
#' }
#'
#' @note For an extensive example, see the accompanying NEPS Survey Paper.
#' The function will only work with NEPS data. To access NEPS data see
#' https://www.neps-data.de/en-us/datacenter/dataaccess.aspx.
#'
#' @importFrom stats prcomp
#' @importFrom stats as.formula
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
    control = list(EAP = FALSE, #WLE = FALSE,
                   Bayes = list(itermcmc = 10000, burnin = 2000, est.alpha = FALSE, thin = 1, tdf = 10,
                               cartctrl1 = 5, cartctrl2 = 0.0001),
                   ML = list(nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                              theta.model = FALSE, np.adj = 8, na.grid = 5,
                             itermcmc = 100, burnin = 50, thin = 1, tdf = 10,
                             cartctrl1 = 5, cartctrl2 = 0.0001))
){
    rea9_sc1u <- wave_w3 <- wave_w5 <- . <- NULL
    # check and prepare arguments
    if (missing(SC)) stop("Starting cohort must be specified.")
    if (!is.numeric(SC) || !is.numeric(wave)) stop("Starting cohort and wave must be numeric.")
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
        if (SC == "SC4") {
            if (domain == "RE")
                waves <- c("_w2", "_w7", "_w10")
            if (domain == "MA")
                waves <- c("_w1", "_w7", "_w10")
            if (domain == "IC")
                waves <- c("_w1", "_w7")
            if (domain == "SC")
                waves <- c("_w1", "_w5")
            if (domain == "EF")
                waves <- c("_w3", "_w7")
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
        data <- tryCatch({haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]),
                                           user_na = TRUE)},
                         error=function(e){
                             stop(paste0("Path ", path, " does not lead to competence files OR wrong file formats!"))
                         })
    } else {
        data <- tryCatch({haven::read_dta(file = paste0(path, files[grep('xTargetCompetencies', files)]),
                                          user_na = TRUE)},
                         error=function(e){
                             stop(paste0("Path '", path, "' does not lead to competence files OR wrong file formats!"))
                         })
    }
    data <- data[order(data$ID_t), ]

    # selection of test takers
    if (longitudinal) {
        sel <- names(data) %in% unique(unlist(item_labels[[SC]][[domain]]))
        nr <- data.frame(ID_t = data$ID_t, nr = rowSums(data[, sel] == -94))
        data[data < 0] <- NA
        data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
        nr <- nr[nr$ID_t %in% data$ID_t, ]
    } else {
        sel <- names(data) %in% item_labels[[SC]][[domain]][[wave]]
        nr <- data.frame(ID_t = data$ID_t, nr = rowSums(data[, sel] == -94))
        data[data < 0] <- NA
        data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
        nr <- nr[nr$ID_t %in% data$ID_t, ]
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
            if (nrow(bgdata) < nrow(data)) {
                bgdata <- suppressWarnings(
                    dplyr::bind_rows(bgdata,
                                     data[!(data$ID_t %in% bgdata$ID_t),
                                          'ID_t', drop = FALSE]))
            }
            # data <- data[data$ID_t %in% bgdata$ID_t, ]
            bgdata <- bgdata[order(bgdata$ID_t), ]
        } else {
            # append subjects in background data that did not take the competence tests
            data <- suppressWarnings(
                dplyr::bind_rows(data, bgdata[!(bgdata$ID_t %in% data$ID_t),
                                              'ID_t', drop = FALSE]))
            data <- data[order(data$ID_t), ]
            bgdata <- suppressWarnings(
                dplyr::bind_rows(bgdata,
                                 data[!(data$ID_t %in% bgdata$ID_t),
                                      'ID_t', drop = FALSE]))
            bgdata <- bgdata[order(bgdata$ID_t), ]
        }
        bgdata <- dplyr::left_join(bgdata, nr, by = "ID_t")
        ID_t <- bgdata[, "ID_t", drop = FALSE]

        # list of categorical variables
        fac <- unlist(lapply(bgdata, is.factor))
        categorical <- names(bgdata)[fac]
    }
    n.valid <- data.frame(ID_t = data[, 'ID_t'])
    if (!longitudinal) {
        n.valid$valid <- rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]]))
    } else {
        for(w in waves) {
            n.valid[[paste0("valid", w)]] <- rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[gsub("_", "", w)]]]))
        }
    }

    if (longitudinal) {
        rotation <- FALSE
        if (method == "ML")
            Q <- qmat(SC, domain)
    } else {
        Q <- NULL
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
                # if (wave == 'w1') {
                #     position[, 'position'] <- data[, 'tx80211_w1']
                #     position[!is.na(position$position) & (position$position == 128), 'position'] <- 1 #  ICT first
                #     position[!is.na(position$position) & (position$position == 129), 'position'] <- 2 #  science first
                #     position$position <- haven::labelled(position$position, c("ICT first" = 1, "science first" = 2))
                # } else if (wave == 'w7') {
                #     position[, 'position'] <- data[, 'tx80211_w7']
                #     if (domain == "RE") {
                #         position[!is.na(position$position) & (position$position %in% c(283:288,300:303)), 'position'] <- 1 # reading first
                #         position[!is.na(position$position) & (position$position %in% c(281,282,289:292)), 'position'] <- 2 # reading last
                #         position$position <- haven::labelled(position$position, c("reading first" = 1, "reading last" = 2))
                #     } else if (domain == "MA") {
                #         position[!is.na(position$position) & (position$position %in% c(289:292)), 'position'] <- 1 # math first
                #         position[!is.na(position$position) & (position$position %in% c(284,285,287,288)), 'position'] <- 2 # math last
                #         position$position <- haven::labelled(position$position, c("math first" = 1, "math last" = 2))
                #     } else if (domain == "ICT") {
                #         position[!is.na(position$position) & (position$position %in% c(281,282,296:299)), 'position'] <- 1 # ICT first
                #         position[!is.na(position$position) & (position$position %in% c(283,286)), 'position'] <- 2 # ICT last
                #         position$position <- haven::labelled(position$position, c("ICT first" = 1, "ICT last" = 2))
                #     }
                # } else if (wave == 'w9') {
                #     position[, 'position'] <- data[, 'tx80211_w9']
                #     position[!is.na(position$position) & (position$position %in% c(470,471,474,475)), 'position'] <- 1 # reading first
                #     position[!is.na(position$position) & (position$position %in% c(472,473,476)), 'position'] <- 2 # math first
                #     position$position <- haven::labelled(position$position, c("reading first" = 1, "math first" = 2))
                # }
            } else if (SC == 'SC5') {
                if (wave == 'w1') {
                    position[, 'position'] <- data[, 'tx80211_w1']
                    position[!is.na(position$position) & (position$position == 126), 'position'] <- 1 # reading first
                    position[!is.na(position$position) & (position$position == 127), 'position'] <- 2 # maths first
                    position$position <- haven::labelled(position$position, c("reading first" = 1, "math first" = 2))
                } else if (wave == 'w5') {
                    position[, 'position'] <- data[, 'tx80211_w5']
                    position[!is.na(position$position) & (position$position %in% c(330,332,334)), 'position'] <- 1 # sc first
                    position[!is.na(position$position) & (position$position %in% c(331,333,335)), 'position'] <- 2 # ict first
                    position$position <- haven::labelled(position$position, c("science first" = 1, "ICT first" = 2))
                }
            } else if (SC == 'SC6') {
                if (wave == 'w3') {
                    position[, 'position'] <- data[, 'tx80211_w3']
                    position[!is.na(position$position) & (position$position %in% c(122,124)), 'position'] <- 1 # maths first
                    position[!is.na(position$position) & (position$position %in% c(123,125)), 'position'] <- 2 # reading first
                    position$position <- haven::labelled(position$position, c("math first" = 1, "reading first" = 2))
                } else if (wave == 'w5') {
                    position[, 'position'] <- data[, 'tx80211_w5']
                    position[!is.na(position$position) & position$position == 247, 'position'] <- 1 # science first
                    position[!is.na(position$position) & position$position == 248, 'position'] <- 2 # ict first
                    # reading first (i.e. "new" SC6 sample) should be marked with 249 (not found in SUF)
                    if (domain == "RE"){
                        position[is.na(position$position), 'position'] <- 3} # reading first; should not occur for IC/SC tests
                    position$position <- haven::labelled(position$position, c("science first" = 1, "ict first" = 2))
                } else if (wave == "w9") {
                    position[, 'position'] <- data[, 'tx80211_w9']
                    position[!is.na(position$position) & position$position %in% c(444,445,448,449,452:455), 'position'] <- 1 # reading first
                    position[!is.na(position$position) & position$position %in% c(446,447,450,451,456,457), 'position'] <- 2 # math first
                    position$position <- haven::labelled(position$position, c("reading first" = 1, "math first" = 2))
                }
            }
            # possible NAs in position variable treated as third group
            position[is.na(position$position), "position"] <- 3
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
    res <- complement_control_lists(control$EAP, #control$WLE,
                                    control$Bayes, control$ML)
    control$EAP <- res$EAP
    #control$WLE <- res$WLE
    control$Bayes <- res$Bayes
    control$ML <- res$ML



    # estimation with ML
    if (method == "ML") {

        # multiple imputation of missing covariate data
        if (!is.null(bgdata)) {
            if (any(is.na(bgdata))) {
                imp <- CART(X = bgdata, itermcmc = control$ML$itermcmc,
                        burnin = control$ML$burnin, nmi = control$ML$nmi,
                        thin = control$ML$thin, cartctrl1 = control$ML$cartctrl1,
                        cartctrl2 = control$ML$cartctrl2)
            } else {
                bgdata <- as.data.frame(lapply(bgdata, as.numeric))
                imp <- NULL
            }
        } else {imp <- NULL}

        if(PCM) {
            res <- adjustments_PCM(resp, SC,
                                   if (longitudinal) gsub("_", "", waves) else wave,
                                   domain)
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
                                           constraint = "cases")
            A <- res$A$A.3d
            B <- res$B$B.3d
            resp <- res$gresp$gresp.noStep
            }
        }

        # fixed item parameters
        if (domain == "MA") {
            if (longitudinal & SC %in% c("SC4", "SC5", "SC6")) {
                fixed_difficulty <- xsi.fixed[["long"]][["MA"]][[SC]]
            } else if ((SC == "SC4" & wave == "w10") |
                       (SC == "SC5" & wave == "w12") |
                       (SC == "SC6" & wave == "w9")) {
                fixed_difficulty <- xsi.fixed[["cross"]][["MA"]]
            }
        } else if (domain == "RE") {
            if (longitudinal & SC %in% c("SC4", "SC5", "SC6")) {
                fixed_difficulty <- xsi.fixed[["long"]][["RE"]][[SC]]
            } else if ((SC == "SC4" & wave == "w10") |
                       (SC == "SC5" & wave == "w12") |
                       (SC == "SC6" & wave == "w9")) {
                fixed_difficulty <- xsi.fixed[["cross"]][["RE"]][[SC]]
            }
        } else {
            fixed_difficulty <- NULL
        }

        pvs <- list()
        EAP.rel <- list()
        regr.coeff <- list()
        variance <- list()
        eap <- replicate(ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi),
                         matrix(c(ID_t$ID_t, rep(0, 2*length(waves)*nrow(resp))),
                                ncol = (1 + 2*length(waves)),
                                nrow = nrow(resp)),
                         simplify = FALSE)
        for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)) {
            if (!is.null(imp)) {
                bgdatacom <- imp[[i]]
                bgdatacom$ID_t <- NULL
                bgdatacom <- as.data.frame(apply(bgdatacom, 2, as.numeric))
            }
            # estimate IRT model
            mod <- TAM::tam.mml(resp = resp,
                                dataY = if (is.null(bgdata)) NULL else if (is.null(imp)) bgdata[, -which(names(bgdata) == "ID_t")] else bgdatacom,
                                formulaY = as.formula(paste("~", paste(colnames(bgdatacom), collapse = "+"))),
                                irtmodel = ifelse(PCM, "PCM2", "1PL"),
                                xsi.fixed = fixed_difficulty,
                                A = if (rotation) A else NULL, B = if (PCM || rotation) B else NULL,
                                Q = Q, verbose = FALSE)
            # impute plausible values
            pmod <- TAM::tam.pv(mod, nplausible = npv,
                                ntheta = control$ML$ntheta,
                                normal.approx = control$ML$normal.approx,
                                samp.regr = control$ML$samp.regr,
                                theta.model = control$ML$theta.model,
                                np.adj = control$ML$np.adj,
                                na.grid = control$ML$na.grid)
            eap[[i]][, -1] <- as.matrix(mod$person[, grep("EAP",
                                                          names(mod$person))])
            colnames(eap[[i]]) <- c('ID_t',  paste0(rep(c('eap','se'),
                                                        length(waves)),
                                                    rep(waves, each = 2)))
            pvs[[i]] <- TAM::tampv2datalist(pmod,
                                            Y = if (is.null(bgdata) || is.null(imp)) ID_t else cbind(ID_t, bgdatacom),
                                            pvnames = paste0("PV", waves))
            EAP.rel[[i]] <- mod$EAP.rel
            regr.coeff[[i]] <- mod$beta
            variance[[i]] <- mod$variance
        }
        # if (control$WLE && !longitudinal) {
        #     wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
        #     wle <- matrix(wmod$theta, nrow = length(wmod$theta), ncol = 1)
        #     wle <- cbind(ID_t, wle, wmod$error)
        #     colnames(wle) <- c('ID_t', 'wle', 'se')
        # } else if (control$WLE && longitudinal) {
        #     wmod <- TAM::tam.mml.wle2(mod, WLE = TRUE, progress = FALSE)
        #     wle <- do.call(cbind, list(ID_t, wmod[grep("theta|error", colnames(wmod))]))
        #     colnames(wle) <- c('ID_t',  paste0(rep(c('wle','se'), length(waves)), rep(waves, each = 2)))
        # }
        datalist <- list()
        d <- 1
        for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)) {
            for (j in 1:npv) {
                datalist[[d]] <- pvs[[i]][[j]]
                d <- d + 1
            }
        }
        pvs <- NULL
        ind <- sample(1:length(datalist), npv)
        datalist <- datalist[ind]
        for (i in 1:npv) {
            datalist[[i]] <- datalist[[i]][, -which(colnames(datalist[[i]]) %in%
                                                        c('pid', 'pweights',
                                                          'test_position'))]
            datalist[[i]] <- datalist[[i]] %>% dplyr::select(ID_t,
                                                             dplyr::everything())
        }
    }



    # estimation using MCMC
    if (method == "Bayes") {

        # collapse categories with N < 200
        if(PCM) {
            res <- adjustments_PCM(resp, SC,
                                   if (longitudinal) gsub("_", "", waves) else wave,
                                   domain)
            resp <- res$resp
        }

        if (rotation) {
            S <- position[, 1]# + 1
        } else {
            S <- NULL
        }

        if (!is.null(bgdata)) bgdata <- data.frame(bgdata)
        resp <- data.frame(resp)
        if (longitudinal){
            datalist <- plausible_values_mdlrm(Y = resp, X = bgdata, npv = npv,
                                               itermcmc = control$Bayes$itermcmc,
                                               burnin = control$Bayes$burnin,
                                               thin = control$Bayes$thin,
                                               tdf = control$Bayes$tdf,
                                               est.alpha = control$Bayes$est.alpha,
                                               cartctrl1 = control$Bayes$cartctrl1,
                                               cartctrl2 = control$Bayes$cartctrl2,
                                               SC = SC, domain = domain,
                                               waves = waves)
        } else {
            datalist <- plausible_values_mglrm(Y = resp, X = bgdata, S = S,
                                               npv = npv,
                                               itermcmc = control$Bayes$itermcmc,
                                               burnin = control$Bayes$burnin,
                                               thin = control$Bayes$thin,
                                               tdf = control$Bayes$tdf,
                                               est.alpha = control$Bayes$est.alpha,
                                               cartctrl1 = control$Bayes$cartctrl1,
                                               cartctrl2 = control$Bayes$cartctrl2,
                                               SC = SC, domain = domain, wave = wave)
        }
        if (is.null(bgdata)) {
            for (i in seq(length(datalist$datalist))) {
                datalist$datalist[[i]]$ID_t <- ID_t$ID_t
                datalist$datalist[[i]] <- datalist$datalist[[i]] %>%
                    dplyr::select(ID_t, dplyr::everything())
            }
        }
    }


    # linear transformation of longitudinal PVs to pre-defined scale
    if (longitudinal) {
        if (method == "Bayes") {
            VAR <- datalist$VAR
            MEAN <- colMeans(datalist$EAP[, grep("EAP", names(datalist$EAP))])
        } else {
            VAR <- colMeans(do.call(rbind, lapply(variance, FUN = function (x) diag(x))))
            MEAN <- colMeans(do.call(rbind, lapply(eap, FUN = function (x) {
                colMeans(x[, seq(2, (1+2*length(waves)), 2)])})))
        }
        names(VAR) <- gsub("_", "", waves)
        names(MEAN) <- gsub("_", "", waves)
        # TODO: for SC6 reading the PVs have to be linked to w3 and w5 separately
        if (SC == "SC6" && domain == "RE") {
            longitudinal_IDs <- list()
            longitudinal_IDs[["w3"]] <- dplyr::filter(data, wave_w3 == 1,
                                                      !is.na(rea9_sc1u))$ID_t
            longitudinal_IDs[["w5"]] <- dplyr::filter(data, wave_w3 == 0,
                                                      wave_w5 == 1,
                                                      !is.na(rea9_sc1u))$ID_t
        }
        pv <- scale_pv(pv = if (method == "Bayes") datalist$datalist else datalist,
                       SC = SC, domain = domain, type = type,
                       wave = gsub("_", "", waves), VAR = VAR, MEAN = MEAN,
                       ID = if(SC == "SC6") longitudinal_IDs else NULL)
        for(p in seq(npv)) {
            for(w in waves) {
                pv[[p]][n.valid[[paste0("valid", w)]] < nvalid, paste0("PV", w)] <- NA
            }
            if (SC == "SC6" && domain == "RE") {
                pv[[p]][["PV_w5"]] <- NA
                pv[[p]][pv[[p]]$ID_t %in% longitudinal_IDs[["w5"]], "PV_w5"] <-
                    pv[[p]][pv[[p]]$ID_t %in% longitudinal_IDs[["w5"]], "PV_w3"]
                pv[[p]][pv[[p]]$ID_t %in% longitudinal_IDs[["w5"]], "PV_w3"] <- NA
            }
        }
    } else {
        pv <- if (method == "ML") datalist else datalist$datalist
        for(p in seq(npv)) {
            pv[[p]][n.valid$valid < nvalid, "PV"] <- NA
        }
        if (method == "Bayes") {
            VAR <- sum(datalist$VAR)
            MEAN <- mean(datalist$EAP$EAP)
        } else {
            VAR <- mean(unlist(variance))
            MEAN <- mean(sapply(eap, FUN = function(x) mean(x[, 2])))
        }
    }

    # output
    res <- list()
    res[['SC']] <- as.numeric(gsub(pattern = "SC", replacement = "", x = SC))
    res[['domain']] <- domain
    res[['wave']] <- as.numeric(gsub(pattern = "w", replacement = "", x = wave))
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
        res[["abs.correction"]] <- sapply(meanvar[[SC]][[domain]],
                                          FUN = function(x) x[[1]]) #+
            # unlist(sapply(gsub("_", "", waves),
            #               FUN = function(x) correction[[SC]][[domain]][[x]]))
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
        # if (control$WLE)
        #     res[['wle']] <- wle
        res[['EAP.rel']] <- EAP.rel
        res[["regr.coeff"]] <- regr.coeff
        if (!is.null(bgdata)) {
            for (n in 1:ifelse(!any(is.na(bgdata)), 1, control$ML$nmi)) {
                if (!longitudinal){
                    rownames(res[["regr.coeff"]][[n]]) <-
                        c("Intercept",
                          names(res[['pv']][[1]][, 2:(ncol(res[['pv']][[1]])-1)]))
                } else {
                    rownames(res[["regr.coeff"]][[n]]) <-
                        c("Intercept", names(res[['pv']][[1]][, 2:(ncol(res[['pv']][[1]]) -
                           ifelse(SC == "SC6", length(waves) + 1, length(waves)))]))
                }
            }
        }

    }
    class(res) <- "pv.obj"
    return(res)
}
