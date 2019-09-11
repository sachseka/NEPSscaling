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
#' @param include.nr logical; whether the number of not-reached items as a proxy
#' for processing speed should be included in the background model (the default
#' is TRUE)
#' @param control      list of additional options. If \code{EAP = TRUE}, the EAPs
#' will be returned as well. Furthermore, additional control options for "ML"
#' (again in form of a list) are \code{nmi} (the number of multiple imputations
#' if there is missing data in the covariates) and arguments concerning the ML
#' estimation given to TAM::tam.pv() (therefore, see TAM for further control
#' options). Analogously, control options for "Bayes" (in form of a list) are
#' "itermcmc" (number of MCMC iterations), "burnin" (number of burn-in iterations),
#' "est.alpha" (whether item discriminations of the graded response model are to
#' be fixed or estimated freely; default: fixed), "thin" (thinning interval in
#' MCMC), "tdf" (df of multiv. t proposal distr. for category cut-offs for
#' ordinal items), "cartctrl1" (min. number of observations in any terminal CART
#' node) and "cartctrl2" (min. decrease of overall lack of fit by each CART
#' split). NOTE that "Bayes" control options are also applied to the CART
#' algorithm for "ML" estimation.
#'
#' @return \code{plausible_values()} returns an object of class \code{pv.obj}
#' containing:
#' \describe{
#' \item{SC}{Starting cohort that plausible values were estimated for}
#' \item{domain}{Competence domain that plausible values were estimated for}
#' \item{wave}{Wave that plausible values were estimated for}
#' \item{method}{Estimation algorithm that was used for estimating plausible
#' values (\code{ML} for the EM algorithm implemented in \code{TAM} or \code{Bayes}
#' for the Gibbs Sampler implemented in \code{LaRA})}
#' \item{type}{Whether cross-sectional ("cross") or longitudinal ("long")
#' plausible values were estimated}
#' \item{rotation}{In most assessments the position of the competence test was
#' rotated with another competence domain. If this was the case for the
#' specific estimation, this variable indicates whether the correction was
#' applied. Depending on the estimation context, this variable may have been
#' automatically set by the function and thus differ from user input}
#' \item{nvalid}{The minimum number of answers a test taker must have given}
#' \item{model}{If the method \code{ML} was chosen, a Rasch or a Partial Credit
#' Model was estimated, depending on the item format. If \code{Bayes} was chosen,
#' a Graded Response Model was estimated.}
#' \item{n.valid}{A \code{data.frame} containing the \code{ID_t} and the
#' number of valid responses given by the respective individual}
#' \item{npv}{The number of plausible values that are returned by the function}
#' \item{control}{The control variables that were applied to fine-tune the
#' estimation algorithms}
#' \item{position}{A \code{data.frame} containing the \code{ID_t} and the
#' position the respective individual got the testlet in (first or second)}
#' \item{abs.correction}{The total amount by which the scale was shifted in
#' longitudinal estimation (consisting of linking constants and dropout
#' corrections)}
#' \item{variance.PV}{The overall variance of all persons' abilities}
#' \item{mean.PV}{The overall mean of all persons' abilities}
#' \item{pv}{A list of \code{data.frame}s containing one plausible value each
#' and the imputed data set that was used to estimate the plausible value.
#' Additionally, if \code{include.nr} was specified, the background model is
#' enriched by the number of not reached items (\code{nr}) per test taker as a
#' proxy for response times.}
#' \item{eap}{A \code{data.frame} containing the \code{ID_t} and the ability
#' EAP value for the respective individual}
#' \item{regr.coeff}{The regression coefficients of the latent regression of
#' the ability}
#' \item{alpha}{If the method \code{Bayes} and, in the controls, \code{est.alpha}
#' were chosen, the item discrimination parameters (as the EAPs) are returned}
#' \item{EAP.rel}{For method \code{ML} the EAP reliability is returned}
#' }
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
#' library(NEPScaling)
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
                             path,
                             filetype = c('SPSS','Stata'),
                             bgdata = NULL,
                             npv = 10L,
                             longitudinal = FALSE,
                             rotation = TRUE,
                             nvalid = 3L,
                             include.nr = TRUE,
                             control = list(EAP = FALSE, WLE = FALSE,
                                            Bayes = list(itermcmc = 10000, burnin = 2000, est.alpha = FALSE, thin = 1, tdf = 10,
                                                         cartctrl1 = 5, cartctrl2 = 0.0001),
                                            ML = list(nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
                                                      theta.model = FALSE, np.adj = 8, na.grid = 5,
                                                      itermcmc = 100, burnin = 50, thin = 1,
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
    filetype <- match.arg(filetype)
    if (missing(path)) stop("Path must be specified.")
    if (!is.character(path) || !grepl("/$",path)) stop("Path must be a character string and end in '/'.")
    if(nvalid < 0) stop("nvalid must be non-negative.")
    if(is.null(item_labels[[SC]][[domain]][[wave]]))
        stop(paste("No competence data availabe for", SC, domain, wave,"."))
    if (longitudinal && ((SC == "SC6" | SC == "SC5") & domain %in% c("IC", "SC", "BA", "EF"))){
        longitudinal <- FALSE
        message(paste0("No longitudinal data for: ", SC, " ", domain,". Estimating cross-sectional plausible values instead."))
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

    # get competence data for SC and domain
    files <- list.files(path = path)
    if (filetype == 'SPSS') {
        if (SC == "SC5" & domain == "BA") {
            data <- tryCatch({haven::read_spss(file = paste0(path, files[grep('xEcoCAPI', files)]),
                                               user_na = TRUE)},
                             error=function(e){
                                 stop(paste0("Path ", path, " does not lead to competence files OR wrong file formats!"))
                             })
        } else {
            data <- tryCatch({haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]),
                                               user_na = TRUE)},
                             error=function(e){
                                 stop(paste0("Path ", path, " does not lead to competence files OR wrong file formats!"))
                             })
        }
    } else {
        if (SC == "SC5" & domain == "BA") {
            data <- tryCatch({haven::read_dta(file = paste0(path, files[grep('xEcoCAPI', files)]),
                                              user_na = TRUE)},
                             error=function(e){
                                 stop(paste0("Path '", path, "' does not lead to competence files OR wrong file formats!"))
                             })
        } else {
            data <- tryCatch({haven::read_dta(file = paste0(path, files[grep('xTargetCompetencies', files)]),
                                              user_na = TRUE)},
                             error=function(e){
                                 stop(paste0("Path '", path, "' does not lead to competence files OR wrong file formats!"))
                             })
        }
    }
    data <- data[order(data$ID_t), ]

    # selection of test takers
    if (longitudinal) {
        sel <- names(data) %in% unique(unlist(item_labels[[SC]][[domain]]))
        if (include.nr) {
            nr <- data.frame(ID_t = data$ID_t, nr = rowSums(data[, sel] == -94))
            data[data < 0] <- NA
            data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
            nr <- nr[nr$ID_t %in% data$ID_t, ]
        } else {
            data[data < 0] <- NA
            data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
        }
    } else {
        sel <- names(data) %in% item_labels[[SC]][[domain]][[wave]]
        if (include.nr) {
            nr <- data.frame(ID_t = data$ID_t, nr = rowSums(data[, sel] == -94))
            data[data < 0] <- NA
            data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
            nr <- nr[nr$ID_t %in% data$ID_t, ]
        } else {
            data[data < 0] <- NA
            data <- data[rowSums(!is.na(data[, sel])) >= nvalid, ]
        }
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
        if (include.nr)
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
            } else if (SC == 'SC5') { # w7 does not have rotation an can be ignored here
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
                } else if (wave == 'w12') { # rotation for all of them --> like SC4
                    if (domain == "RE") {
                        position[, 'position'] <- data[, 'tx80211_w12']
                        position[!is.na(position$position) & (position$position %in% c(459,462,465,468)), 'position'] <- 1 # reading first
                        position[!is.na(position$position) & (position$position %in% c(458,463,464,469)), 'position'] <- 2 # reading second
                        position$position <- haven::labelled(position$position, c("reading first" = 1, "reading second" = 2))
                    } else if (domain == "MA") {
                        position[, 'position'] <- data[, 'tx80211_w12']
                        position[!is.na(position$position) & (position$position %in% c(458,460,464,466)), 'position'] <- 1 # math first
                        position[!is.na(position$position) & (position$position %in% c(459,461,465,467)), 'position'] <- 2 # math second
                        position$position <- haven::labelled(position$position, c("math first" = 1, "math second" = 2))
                    } else if (domain == "EF") {
                        position[, 'position'] <- data[, 'tx80211_w12']
                        position[!is.na(position$position) & (position$position %in% c(461,463,467,469)), 'position'] <- 1 # English first
                        position[!is.na(position$position) & (position$position %in% c(460,462,466,468)), 'position'] <- 2 # English second
                        position$position <- haven::labelled(position$position, c("English first" = 1, "English second" = 2))
                    }
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
    } else {
        resp <- data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]]
    }

    # check for Partial Credit Items
    PCM <- max(apply(resp, 2, max, na.rm = TRUE)) > 1

    # complement control lists
    res <- complement_control_lists(control$EAP, #control$WLE,
                                    control$Bayes, control$ML)
    control$EAP <- res$EAP
    control$Bayes <- res$Bayes
    control$ML <- res$ML


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
            frmY <- as.formula(paste("~", paste(colnames(bgdata[, -which(colnames(bgdata) == "ID_t")]), collapse = "+")))
        }
    } else {imp <- frmY <- NULL}

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
            if (longitudinal) {
                for (i in seq(length(Q))) {
                    Q[[i]][ind[[i]], ] <- 0.5
                }
                B <- A <- NULL
            } else {
                B <- TAM::designMatrices(modeltype = 'PCM',
                                         Q = Q,
                                         resp = resp)$B
                B[ind, , ] <- 0.5 * B[ind, , ]
                A <- NULL
            }
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

    pvs <- list()
    EAP.rel <- list(NULL)
    regr.coeff <- list(NULL)
    variance <- list(NULL)
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
            frmY <- as.formula(paste("~", paste(colnames(bgdatacom), collapse = "+")))
        }
        # estimate IRT model
        mod <- list()
        tmp_pvs <- list()
        pmod <- list()
        for (j in seq(length(waves))) {
            mod[[j]] <- TAM::tam.mml(resp = if (longitudinal) {resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)[j]]] ]} else {resp},
                                     dataY = if (is.null(bgdata)) {NULL} else if (is.null(imp)) {bgdata[, -which(names(bgdata) == "ID_t")]} else {bgdatacom},
                                     formulaY = frmY,
                                     irtmodel = ifelse(PCM, "PCM2", "1PL"),
                                     xsi.fixed = if (is.null(xsi.fixed[[type]][[domain]][[SC]][[if (longitudinal) {gsub("_", "", waves)[j]} else {wave}]])) {NULL} else
                                         {cbind(1:nrow(xsi.fixed[[type]][[domain]][[SC]][[if (longitudinal) {gsub("_", "", waves)[j]} else {wave}]]),
                                                xsi.fixed[[type]][[domain]][[SC]][[if (longitudinal) {gsub("_", "", waves)[j]} else {wave}]][,2])},
                                     Q = Q[[j]], B = B, A = A,
                                     verbose = FALSE)
            # impute plausible values
            pmod[[j]] <- TAM::tam.pv(mod[[j]], nplausible = npv,
                                     ntheta = control$ML$ntheta,
                                     normal.approx = control$ML$normal.approx,
                                     samp.regr = control$ML$samp.regr,
                                     theta.model = control$ML$theta.model,
                                     np.adj = control$ML$np.adj,
                                     na.grid = control$ML$na.grid)
            tmp_pvs[[j]] <- TAM::tampv2datalist(pmod[[j]],
                                                Y = if (is.null(bgdata) || is.null(imp)) {ID_t} else {cbind(ID_t, bgdatacom)},
                                                pvnames = paste0("PV", waves[j]))
            eap[[i]][, (j*2):(j*2+1)] <-
                as.matrix(mod[[j]]$person[, grep("EAP", names(mod[[j]]$person))])
            EAP.rel[[i]] <- c(EAP.rel[[i]], mod[[j]]$EAP.rel)
            regr.coeff[[i]] <- cbind(regr.coeff[[i]], mod[[j]]$beta)
            variance[[i]] <- c(variance[[i]], mod[[j]]$variance)
        }
        if (longitudinal) {
            for (j in seq(length(tmp_pvs)-1)) {
                for (n in 1:npv) {
                    pvs[[n]] <- suppressMessages(dplyr::full_join(tmp_pvs[[j]][[n]], tmp_pvs[[j+1]][[n]]))
                }
            }
        } else {
            pvs <- tmp_pvs[[1]]
        }
        rm(tmp_pvs)
        colnames(eap[[i]]) <- c('ID_t',  paste0(rep(c('eap','se'),
                                                    length(waves)),
                                                rep(waves, each = 2)))
    }
    if (control$WLE) {
        wmod <- list()
        for (j in seq(length(waves))) {
            wmod[[j]] <- TAM::tam.mml.wle2(mod[[j]], WLE = TRUE, progress = FALSE)
        }
        wmod <- wmod %>% Reduce(function(df1,df2) dplyr::full_join(df1,df2,by="pid"), .)
        wle <- do.call(cbind, list(ID_t, wmod[grep("theta|error", colnames(wmod))]))
        colnames(wle) <- c('ID_t',  paste0(rep(c('wle','se'), length(waves)), rep(waves, each = 2)))
    }
    datalist <- list()
    d <- 1
    for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)) {
        for (j in 1:npv) {
            datalist[[d]] <- pvs[[i]][, c("ID_t", paste0("PV", waves))]
            d <- d + 1
        }
    }
    pvs <- NULL
    ind <- sample(1:length(datalist), npv)
    datalist <- datalist[ind]


    # linear transformation of longitudinal PVs to pre-defined scale
    if (longitudinal) {
        VAR <- colMeans(do.call(rbind, lapply(variance, FUN = function (x) diag(x))))
        MEAN <- colMeans(do.call(rbind, lapply(eap, FUN = function (x) {
            colMeans(x[, seq(2, (1+2*length(waves)), 2)])})))
        names(VAR) <- gsub("_", "", waves)
        names(MEAN) <- gsub("_", "", waves)
        if (SC == "SC6" && domain == "RE") {
            longitudinal_IDs <- list()
            longitudinal_IDs[["w3"]] <- dplyr::filter(data, wave_w3 == 1,
                                                      !is.na(rea9_sc1u))$ID_t
            longitudinal_IDs[["w5"]] <- dplyr::filter(data, wave_w3 == 0,
                                                      wave_w5 == 1,
                                                      !is.na(rea9_sc1u))$ID_t
        }
        pv <- scale_pv(pv = datalist,
                       SC = SC, domain = domain, type = type,
                       wave = gsub("_", "", waves), VAR = VAR, MEAN = MEAN,
                       ID = if(SC == "SC6") longitudinal_IDs else NULL)
        if (control$WLE) {
            m <- colMeans(wle[paste0("wle", waves)], na.rm = TRUE)
            names(m) <- gsub("_", "", waves)
            wle <- scale_wle(wle = wle, SC = SC, domain = domain, type = NULL,
                             wave = gsub("_", "", waves), VAR = NULL,
                             MEAN = m,
                             ID = if(SC == "SC6") longitudinal_IDs else NULL)
        }
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
        if (control$WLE && SC == "SC6" && domain == "RE") {
            wle[["wle_w5"]] <- NA
            wle[["se_w5"]] <- NA
            wle[wle$ID_t %in% longitudinal_IDs[["w5"]], c("wle_w5", "se_w5")] <-
                wle[wle$ID_t %in% longitudinal_IDs[["w5"]], c("wle_w3", "se_w3")]
            wle[wle$ID_t %in% longitudinal_IDs[["w5"]], c("wle_w3", "se_w3")] <- NA
        }
    } else {
        pv <- datalist
        for(p in seq(npv)) {
            pv[[p]][n.valid$valid < nvalid, "PV"] <- NA
        }
        VAR <- mean(unlist(variance))
        MEAN <- mean(sapply(eap, FUN = function(x) mean(x[, 2])))
    }

    # output
    res <- list()
    res[['SC']] <- as.numeric(gsub(pattern = "SC", replacement = "", x = SC))
    res[['domain']] <- domain
    res[['wave']] <- as.numeric(gsub(pattern = "w", replacement = "", x = wave))
    res[['type']] <- type
    res[['rotation']] <- ifelse(rotation, 'Corrected For Test Position',
                                'No Correction For Test Position')
    res[['nvalid']] <- nvalid
    res[['model']] <- ifelse(PCM, 'Partial Credit Model', 'Rasch Model')
    res[['n.valid']] <- n.valid
    res[['npv']] <- npv
    res[['control']] <- control
    if (rotation)
        res[['position']] <- data.frame(ID_t, position)
    if (longitudinal){
        res[["abs.correction"]] <- sapply(meanvar[[SC]][[domain]],
                                          FUN = function(x) x[[1]])
    }
    res[["variance.PV"]] <- VAR
    res[["mean.PV"]] <- MEAN
    res[['pv']] <- pv

    if (control$EAP) {
        res[['eap']] <- eap
    }
    if (control$WLE)
        res[['wle']] <- wle
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
                                                                   ifelse(SC == "SC6" & domain == "RE", length(waves) + 1, length(waves)))]))
            }
        }
    }
    class(res) <- "pv.obj"
    return(res)
}
