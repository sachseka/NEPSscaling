#' Function for estimating plausible values with NEPS competence data and NEPS
#' scaled item parameters
#'
#' @param SC           numeric. The starting cohort used for the analysis is
#' indicated by an integer value (e.g., starting cohort one = 1).
#' @param domain       character. The competence domain of interest is indicated
#' by the domain abbreviation as used in the variable names (see Fuß, D.,
#' Gnambs, T., Lockl, K., & Attig, M., 2016).
#' @param wave         numeric. The wave of competence testing is indicated by
#' an integer value (e.g., wave one = 1).
#' @param path         character; file path leading to the location of the
#' competence data
#' @param filetype     filetype of competence data (the NEPS RDC provides SPSS
#' and Stata data files)
#' @param bgdata       data frame containing background variables. Categorical
#' variables have to be specified as factors. If \code{bgdata = NULL}, plausible
#' values are estimated without a background model. Missing data in the
#' covariates is imputed using sequential classification and regression trees.
#' @param npv          numeric; number of plausible values to be estimated;
#' defaults to 10.
#' @param nvalid       numeric; minimum number of responses test takers had to
#' give for inclusion into the estimation process. NEPS default is at least
#' three.
#' @param longitudinal logical. TRUE indicating that a multidimensional model
#' with the measurement points as its dimensions is to be estimated. Defaults to
#' FALSE.
#' @param rotation     logical. TRUE indicating that the competence scores
#' should be corrected for the rotation design of the booklets. Defaults to
#' TRUE. If both longitudinal and rotation are TRUE, rotation is set to FALSE
#' automatically.
#' @param include.nr logical; whether the number of not-reached items as a proxy
#' for processing speed should be included in the background model (the default
#' is TRUE)
#' @param verbose logical; whether progress should be displayed in the console
#' (the default is TRUE)
#' @param control      list of additional options. If \code{EAP = TRUE}, the
#' EAPs will be returned as well. Furthermore, additional control options for
#' "ML" (again in form of a list) are \code{nmi} (the number of multiple
#' imputations if there is missing data in the covariates) and arguments
#' concerning the ML estimation given to TAM::tam.pv() (therefore, see TAM for
#' further control options). Analogously, control options for "Bayes" (in form
#' of a list) are "itermcmc" (number of MCMC iterations), "burnin" (number of
#' burn-in iterations), "est.alpha" (whether item discriminations of the graded
#' response model are to be fixed or estimated freely; default: fixed), "thin"
#' (thinning interval in MCMC), "tdf" (df of multiv. t proposal distr. for
#' category cut-offs for ordinal items), "cartctrl1" (min. number of
#' observations in any terminal CART node) and "cartctrl2" (min. decrease of
#' overall lack of fit by each CART split). NOTE that "Bayes" control options
#' are also applied to the CART algorithm for "ML" estimation.
#'
#' @return \code{plausible_values()} returns an object of class \code{pv.obj}
#' containing:
#' \describe{
#' \item{SC}{Starting cohort that plausible values were estimated for}
#' \item{domain}{Competence domain that plausible values were estimated for}
#' \item{wave}{Wave that plausible values were estimated for}
#' \item{method}{Estimation algorithm that was used for estimating plausible
#' values (\code{ML} for the EM algorithm implemented in \code{TAM} or
#' \code{Bayes} for the Gibbs Sampler implemented in \code{LaRA})}
#' \item{type}{Whether cross-sectional ("cross") or longitudinal ("long")
#' plausible values were estimated}
#' \item{rotation}{In most assessments the position of the competence test was
#' rotated with another competence domain. If this was the case for the
#' specific estimation, this variable indicates whether the correction was
#' applied. Depending on the estimation context, this variable may have been
#' automatically set by the function and thus differ from user input}
#' \item{nvalid}{The minimum number of answers a test taker must have given}
#' \item{model}{If the method \code{ML} was chosen, a Rasch or a Partial Credit
#' Model was estimated, depending on the item format. If \code{Bayes} was
#' chosen, a Graded Response Model was estimated.}
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
#' \item{alpha}{If the method \code{Bayes} and, in the controls,
#' \code{est.alpha} were chosen, the item discrimination parameters (as the
#' EAPs) are returned}
#' \item{EAP.rel}{For method \code{ML} the EAP reliability is returned}
#' }
#'
#' @references Albert, J. H. (1992). Bayesian estimation of normal ogive item
#' response curves using Gibbs sampling. \emph{Journal of Educational
#' Statistics}, \emph{17}(3), 251-269.
#' @references Azur, M. J., Stuart, E. A., Frangakis, C., & Leaf, P. J. (2011).
#' Multiple imputation by chained equations: what is it and how does it work?
#' \emph{International Journal of Methods in Psychiatric Research, 20}(1), 40-49.
#' @references Blossfeld, H.-P., Rossbach, H.-G., & von Maurice,  J. (Eds.)
#' (2011). Education as a Lifelong Process - The German National Educational
#' Panel Study (NEPS). \emph{Zeitschrift fuer Erziehungswissenschaft,
#' Sonderheft 14}.
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
#' @references Weirich, S., Haag, N., Hecht, M., Boehme, K., Siegle, T., &
#' Luedtke, O. (2014). Nested multiple imputation in large-scale assessments.
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
#' path <- "Usr/NEPS-data/"
#' setwd(path)
#' ## save simulated target competencies to path
#' data(xTargetCompetencies_sim)
#' write.dta(
#'   dataframe = xTargetCompetencies_sim,
#'   file = "SC6_xTargetCompetencies_sim.dta"
#' )
#' ## estimate default number of 10 plausible values
#' ## note: the example background data is completely observed!
#' result <- plausible_values(
#'   SC = 6,
#'   domain = "RE",
#'   wave = 3,
#'   bgdata = bg_data,
#'   path = path,
#'   method = "Bayes",
#'   filetype = "Stata"
#' )
#' }
#'
#' @note For an extensive example, see the accompanying NEPS Survey Paper.
#' The function will only work with NEPS data. To access NEPS data see
#' https://www.neps-data.de/en-us/datacenter/dataaccess.aspx.
#'
#' @importFrom stats prcomp
#' @importFrom stats as.formula
#' @importFrom utils flush.console
#'
#' @export

plausible_values <- function(SC,
                             domain = c(
                               "MA", "RE", "SC", "IC", "LI", "EF",
                               "NR", "NT", "OR", "ST", "BA", "CD",
                               "GR"
                             ),
                             wave,
                             path,
                             filetype = c("SPSS", "Stata"),
                             bgdata = NULL,
                             npv = 10L,
                             longitudinal = FALSE,
                             rotation = TRUE,
                             nvalid = 3L,
                             include.nr = TRUE,
                             verbose = TRUE,
                             control = list(
                               EAP = FALSE, WLE = FALSE,
                               Bayes = list(
                                 itermcmc = 10000,
                                 burnin = 2000,
                                 est.alpha = FALSE,
                                 thin = 1, tdf = 10,
                                 cartctrl1 = 5,
                                 cartctrl2 = 0.0001
                               ),
                               ML = list(
                                 nmi = 10L, ntheta = 2000,
                                 normal.approx = FALSE,
                                 samp.regr = FALSE,
                                 theta.model = FALSE,
                                 np.adj = 8, na.grid = 5,
                                 itermcmc = 100,
                                 burnin = 50, thin = 1,
                                 cartctrl1 = 5,
                                 cartctrl2 = 0.0001
                               )
                             )) {
  rea9_sc1u <- wave_w3 <- wave_w5 <- . <- NULL
  # check and prepare arguments
  if (missing(SC)) {
    stop("Starting cohort must be specified.")
  }
  if (!is.numeric(SC) || !is.numeric(wave)) {
    stop("Starting cohort and wave must be numeric.")
  }
  SC <- paste0("SC", SC)
  if (!longitudinal && missing(wave)) {
    stop("Wave must be specified for cross-sectional modeling.")
  } else {
    wave <- paste0("w", wave)
  }
  domain <- toupper(domain)
  domain <- match.arg(domain)
  filetype <- match.arg(filetype)
  if (missing(path)) {
    stop("Path must be specified.")
  }
  if (!is.character(path) || !grepl("/$", path)) {
    stop("Path must be a character string and end in '/'.")
  }
  if (nvalid < 0) {
    stop("nvalid must be non-negative.")
  }
  if (is.null(item_labels[[SC]][[domain]][[wave]])) {
    stop(paste0(
      "No competence data available for ", SC, " ", domain, " ",
      wave, "."
    ))
  }
  if (longitudinal && ((SC == "SC6" | SC == "SC5") &
    domain %in% c("IC", "SC", "BA", "EF"))) {
    longitudinal <- FALSE
    message(paste0(
      "No longitudinal data for: ", SC, " ", domain,
      ". Estimating cross-sectional plausible values instead."
    ))
  }
  if (verbose) {
    cat("Begin pre-processing of data... ", paste(Sys.time()), "\n")
    flush.console()
  }
  if (longitudinal) {
    type <- "long"
    if (SC == "SC6") {
      if (domain == "RE") {
        waves <- c("_w3", "_w5", "_w9")
      }
      if (domain == "MA") {
        waves <- c("_w3", "_w9")
      }
    }
    if (SC == "SC5" && domain %in% c("RE", "MA")) {
      waves <- c("_w1", "_w12")
    }
    if (SC == "SC4") {
      if (domain == "RE") {
        waves <- c("_w2", "_w7", "_w10")
      }
      if (domain == "MA") {
        waves <- c("_w1", "_w7", "_w10")
      }
      if (domain == "IC") {
        waves <- c("_w1", "_w7")
      }
      if (domain == "SC") {
        waves <- c("_w1", "_w5")
      }
      if (domain == "EF") {
        waves <- c("_w3", "_w7")
      }
    }
  } else {
    type <- "cross"
    waves <- paste0("_", wave)
  }

  # get competence data for SC and domain
  files <- list.files(path = path)
  if (filetype == "SPSS") {
    if (SC == "SC5" & domain == "BA") {
      data <-
        tryCatch(
          {
            haven::read_spss(
              file = paste0(path, files[grep("xEcoCAPI", files)]),
              user_na = TRUE
            )
          },
          error = function(e) {
            stop(paste0(
              "Path ", path,
              " does not lead to competence files OR ",
              "wrong file formats!"
            ))
          }
        )
    } else {
      data <-
        tryCatch(
          {
            haven::read_spss(
              file = paste0(
                path,
                files[grep("xTargetCompetencies", files)]
              ),
              user_na = TRUE
            )
          },
          error = function(e) {
            stop(paste0(
              "Path ", path,
              " does not lead to competence files OR ",
              "wrong file formats!"
            ))
          }
        )
    }
  } else {
    if (SC == "SC5" & domain == "BA") {
      data <-
        tryCatch(
          {
            haven::read_dta(
              file = paste0(path, files[grep("xEcoCAPI", files)]),
              user_na = TRUE
            )
          },
          error = function(e) {
            stop(paste0(
              "Path '", path,
              " does not lead to competence files OR ",
              "wrong file formats!"
            ))
          }
        )
    } else {
      data <-
        tryCatch(
          {
            haven::read_dta(
              file = paste0(
                path,
                files[grep("xTargetCompetencies", files)]
              ),
              user_na = TRUE
            )
          },
          error = function(e) {
            stop(paste0(
              "Path '", path,
              " does not lead to competence files OR ",
              "wrong file formats!"
            ))
          }
        )
    }
  }
  data <- data[order(data$ID_t), ]

  # number of not-reached items as processing time proxy
  if (include.nr) {
    sel <- (if (longitudinal) {
      names(data) %in% unique(unlist(item_labels[[SC]][[domain]]))
    } else {
      names(data) %in% item_labels[[SC]][[domain]][[wave]]
    })
    nr <- data.frame(ID_t = data$ID_t, nr = rowSums(data[, sel] == -94))
  }
  data[data < -20] <- NA

  # test data and test taker selection
  if (longitudinal) {
    resp <- list()
    if (SC == "SC6" && domain == "RE") {
      resp[[1]] <-
        data[
          data$wave_w3 == 1,
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w3"]]
          )
        ]
      resp[[1]] <- resp[[1]][rowSums(!is.na(resp[[1]][, -1])) >= nvalid, ]
      resp[[1]] <- resp[[1]][order(resp[[1]]$ID_t), ]
      resp[[2]] <-
        data[
          data$wave_w3 == 0 & data$wave_w5 == 1,
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w5"]]
          )
        ]
      resp[[2]] <- resp[[2]][rowSums(!is.na(resp[[2]][, -1])) >= nvalid, ]
      resp[[2]] <- resp[[2]][order(resp[[2]]$ID_t), ]
      resp[[3]] <-
        data[
          !is.na(data$rea9_sc1u),
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w9"]]
          )
        ]
      resp[[3]] <- resp[[3]][rowSums(!is.na(resp[[3]][, -1])) >= nvalid, ]
      resp[[3]] <- resp[[3]][order(resp[[3]]$ID_t), ]
    } else {
      for (i in seq(length(item_labels[[SC]][[domain]]))) {
        resp[[i]] <-
          data[, names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][[i]]
          )]
        resp[[i]] <-
          resp[[i]][rowSums(!is.na(resp[[i]][, -1])) >= nvalid, ]
        resp[[i]] <- resp[[i]][order(resp[[i]]$ID_t), ]
      }
    }
    data <-
      data[data$ID_t %in% unique(
        unlist(lapply(resp, function(x) {
          x[["ID_t"]]
        }))
      ), ]
    data <- data[order(data$ID_t), ]
  } else {
    # reading has been tested twice for different samples in SC6:
    # estimating simultaneously might cause problems because of different
    # information available for background model (large quantities of missing data)
    if (SC == "SC6" & domain == "RE") {
      if (wave == "w3") {
        data <- data[data$wave_w3 == 1, ]
      } else if (wave == "w5") {
        data <- data[data$wave_w3 == 0 & data$wave_w5 == 1, ]
      }
    }
    resp <-
      data[, names(data) %in% c(
        "ID_t",
        item_labels[[SC]][[domain]][[wave]]
      )]
    resp <- resp[rowSums(!is.na(resp[, -1])) >= nvalid, ]
    resp <- resp[order(resp$ID_t), ]
    data <- data[data$ID_t %in% resp$ID_t, ]
    data <- data[order(data$ID_t), ]
  }

  # check for Partial Credit Items
  PCM <- (if (longitudinal) {
    lapply(resp, function(x) {
      max(apply(x[, -1], 2, max, na.rm = TRUE)) > 1
    })
  }
  else {
    max(apply(resp[, -1], 2, max, na.rm = TRUE)) > 1
  })

  # process background data
  if (is.null(bgdata)) {
    ID_t <- data[, "ID_t", drop = FALSE]
    if (include.nr) {
      bgdata <- dplyr::left_join(ID_t, nr, by = "ID_t")
    }
  } else {
    if (!is.data.frame(bgdata)) {
      stop("bgdata must be a data.frame.")
    }
    if (is.null(bgdata$ID_t)) {
      stop("ID_t must be included in bgdata.")
    }
    bgdata <- bgdata[order(bgdata$ID_t), ]

    if (nvalid > 0) {
      bgdata <- bgdata[bgdata$ID_t %in% data$ID_t, ]
      if (nrow(bgdata) < nrow(data)) {
        bgdata <- suppressWarnings(
          dplyr::bind_rows(
            bgdata,
            data[!(data$ID_t %in% bgdata$ID_t),
              "ID_t",
              drop = FALSE
            ]
          )
        )
      }
      bgdata <- bgdata[order(bgdata$ID_t), ]
    } else {
      # append subjects in background data that did not take the competence tests
      data <- suppressWarnings(
        dplyr::bind_rows(data, bgdata[!(bgdata$ID_t %in% data$ID_t),
          "ID_t",
          drop = FALSE
        ])
      )
      data <- data[order(data$ID_t), ]
      bgdata <- suppressWarnings(
        dplyr::bind_rows(
          bgdata,
          data[!(data$ID_t %in% bgdata$ID_t),
            "ID_t",
            drop = FALSE
          ]
        )
      )
      bgdata <- bgdata[order(bgdata$ID_t), ]
    }
    if (include.nr) {
      bgdata <- dplyr::left_join(bgdata, nr, by = "ID_t")
    }
    ID_t <- bgdata[, "ID_t", drop = FALSE]

    # list of categorical variables
    fac <- unlist(lapply(bgdata, is.factor))
    categorical <- names(bgdata)[fac]
  }
  # number of valid responses per person (and wave)
  if (longitudinal) {
    n.valid <-
      data.frame(ID_t = unique(
        unlist(lapply(resp, function(x) {
          x[["ID_t"]]
        }))
      ))
    for (w in seq(length(waves))) {
      tmp <-
        data.frame(
          ID_t = resp[[w]]$ID_t,
          rowSums(!is.na(resp[[w]][, -1]))
        )
      n.valid <- dplyr::left_join(n.valid, tmp, by = "ID_t")
      names(n.valid)[w + 1] <- paste0("valid", waves[w])
    }
    rm(tmp)
  } else {
    n.valid <- data.frame(ID_t = resp$ID_t)
    n.valid$valid <- rowSums(!is.na(resp[, -1]))
  }

  # consider test form rotation
  if (longitudinal) {
    rotation <- FALSE
    Q <- qmat(SC, domain)
  } else {
    if (rotation) {
      position <- data.frame(
        ID_t = data$ID_t,
        position = rep(NA, nrow(data))
      )
      # construct facet to correct for rotation design
      if (SC == "SC1") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC2") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC3") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC4") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC5") { # w7 does not have rotation and can be ignored here
        if (wave == "w1") {
          position[, "position"] <- data[, "tx80211_w1"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position == 126),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position == 127),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position == 127),
              "position"
            ] <- 1 # maths first
            position[
              !is.na(position$position) &
                (position$position == 126),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          }
        } else if (wave == "w5") {
          position[, "position"] <- data[, "tx80211_w5"]
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                (position$position %in% c(331, 333, 335)),
              "position"
            ] <- 1 # ict first
            position[
              !is.na(position$position) &
                (position$position %in% c(330, 332, 334, 336)),
              "position"
            ] <- 2 # ict second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ICT first" = 1,
                  "ICT second" = 2
                )
              )
          } else if (domain == "SC") {
            position[
              !is.na(position$position) &
                (position$position %in% c(330, 332, 334, 336)),
              "position"
            ] <- 1 # sc first
            position[
              !is.na(position$position) &
                (position$position %in% c(331, 333, 335)),
              "position"
            ] <- 2 # science second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "science first" = 1,
                  "science second" = 2
                )
              )
          }
        } else if (wave == "w12") {
          position[, "position"] <- data[, "tx80211_w12"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(459, 462, 465, 468)),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position %in% c(458, 463, 464, 469)),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(458, 460, 464, 466)),
              "position"
            ] <- 1 # math first
            position[
              !is.na(position$position) &
                (position$position %in% c(459, 461, 465, 467)),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          } else if (domain == "EF") {
            position[
              !is.na(position$position) &
                (position$position %in% c(461, 463, 467, 469)),
              "position"
            ] <- 1 # English first
            position[
              !is.na(position$position) &
                (position$position %in% c(460, 462, 466, 468)),
              "position"
            ] <- 2 # English second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "English first" = 1,
                  "English second" = 2
                )
              )
          }
        }
      } else if (SC == "SC6") {
        if (wave == "w3") {
          position[, "position"] <- data[, "tx80211_w3"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(123, 125)),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position %in% c(122, 124)),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(122, 124)),
              "position"
            ] <- 1 # maths first
            position[
              !is.na(position$position) &
                (position$position %in% c(123, 125)),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          }
        } else if (wave == "w5") {
          position[, "position"] <- data[, "tx80211_w5"]
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                position$position == 248,
              "position"
            ] <- 1 # ict first
            position[
              !is.na(position$position) &
                position$position == 247,
              "position"
            ] <- 2 # ict second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ict first" = 1,
                  "ict second" = 2
                )
              )
          } else if (domain == "SC") {
            position[
              !is.na(position$position) &
                position$position == 247,
              "position"
            ] <- 1 # science first
            position[
              !is.na(position$position) &
                position$position == 248,
              "position"
            ] <- 2 # science second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "science first" = 1,
                  "science second" = 2
                )
              )
          } else if (domain == "RE") {
            position[
              !is.na(position$position) &
                position$position == 249,
              "position"
            ] <- 1 # no rotation should be found
          }
        } else if (wave == "w9") {
          position[, "position"] <- data[, "tx80211_w9"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                position$position %in%
                  c(444, 445, 448, 449, 452:455),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                position$position %in%
                  c(446, 447, 450, 451, 456, 457),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                position$position %in%
                  c(446, 447, 450, 451, 456, 457),
              "position"
            ] <- 1 # math first
            position[
              !is.na(position$position) &
                position$position %in%
                  c(444, 445, 448, 449, 452:455),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math math" = 2
                )
              )
          }
        }
      }
      # possible NAs lead to further data selection
      position <- position[!is.na(position$position), ]
      rotation <- length(unique(position$position)) > 1 # T: with rotation, F: without rotation
      if (rotation) {
        data <- data[data$ID_t %in% position$ID_t, ]
        resp <- resp[resp$ID_t %in% position$ID_t, ]
        ID_t <- ID_t[ID_t$ID_t %in% position$ID_t, , drop = FALSE]
        if (!is.null(bgdata)) {
          bgdata <- bgdata[bgdata$ID_t %in% position$ID_t, ]
        }
        # # possible NAs in position variable treated as third group
        # position[is.na(position$position), "position"] <- 3
        # format position effect information
        position <- position[, 2, drop = FALSE]
      }
    }
  }

  # complement control lists
  res <- complement_control_lists( # control$Bayes,
    control$EAP, control$WLE, control$ML
  )
  # control$Bayes <- res$Bayes
  control$ML <- res$ML
  control$EAP <- res$EAP
  control$WLE <- res$WLE


  # multiple imputation of missing covariate data
  if (!is.null(bgdata)) {
    if (any(is.na(bgdata))) {
      if (verbose) {
        cat(
          "Begin multiple imputation of missing background data... ",
          paste(Sys.time()), "\n"
        )
        flush.console()
      }
      imp <- CART(
        X = bgdata, itermcmc = control$ML$itermcmc,
        burnin = control$ML$burnin, nmi = control$ML$nmi,
        thin = control$ML$thin, cartctrl1 = control$ML$cartctrl1,
        cartctrl2 = control$ML$cartctrl2
      )
    } else {
      bgdata <- as.data.frame(lapply(bgdata, as.numeric))
      imp <- NULL
      frmY <-
        as.formula(
          paste(
            "~",
            paste(
              colnames(
                bgdata[, -which(colnames(bgdata) == "ID_t"),
                  drop = FALSE
                ]
              ),
              collapse = "+"
            )
          )
        )
    }
  } else {
    imp <- frmY <- NULL
  }

  if (verbose) {
    cat(
      "Begin estimation... ", paste(Sys.time()),
      "\nThis might take some time.\n"
    )
    flush.console()
  }

  if (longitudinal) {
    res <- longitudinal_estimation(bgdata, imp,
      frmY = NULL, resp, Q,
      PCM, ID_t, waves, type, domain, SC,
      control, npv
    )
  } else {
    if (rotation) {
      if (PCM) {
        res <- cross_sectional_PCM_rotation(bgdata, imp,
          frmY = NULL,
          waves, ID_t, resp,
          type, domain, SC, control,
          npv, position
        )
      } else {
        res <- cross_sectional_rotation(bgdata, imp,
          frmY = NULL, resp,
          position, waves, ID_t, type,
          domain, SC, control, npv
        )
      }
    } else {
      if (PCM) {
        res <- estimate_cross_pcm_uncorrected(bgdata, imp, resp, waves,
          frmY = NULL, ID_t, type, domain, SC,
          control, npv
        )
      } else {
        res <- estimate_cross_rasch_uncorrected(bgdata, imp, resp, waves,
          frmY = NULL, ID_t, type, domain, SC,
          control, npv
        )
      }
    }
  }
  eap <- res$eap
  pvs <- res$pvs
  EAP.rel <- res$EAP.rel
  regr.coeff <- res$regr.coeff
  mod <- res$mod
  if (verbose) {
    cat(
      "Finished estimation. Begin post-processing... ", paste(Sys.time()),
      "\n"
    )
    flush.console()
  }
  # assumption: eaps are equivalent over all estimations
  eap <- eap[[1]]
  if (control$WLE) {
    if (longitudinal) {
      wmod <- list()
      for (j in seq(length(waves))) {
        wmod[[j]] <- TAM::tam.mml.wle2(mod[[j]],
          WLE = TRUE,
          progress = FALSE
        )
      }
      wmod <- wmod %>%
        Reduce(function(df1, df2) {
          dplyr::full_join(df1, df2, by = "pid")
        }, .)
    } else {
      wmod <- TAM::tam.mml.wle2(mod[[1]], WLE = TRUE, progress = FALSE)
    }
    wle <- wmod[grep("pid|theta|error", colnames(wmod))]
    colnames(wle) <-
      c("ID_t", paste0(
        rep(c("wle", "se"), length(waves)),
        rep(if (longitudinal) {
          waves
        } else {
          ""
        },
        each = 2
        )
      ))
  }
  datalist <- list()
  d <- 1
  for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1,
    control$ML$nmi
  )) {
    for (j in 1:npv) {
      datalist[[d]] <- pvs[[i]][[j]][, !grepl(
        "pweights|test_postition",
        colnames(pvs[[i]][[j]])
      )]
      datalist[[d]] <- datalist[[d]] %>%
        dplyr::select(ID_t, dplyr::everything())
      d <- d + 1
    }
  }
  pvs <- NULL
  ind <- sample(1:length(datalist), npv)
  datalist <- datalist[ind]


  # linear transformation of longitudinal PVs to pre-defined scale
  if (longitudinal) {
    for (p in seq(npv)) {
      for (w in waves) {
        for (i in seq(nrow(n.valid))) {
          if (is.na(n.valid[i, paste0("valid", w)]) ||
            n.valid[i, paste0("valid", w)] < nvalid) {
            datalist[[p]][i, paste0("PV", w)] <- NA
          }
        }
      }
    }
    for (w in waves) {
      for (j in seq(nrow(n.valid))) {
        if (is.na(n.valid[j, paste0("valid", w)]) ||
          n.valid[j, paste0("valid", w)] < nvalid) {
          eap[j, paste0(c("eap", "se"), w)] <- NA
        }
      }
    }
    # longitudinal subsamples
    longitudinal_IDs <- list()
    if (SC == "SC6" && domain == "RE") {
      longitudinal_IDs[["w3"]] <- dplyr::filter(
        data, wave_w3 == 1,
        !is.na(rea9_sc1u)
      )$ID_t
      longitudinal_IDs[["w5"]] <- dplyr::filter(
        data, wave_w3 == 0,
        wave_w5 == 1,
        !is.na(rea9_sc1u)
      )$ID_t
    } else {
      for (i in seq(2, length(waves))) {
        longitudinal_IDs[[i - 1]] <-
          dplyr::filter(
            eap, !is.na(eap[paste0("eap", waves[i - 1])]),
            !is.na(eap[paste0("eap", waves[i])])
          )$ID_t
      }
    }
    # re-scaling for longitudinal link
    res <- scale_person_estimates(
      pv = datalist,
      wle = if (control$WLE) {
        wle
      } else {
        NULL
      },
      eap = eap,
      SC = SC, domain = domain,
      wave = gsub("_", "", waves),
      longitudinal_IDs = longitudinal_IDs
    )
    pv <- res$pv
    wle <- res$wle
    eap <- res$eap
  } else {
    pv <- datalist
    for (p in seq(npv)) {
      pv[[p]][n.valid$valid < nvalid, "PV"] <- NA
    }
  }
  MEAN <- colMeans(eap[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE],
    na.rm = TRUE
  )
  names(MEAN) <- gsub("_", "", waves)

  # output
  res <- list()
  res[["SC"]] <- as.numeric(gsub(pattern = "SC", replacement = "", x = SC))
  res[["domain"]] <- domain
  res[["wave"]] <- as.numeric(gsub(pattern = "w", replacement = "", x = wave))
  res[["type"]] <- type
  res[["rotation"]] <- ifelse(rotation, "Corrected For Test Position",
    "No Correction For Test Position"
  )
  res[["nvalid"]] <- nvalid
  res[["model"]] <- ifelse(PCM, "Partial Credit Model", "Rasch Model")
  res[["n.valid"]] <- n.valid
  res[["npv"]] <- npv
  res[["control"]] <- control
  if (rotation) {
    res[["position"]] <- data.frame(ID_t, position)
  }
  res[["mean.PV"]] <- MEAN
  res[["pv"]] <- pv

  if (control$EAP) {
    res[["eap"]] <- eap
  }
  if (control$WLE) {
    res[["wle"]] <- wle
  }
  res[["EAP.rel"]] <- EAP.rel
  res[["regr.coeff"]] <- regr.coeff
  if (!is.null(bgdata)) {
    if (longitudinal) {
      for (w in seq(length(waves))) {
        rownames(res[["regr.coeff"]][[w]]) <-
          c(
            "Intercept",
            names(res[["pv"]][[1]][
              ,
              2:(ncol(res[["pv"]][[1]]) -
                length(waves))
            ])
          )
      }
    } else {
      rownames(res[["regr.coeff"]]) <-
        c(
          "Intercept",
          names(res[["pv"]][[1]][
            ,
            2:(ncol(res[["pv"]][[1]]) - 1)
          ])
        )
    }
  }
  res[["items"]] <- mod[[1]]$xsi
  class(res) <- "pv.obj"
  return(res)
}
