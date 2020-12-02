#' estimation of longitudinal plausible values accomodating dichotomous and
#'   polytomous data
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param imp Imputed background data (either NULL if is.null(bgdata) or no
#'   missing data is in background data)
#' @param resp List of matrices of item responses given by test takers
#' @param waves Waves in which the competencies have been assessed (Strings in
#'   the form "_wX")
#' @param frmY Formula detailing how the background data is to be used for
#'   plausible values estimation -- defaults to NULL if is.null(bgdata) or to
#'   linear combination of all bgdata variables
#' @param ID_t Data.frame containing all IDs for all test takers of the
#'   selected waves
#' @param type String ("cross" for cross-sectional or "long" for longitudinal)
#' @param domain String detailing the competence domain
#' @param SC String detailing which starting cohort is used
#' @param control List of control variables for plausible values estimation
#'   algorithm
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPScaling::plausible_values()`
#'
#' @noRd

estimate_longitudinal <- function(bgdata, imp, frmY = NULL, resp, PCM, ID_t,
                                  waves, type, domain, SC, control, npv) {
  items <- lapply(xsi.fixed$long[[domain]][[SC]], rownames)
  if (SC == "SC2" && domain == "SC") {
    items <- items[-length(items)]
    PCM <- PCM[-length(PCM)]
  }

  res <- prepare_resp_q_longitudinal(PCM, resp, items, waves, SC, domain)
  resp <- res[["resp"]]
  Q <- res[["Q"]]

  times <- ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)
  pvs <- EAP.rel <- regr.coeff <- replicate(times, list(), simplify = FALSE)
  eap <- replicate(times, data.frame(ID_t = ID_t$ID_t), simplify = FALSE)
  for (i in 1:times) {
    res <- prepare_bgdata_frmY(imp, i, frmY)
    bgdatacom <- res[["bgdatacom"]]
    frmY <- res[["frmY"]]

    # estimate IRT model
    mod <- tmp_pvs <- list()
    for (j in seq(length(waves))) {
      mod[[j]] <- TAM::tam.mml(
        resp = resp[[j]][, items[[j]]],
        dataY = if (is.null(bgdata)) {
          NULL
        } else if (is.null(imp)) {
          bgdata[
            bgdata$ID_t %in% resp[[j]]$ID_t,
            -which(names(bgdata) == "ID_t"),
            drop = FALSE
          ]
        } else {
          bgdatacom[
            bgdatacom$ID_t %in% resp[[j]]$ID_t,
            -which(names(bgdatacom) == "ID_t"),
            drop = FALSE
          ]
        },
        formulaY = frmY,
        pid = resp[[j]]$ID_t,
        irtmodel = ifelse(PCM[[j]], "PCM2", "1PL"),
        xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub("_", "", waves)[j]]],
        Q = Q[[j]],
        verbose = FALSE
      )
      res <- post_process_long_tam_results(mod[[j]], npv, control, imp,
                                           bgdatacom, eap, i, j, EAP.rel,
                                           regr.coeff, bgdata, waves)
      tmp_pvs[[j]] <- res[["tmp_pvs"]]
      eap <- res[["eap"]]
      regr.coeff <- res[["regr.coeff"]]
      EAP.rel <- res[["EAP.rel"]]
    }
    pvs <- reformat_longitudinal_tmp_pvs(npv, pvs, i, tmp_pvs, bgdata)
    rm(tmp_pvs)
    colnames(eap[[i]]) <-
      c("ID_t", paste0(rep(c("eap", "se"), length(waves)), rep(waves, each = 2)))
  }
  res <- list(
    eap = eap, pvs = pvs, EAP.rel = EAP.rel,
    regr.coeff = regr.coeff, mod = mod
  )
  res
}
