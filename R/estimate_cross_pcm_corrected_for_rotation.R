#' estimation of cross-sectional plausible values (polytomous data) with
#'   correction for testlet position (multi-facet model)
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param imp Imputed background data (either NULL if is.null(bgdata) or no
#'   missing data is in background data)
#' @param resp Matrix of item responses given by test takers
#' @param waves Wave in which the competencies have been assessed (String in
#'   the form "_wX")
#' @param frmY Formula detailing how the background data is to be used for
#'   plausible values estimation -- defaults to NULL if is.null(bgdata) or to
#'   linear combination of all bgdata variables
#' @param ID_t Data.frame containing all IDs for the current test takers
#' @param type String ("cross" for cross-sectional or "long" for longitudinal)
#' @param domain String detailing the competence domain
#' @param SC String detailing which starting cohort is used
#' @param control List of control variables for plausible values estimation
#'   algorithm
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPScaling::plausible_values()`
#'
#' @noRd

estimate_cross_pcm_corrected_for_rotation <- function(
                                                      bgdata, imp, frmY = NULL,
                                                      waves, ID_t, resp, type,
                                                      domain, SC, control, npv,
                                                      position) {
  res <- collapse_categories_pcm(
    resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)]] ], SC,
    gsub("_", "", waves), domain
  )
  resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)]] ] <- res$resp
  ind <- score(SC, domain, gsub("_", "", waves))
  B <- TAM::designMatrices(
    modeltype = "PCM",
    resp = resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)]] ]
  )$B
  B[ind, , ] <- 0.5 * B[ind, , ]
  pvs <- list(NULL)
  EAP.rel <- NULL
  regr.coeff <- NULL
  eap <-
    replicate(
      ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi),
      data.frame(ID_t = ID_t$ID_t),
      simplify = FALSE
    )
  for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1,
    control$ML$nmi
  )) {
    if (!is.null(imp)) {
      bgdatacom <- imp[[i]]
      bgdatacom <- as.data.frame(apply(bgdatacom, 2, as.numeric))
      frmY <-
        as.formula(paste(
          "~",
          paste(colnames(bgdatacom)[-which(names(bgdatacom) == "ID_t")],
            collapse = "+"
          )
        ))
    }
    # estimate IRT model
    mod <- list()

    mod[[1]] <- TAM::tam.mml.mfr(
      formulaA = ~ 0 + item + item:step + position,
      facets = position,
      B = B,
      resp = resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)]] ],
      dataY = if (is.null(bgdata)) {
        NULL
      } else if (is.null(imp)) {
        bgdata[, -which(names(bgdata) == "ID_t"), drop = FALSE]
      } else {
        bgdatacom[, -which(names(bgdatacom) == "ID_t"), drop = FALSE]
      },
      formulaY = frmY,
      pid = resp$ID_t,
      irtmodel = "PCM2",
      xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub(
        "_", "",
        waves
      ) ]],
      verbose = FALSE
    )
    # post-processing of model
    res <- cross_sectional_post_proc(mod[[1]], npv, control,
      Y = if (is.null(bgdata)) {
        NULL
      } else if (is.null(imp)) {
        bgdata
      } else {
        bgdatacom
      },
      Y.pid = if (is.null(bgdata)) {
        NULL
      } else {
        "ID_t"
      },
      eap, i, EAP.rel, regr.coeff, pvs, bgdata
    )
    eap <- res$eap
    regr.coeff <- res$regr.coeff
    pvs <- res$pvs
    mod[[1]] <- res$mod
    EAP.rel <- res$EAP.rel
  }
  res <- list(
    eap = eap, pvs = pvs, mod = mod, EAP.rel = EAP.rel,
    regr.coeff = regr.coeff
  )
  return(res)
}
