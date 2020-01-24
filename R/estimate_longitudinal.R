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
#' @param Q List of matrices detailing the item scoring (0.5 scoring for PCM
#'   items)
#'
#' @noRd

estimate_longitudinal <- function(bgdata, imp, frmY = NULL, resp, Q,
                                  PCM, ID_t, waves, type, domain, SC,
                                  control, npv) {
  . <- NULL
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  for (i in seq(length(PCM))) {
    if (PCM[[i]]) {
      res <- collapse_categories_pcm(
        resp[[i]][, item_labels[[SC]][[domain]][[i]]], SC,
        gsub("_", "", waves)[i], domain
      )
      resp[[i]][, item_labels[[SC]][[domain]][[i]]] <- res$resp
      ind <- get_indicators_for_half_scoring(
        SC, domain,
        gsub("_", "", waves)[i]
      )
      Q[[i]][ind, ] <- 0.5
    }
  }
  pvs <- replicate(ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1,
    control$ML$nmi
  ), list(), simplify = FALSE)
  EAP.rel <- replicate(length(waves), list(), simplify = FALSE)
  regr.coeff <- replicate(length(waves), list(), simplify = FALSE)
  eap <-
    replicate(
      ifelse(
        is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi
      ),
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
    tmp_pvs <- list()
    pmod <- list()

    for (j in seq(length(waves))) {
      mod[[j]] <- TAM::tam.mml(
        resp = resp[[j]][, item_labels[[SC]][[domain]][[j]]],
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
        xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub(
          "_", "",
          waves
        )[j] ]],
        Q = Q[[j]],
        verbose = FALSE
      )
      # impute plausible values
      pmod[[j]] <- TAM::tam.pv(mod[[j]],
        nplausible = npv,
        ntheta = control$ML$ntheta,
        normal.approx = control$ML$normal.approx,
        samp.regr = control$ML$samp.regr,
        theta.model = control$ML$theta.model,
        np.adj = control$ML$np.adj,
        na.grid = control$ML$na.grid,
        verbose = FALSE
      )
      tmp_pvs[[j]] <- TAM::tampv2datalist(
        pmod[[j]],
        Y.pid = if (is.null(bgdata)) {
          NULL
        } else {
          "ID_t"
        },
        Y = if (is.null(bgdata)) {
          NULL
        } else if (is.null(imp)) {
          bgdata
        } else {
          bgdatacom
        },
        pvnames = paste0("PV", waves[j])
      )
      eap[[i]] <-
        dplyr::left_join(eap[[i]],
          mod[[j]]$person[, grep(
            "pid|EAP",
            names(mod[[j]]$person)
          )],
          by = c("ID_t" = "pid")
        )
      EAP.rel[[j]] <- c(EAP.rel[[j]], mod[[j]]$EAP.rel)
      regr.coeff[[j]] <- cbind(regr.coeff[[j]], quiet(TAM::tam.se(mod[[j]])$beta))
    }
    for (n in 1:npv) {
      pvs[[i]][[n]] <-
        suppressMessages(lapply(tmp_pvs, function(x) {
          x[[n]]
        }) %>%
          Reduce(
            function(df1, df2) {
              df2 <- df2[, grepl(
                "ID_t|pid|PV",
                names(df2)
              )]
              dplyr::full_join(df1, df2)
            }, .
          ))
      if (is.null(bgdata)) {
        names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <-
          "ID_t"
      }
    }
    rm(tmp_pvs)
    colnames(eap[[i]]) <-
      c("ID_t", paste0(
        rep(c("eap", "se"), length(waves)),
        rep(waves, each = 2)
      ))
  }
  res <- list(
    eap = eap, pvs = pvs, EAP.rel = EAP.rel,
    regr.coeff = regr.coeff, mod = mod
  )
  res
}
