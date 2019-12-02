# model post-processing in cross-sectional estimation

cross_sectional_post_proc <- function(mod, npv, control, Y, Y.pid, eap, i,
                                      EAP.rel, regr.coeff, pvs, bgdata) {

  # impute plausible values
  pmod <- TAM::tam.pv(mod,
    nplausible = npv,
    ntheta = control$ML$ntheta,
    normal.approx = control$ML$normal.approx,
    samp.regr = control$ML$samp.regr,
    theta.model = control$ML$theta.model,
    np.adj = control$ML$np.adj,
    na.grid = control$ML$na.grid,
    verbose = FALSE
  )
  tmp_pvs <- TAM::tampv2datalist(pmod, Y.pid = Y.pid, Y = Y, pvnames = "PV")
  eap[[i]] <-
    dplyr::left_join(eap[[i]],
      mod$person[, grep(
        "pid|EAP",
        names(mod$person)
      )],
      by = c("ID_t" = "pid")
    )
  EAP.rel <- c(EAP.rel, mod$EAP.rel)
  if (i == 1) {
    regr.coeff <- mod$beta
  } else if (i > 1) {
    regr.coeff <- cbind(regr.coeff, mod[[1]]$beta)
  }
  pvs[[i]] <- lapply(tmp_pvs, function(x) {
    x[, -which(colnames(x) == "pweights")]
  })
  if (is.null(bgdata)) {
    for (n in 1:npv) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <-
        "ID_t"
    }
  }
  colnames(eap[[i]]) <- c("ID_t", "eap", "se")

  return(list(
    eap = eap, regr.coeff = regr.coeff, pvs = pvs, mod = mod,
    EAP.rel = EAP.rel
  ))
}
