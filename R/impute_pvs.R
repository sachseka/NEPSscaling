#' impute pvs and format tmp_pvs list
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPSscaling::plausible_values()`
#' @param j numeric; 1 (cross) or number of wave (long)
#' @param control list of arguments for TAM::tam.pv() routine
#' @param mod tam.obj
#' @param imp imputations of bgdata or NULL
#' @param bgdata data.frame; complete background data or NULL
#'
#' @noRd
#'
impute_pvs <- function(mod, npv, control, bgdata, imp, waves, j) {
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
    tmp_pvs <- TAM::tampv2datalist(
        pmod,
        Y.pid = if (is.null(bgdata)) {NULL} else {"ID_t"},
        Y = bgdata,
        pvnames = paste0("PV", waves[j])
    )
    tmp_pvs
}
