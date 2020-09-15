#' impute pvs and format tmp_pvs list
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPScaling::plausible_values()`
#' @param j ...
#' @param control ...
#' @param mod ...
#' @param imp ...
#' @param bgdatacom ...
#'
#' @noRd
#'
impute_pvs <- function(mod, npv, control, bgdata, imp, bgdatacom = NULL, waves, j) {
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
        Y = if (is.null(bgdata)) {
            NULL
        } else if (is.null(imp)) {
            bgdata
        } else {
            bgdatacom
        },
        pvnames = paste0("PV", waves[j])
    )
    tmp_pvs
}
