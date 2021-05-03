#' reformat tmp_pvs list
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPSscaling::plausible_values()`
#' @param i numeric; number of current imputation
#' @param pvs list of lists; output containing the pvs of all waves in one
#' data.frame per imputation
#' @param tmp_pvs list; contains the raw estimation of TAM::tam.pv()
#'
#' @noRd

reformat_longitudinal_tmp_pvs <- function(npv, pvs, i, tmp_pvs, bgdata) {
  # tmp_pv: list of length(waves), each containing list of npv estimations
  for (n in 1:npv) {
    pvs[[i]][[n]] <- suppressWarnings(
      suppressMessages(lapply(tmp_pvs, function(x) { # address each wave
        x[[n]] # take the nth pv for each wave
      }) %>% # result: list of data.frames for each wave / nth pv estimation
        purrr::reduce(
          function(df1, df2) {
            df2 <- df2[, grepl("ID_t|pid|PV", names(df2))]
            dplyr::full_join(df1, df2) # merge data sets and store in pvs list
          }
        ))
    )
    if (is.null(bgdata)) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <- "ID_t"
    }
  }
  pvs
}
