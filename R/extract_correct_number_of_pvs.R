#' extract correct number of plausible values from pvs object
#'
#' @param bgdata data.frame or NULL; background data
#' @param control list; contains number of multiple imputations
#' @param npv numeric; number of plausible values
#' @param pvs list of data.frames; contains completed bgdata and pvs
#'
#' @return list of npv data.frames named after the nested combination of which
#' imputation and which PV
#' @noRd

extract_correct_number_of_pvs <- function(bgdata, control, npv, pvs) {
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
        dplyr::select(.data$ID_t, dplyr::everything()) %>%
        dplyr::arrange(.data$ID_t)
      names(datalist)[d] <- paste0("imp", i, "pv", j)
      d <- d + 1
    }
  }
  ind <- sample(1:length(datalist), npv)
  datalist <- datalist[ind]
  datalist
}
