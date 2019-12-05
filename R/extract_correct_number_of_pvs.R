#' extract correct number of plausible values from pvs object
#'
#' @param bgdata
#' @param control
#' @param npv
#' @param pvs
#'
#' @noRd

extract_correct_number_of_pvs <- function(bgdata, control, npv, pvs) {
  ID_t <- NULL
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
  ind <- sample(1:length(datalist), npv)
  datalist <- datalist[ind]
  datalist
}
