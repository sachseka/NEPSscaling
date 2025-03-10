#' Loading patterns for longitudinal item response models (ML)
#'
#' @param SC String representation of starting cohort
#' @param domain String representation of current competency domain
#' @param items list of item names per wave
#'
#' @return list of loading matrices Q (one for each assessment wave, with as
#' many rows as there are items in the model)
#' @noRd
create_loading_matrix_q_longitudinal <- function(SC, domain, items) {
  Q <- list()
  for (l in seq(length(items))) {
    # if items were split due to DIF
    len <- if (SC == "SC4" && domain ==  "MA" && l == 2) {
      31
    } else if (SC == "SC2" && domain ==  "RE" && l == 2) {
      45
    }# else if (SC == "SC4" && domain ==  "SC" && l == 2) {
    #  41
    #} else if (SC == "SC6" && domain ==  "SC" && l == 2) {
    #  41
    #}
  else {
      length(items[[l]])
    }
    Q[[l]] <- matrix(
      data = 1,
      nrow = len,
      ncol = 1
    )
  }
  Q
}
