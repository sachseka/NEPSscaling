#' Loading patterns for longitudinal item response models (ML)
#'
#' @param SC String representation of starting cohort
#' @param domain String representation of current competency domain
#'
#' @noRd

create_loading_matrix_q_longitudinal <- function(SC, domain) {
  Q <- list()
  for (l in seq(length(item_labels[[SC]][[domain]]))) {
    len <- if (SC == "SC4" && domain ==  "MA" && l == 2) {
      31
    } else if (SC == "SC2" && domain ==  "RE" && l == 2) {
      45
    } else {
      length(item_labels[[SC]][[domain]][[l]])
    }
    Q[[l]] <- matrix(
      data = 1,
      nrow = len,
      ncol = 1
    )
  }
  Q
}
