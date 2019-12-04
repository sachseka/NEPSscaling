#' Loading patterns for longitudinal item response models (ML)
#'
#' @param SC String representation of starting cohort
#' @param domain String representation of current competency domain
#'
#' @noRd

create_loading_matrix_q_longitudinal <- function(SC, domain) {
  Q <- list()
  for (l in seq(length(item_labels[[SC]][[domain]]))) {
    Q[[l]] <- matrix(
      data = 1,
      nrow = length(item_labels[[SC]][[domain]][[l]]),
      ncol = 1
    )
  }
  Q
}
