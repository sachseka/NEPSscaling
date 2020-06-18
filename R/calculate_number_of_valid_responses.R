#' calculate number of valid responses per person
#'
#' @param longitudinal ...
#' @param resp ...
#' @param waves ...
#'
#' @return data.frame
#'
#' @noRd

calculate_number_of_valid_responses <- function(longitudinal, resp, waves) {
  if (longitudinal) {
    n.valid <-
      data.frame(ID_t = unique(
        unlist(lapply(resp, function(x) {
          x[["ID_t"]]
        }))
      ))
    n.valid <- n.valid[order(n.valid[["ID_t"]]), , drop = FALSE]
    for (w in seq(length(waves))) {
      tmp <-
        data.frame(
          ID_t = resp[[w]][["ID_t"]],
          rowSums(!is.na(resp[[w]][, -1]))
        )
      n.valid <- suppressWarnings(dplyr::left_join(n.valid, tmp, by = "ID_t"))
      names(n.valid)[w + 1] <- paste0("valid", waves[w])
    }
    rm(tmp)
  } else {
    n.valid <- data.frame(ID_t = resp[["ID_t"]])
    n.valid[["valid"]] <- rowSums(!is.na(resp[, -1]))
  }
  n.valid
}
