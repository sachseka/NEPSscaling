#' estimate WLEs
#'
#' @param longitudinal
#' @param waves
#' @param mod
#'
#' @noRd

estimate_wles <- function(longitudinal, waves, mod) {
  if (longitudinal) {
    wmod <- list()
    for (j in seq(length(waves))) {
      wmod[[j]] <- TAM::tam.mml.wle2(mod[[j]],
        WLE = TRUE,
        progress = FALSE
      )
    }
    wmod <- wmod %>%
      Reduce(function(df1, df2) {
        dplyr::full_join(df1, df2, by = "pid")
      }, .)
  } else {
    wmod <- TAM::tam.mml.wle2(mod[[1]], WLE = TRUE, progress = FALSE)
  }
  wle <- wmod[grep("pid|theta|error", colnames(wmod))]
  colnames(wle) <-
    c("ID_t", paste0(
      rep(c("wle", "se"), length(waves)),
      rep(if (longitudinal) {
        waves
      } else {
        ""
      },
      each = 2
      )
    ))
  wle
}
