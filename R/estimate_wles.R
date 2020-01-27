#' estimate WLEs
#'
#' @param longitudinal ...
#' @param waves ...
#' @param mod ...
#'
#' @noRd

estimate_wles <- function(longitudinal, waves, mod) {
    . <- NULL
  if (longitudinal) {
    wmod <- list()
    WLE.rel <- vector("numeric", length(waves))
    for (j in seq(length(waves))) {
      wmod[[j]] <- as.data.frame(TAM::tam.mml.wle2(mod[[j]],
        WLE = TRUE,
        progress = FALSE
      ))
      WLE.rel[j] <- wmod[[j]]$WLE.rel[1]
    }
    wmod <- wmod %>%
      Reduce(function(df1, df2) {
        dplyr::full_join(df1, df2, by = "pid")
      }, .)
  } else {
    wmod <-
      as.data.frame(TAM::tam.mml.wle2(mod[[1]], WLE = TRUE, progress = FALSE))
    WLE.rel <- wmod$WLE.rel[1]
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
  list(wle = wle, WLE.rel = WLE.rel)
}
