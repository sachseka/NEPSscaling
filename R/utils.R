#' disable print/cat side effects: https://stackoverflow.com/a/54136863
#' @param x ...
#' @noRd
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

is_PCM <- function(longitudinal, resp) {
  if (longitudinal) {
    return(lapply(resp,
                  function(x) {max(apply(x[, -1], 2, max, na.rm = TRUE)) > 1}))
  }
  max(apply(resp[, -1], 2, max, na.rm = TRUE)) > 1
}

was_assessed_in_school <- function(longitudinal, SC, wave) {
  if (longitudinal) {
    # some starting cohorts are never assessed in school context, thus, they
    # can be excluded as a whole in the longitudinal case
    if (SC %in% c("SC1", "SC5", "SC6")) {
      return(FALSE)
    }
  } else {
    # some starting cohorts transition into or out of school, thus, specific
    # waves have to be checked in the cross-sectional case
    if (!(SC == "SC2" & wave %in% c("w3", "w4", "w5", "w6", "w9")) &&
        !(SC == "SC3" & wave %in% c("w1", "w2", "w3", "w5", "w6", "w7",
                                    "w8", "w9")) &&
        !(SC == "SC4" & wave %in% c("w1", "w2", "w3", "w5", "w7"))) {
      return(FALSE)
    }
  }
  TRUE
}

extract_bgdata_variables <- function(bgdatacom, exclude_for_wave, waves, j) {
  if (gsub("_", "", waves[j]) %in% names(exclude_for_wave)) {
    exclude <- names(bgdatacom) %in% exclude_for_wave[[gsub("_", "", waves[j])]]
    if (any(exclude)) {
      return(bgdatacom[, !exclude])
    }
  }
  bgdatacom
}
