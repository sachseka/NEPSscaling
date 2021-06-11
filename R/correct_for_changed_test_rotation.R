#' in some starting cohorts, the test rotation (e.g. MA-RE and RE-MA) was changed
#' in later assessments (e.g. only MA-RE). In the longitudinal case, this change
#' needs to be corrected for the position effect due to fatigue etc.
#'
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#' @param position data.frame; contains the test rotation for each person
#' @param wle data.frame; estimated weighted maximum likelihood estimates
#' @param eap list of data.frames; estimated expected a posteriori values
#' @param pv list of data.frames; estimated plausible values
#'
#' @return list of eaps (list of data.frames), wle (data.frame), pvs (list of
#' data.frames) corrected for change in test positions
#' @noRd
correct_for_changed_test_rotation <- function(SC, domain, position, wle, eap, pv) {
  res <- set_correction_term(SC, domain, nrow(eap[[1]]), position)
  pos <- res[["pos"]]
  wave <- res[["wave"]]
  correction <- res[["correction"]]
  res <- apply_correction_for_changed_test_rotation(pos, wave, correction,
                                                    wle, eap, pv, position)
  res
}

#' correction term
#'
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#' @param position data.frame; contains the test rotation for each person
#' @param n_eap integer vector; number of test takers
#'
#' @return list of correction term (numeric), position indicator (pos) of test
#' takers with changed rotation, wave to be corrected in
#' @noRd
set_correction_term <- function(SC, domain, n_eap, position) {
  if (SC == "SC4") {
    # correct longitudinal values for change in rotation design
    # MA: add -0.060 to all participants who took math first (position == 1)
    # RE: add 0.164 to all participants who took reading second (position == 2)
    correction <- ifelse(domain == "MA", -0.060, 0.164)
    pos <- ifelse(domain == "MA", 1, 2)
    wave <- "w7"
  } else if (SC == "SC3") {
    # correct longitudinal values for change in rotation design
    # RE: add 0.174 to all participants who took reading second (position == 2)
    # NOTE: might not be necessary because this was done to calculate the link
    # constant!!! TODO
    correction <- 0.174
    pos <- 2
    wave <- "w6"
  } else if (SC == "SC2") {
    # correct longitudinal values for change in rotation design
    # VO: add 0.03 to all participants who took vocab test --> all children (wave 1)
    # VO: add 0.03 to all participants who took vocab test in (position == 2) (wave 3)
    if (domain == "VO") {
      correction <- matrix(0.03, n_eap, 2)
      correction[position[["position"]] == 1, 2] <- 0
      wave <- c("w1", "w3")
      pos <- c(1, 2)
    } else if (domain == "MA") {
      correction <- 0.0035
      # add half the position effect of grade 1 to persons who received
      # the test in second position in grade 1
      wave <- "w4"
      pos <- 2
    }
  }
  list(correction = correction, pos = pos, wave = wave)
}


#' in some starting cohorts, the test rotation (e.g. MA-RE and RE-MA) was changed
#' in later assessments (e.g. only MA-RE). In the longitudinal case, this change
#' needs to be corrected for the position effect due to fatigue etc.
#'
#' @param pos integer; position indicator for test takers with changed rotation
#' design
#' @param wave character; wave to be corrected in
#' @param correction numeric (matrix); contains correction term for test takers
#' with changed rotation design
#' @param wle data.frame; estimated weighted maximum likelihood estimates
#' @param eap list of data.frames; estimated expected a posteriori values
#' @param pv list of data.frames; estimated plausible values
#' @param position data.frame; contains the test rotation for each person
#'
#' @return list of eaps (list of data.frames), wle (data.frame), pvs (list of
#' data.frames) corrected for change in test positions
#' @noRd
apply_correction_for_changed_test_rotation <- function(pos, wave, correction,
                                                       wle, eap, pv, position) {
  if (!is.null(wle)) {
    wle[position[["ID_t"]] %in% wle[["ID_t"]] &
          position[["position"]] %in% pos, paste0("wle_", wave)] <-
      wle[position[["ID_t"]] %in% wle[["ID_t"]] &
            position[["position"]] %in% pos, paste0("wle_", wave)] + correction
  }
  eap <- lapply(eap, function(x) {
    x[position[["ID_t"]] %in% x[["ID_t"]] &
        position[["position"]] %in% pos, paste0("eap_", wave)] <-
      x[position[["ID_t"]] %in% x[["ID_t"]] &
          position[["position"]] %in% pos, paste0("eap_", wave)] + correction
    x
  })
  pv <- lapply(pv, function(x) {
    x[position[["ID_t"]] %in% x[["ID_t"]] &
              position[["position"]] %in% pos, paste0("PV_", wave)] <-
      x[position[["ID_t"]] %in% x[["ID_t"]] &
          position[["position"]] %in% pos, paste0("PV_", wave)] + correction
    x
  })
  list(wle = wle, eap = eap, pv = pv)
}
