#' correct linking term because of deviations from SUF WLE means
#'
#' @param term ...
#' @param SC ...
#' @param domain ...
#' @param wave ...
#' @param w ...
#'
#' @noRd

correct_linking_term <- function(term, SC, domain, wave, w) {
  # estimated WLE means around 0 (as befitting the model),
  # estimated correlations with SUF >> 0.95
  if (SC == "SC2" & domain == "MA" & wave[w] == "w4") {
    # means bias the wle estimate wrt the suf estimate, link constants alone
    # give correct result
    term <- -(link_constant[[SC]][[domain]][[wave[w] ]] +
            link_constant[[SC]][[domain]][[wave[w - 1] ]])
  } else if (SC == "SC3" & domain == "IC" & wave[w] == "w5") {
    # means bias the wle estimate wrt the suf estimate
    term <- term + 0.2
  } else if (SC == "SC3" & domain == "SC" & wave[w] == "w8") {
    # means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "MA" & wave[w] == "w7") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "SC" & wave[w] == "w5") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "RE" & wave[w] == "w10") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  }
  term
}

# #' correct deviations in WLE means from SUF WLE means in cross-sectional and
# #' fixed parameter linked estimation
# #'
# #' @param pv ...
# #' @param wle ...
# #' @param eap ...
# #' @param SC ...
# #' @param domain ...
# #' @param type ...
# #'
# #' @noRd
#
# correct_mean_deviations <- function(pv, wle, eap, SC, domain, type) {
#   # estimated correlations with SUF >> 0.95
#   if (SC == "SC3" & domain == "EF" & type == "cross") {
#     term <- 0.1
#     wave <- ""
#   } else if (SC == "SC3" & domain == "EF" & type == "long") {
#     term <- 0.2
#     wave <- "_w9"
#   } else if (SC == "SC3" & domain == "ORB" & type == "cross") {
#     term <- -0.1
#     wave <- ""
#   } else if (SC == "SC3" & domain == "ORB" & type == "long") {
#     term <- -0.1
#     wave <- "_w3"
#   } else if (SC == "SC4" & domain == "EF" & type == "long") {
#     term <- 0.2
#     wave <- "_w7"
#   }
#
#   eap[, paste0("eap", wave)] <- eap[, paste0("eap", wave)] + term
#   for (i in seq(length(pv))) {
#     pv[[i]][, paste0("PV", wave)] <-
#       pv[[i]][, paste0("PV", wave)] + term
#   }
#   if (!is.null(wle)) {
#     wle[, paste0("wle", wave)] <- wle[, paste0("wle", wave)] + term
#   }
#   list(pv = pv, wle = wle, eap = eap)
# }
