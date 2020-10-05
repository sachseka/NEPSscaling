#' correct linking term because of deviations from SUF WLE means
#'
#' @param term ...
#' @param SC ...
#' @param domain ...
#' @param waves_ ...
#' @param w ...
#'
#' @noRd

correct_linking_term <- function(term, SC, domain, waves_, w) {
  # estimated WLE means around 0 (as befitting the model),
  # estimated correlations with SUF >> 0.95
  if (SC == "SC2" & domain == "MA" & waves_[w] == "w4") {
    # means bias the wle estimate wrt the suf estimate, link constants alone
    # give correct result
    term <- -(link_constant[[SC]][[domain]][[waves_[w] ]] +
            link_constant[[SC]][[domain]][[waves_[w - 1] ]])
  } else if (SC == "SC2" & domain == "SC" & waves_[w] == "w3") {
    term <- term + 0.3
  } else if (SC == "SC3" & domain == "IC" & waves_[w] == "w5") {
    # means bias the wle estimate wrt the suf estimate
    term <- term + 0.2
  } else if (SC == "SC3" & domain == "SC" & waves_[w] == "w8") {
    # means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC3" & domain == "RE" & waves_[w] == "w6") {
    # means bias the wle estimate wrt the suf estimate
    term <- term + 0.1
  } else if (SC == "SC3" & domain == "RE" & waves_[w] == "w9") {
    # means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "MA" & waves_[w] == "w7") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "SC" & waves_[w] == "w5") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "RE" & waves_[w] == "w10") {
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
#     waves_ <- ""
#   } else if (SC == "SC3" & domain == "EF" & type == "long") {
#     term <- 0.2
#     waves_ <- "_w9"
#   } else if (SC == "SC3" & domain == "ORB" & type == "cross") {
#     term <- -0.1
#     waves_ <- ""
#   } else if (SC == "SC3" & domain == "ORB" & type == "long") {
#     term <- -0.1
#     waves_ <- "_w3"
#   } else if (SC == "SC4" & domain == "EF" & type == "long") {
#     term <- 0.2
#     waves_ <- "_w7"
#   }
#
#   eap[, paste0("eap", waves_)] <- eap[, paste0("eap", waves_)] + term
#   for (i in seq(length(pv))) {
#     pv[[i]][, paste0("PV", waves_)] <-
#       pv[[i]][, paste0("PV", waves_)] + term
#   }
#   if (!is.null(wle)) {
#     wle[, paste0("wle", waves_)] <- wle[, paste0("wle", waves_)] + term
#   }
#   list(pv = pv, wle = wle, eap = eap)
# }
