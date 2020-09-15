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
