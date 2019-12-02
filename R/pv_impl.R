#' Get information about implementation progress of
#' \code{NEPScaling::plausible_values()}
#'
#' @export

pv_impl <- function() {
  cat(
    "Plausible_values() is implemented for the following \nstarting cohorts, domains and waves; \nall competence scores are available for \ncross-sectional and longitudinal research\n(if multiple waves are implemented).\n
== SC1 ==\n--\n
== SC2 ==\n--\n
== SC3 ==\n--\n
== SC4 ==\n--\n
== SC5 ==\nReading: waves 1 and 12\nMath: waves 1 and 12\nICT: wave 5\nScience: wave 5\nBusiness Administration: wave 7\nEnglish as a foreign language: wave 12\n
== SC6 ==\nReading: waves 3/5 and 9\nMath: waves 3 and 9\nICT: wave 5\nScience: wave 5\n"
  )
}
