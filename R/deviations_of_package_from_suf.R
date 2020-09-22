#' Get information about deviations of the NEPSscaling WLE mean estimation
#' (with an empty background model) from SUF WLE mean estimation. The
#' competencies still correlate to at least 0.95
#' \code{NEPScaling::plausible_values()}
#'
#' @export

deviations_of_package_from_suf <- function() {
cat(
"There are known deviations from the WLE mean estimations of the SUFs in
NEPSscaling (SUF - Package):
Starting Cohort 3:
- Orthography, wave 3 (longitudinal & cross-sectional): -0.100
- English as a foreign language, wave 7 (cross-sectional): 0.122
- English as a foreign language, wave 7 (longitudinal): 0.230
Starting Cohort 4:
- English as a foreign language, wave 9 (longitudinal): 0.216
"
)
}
