#' Get information about known deviations of the NEPSscaling WLE mean
#' estimation from SUF WLE mean estimation. The competence estimators of SUF
#' and package correlate to at least 0.95
#'
#' @export

deviations_of_package_from_suf <- function() {
cat(
"There are known deviations from the WLE mean estimations of the SUFs in
NEPSscaling (SUF - Package):
Starting Cohort 2:
- Grammar, wave 3 (cross-sectional): 1.701
Starting Cohort 3:
- Mathematics, wave 3 (cross-sectional): 0.722
- Reading, wave 3 (cross-sectional): 0.671
- ICT, wave 5 (cross-sectional): 0.724
- Orthography, wave 3 (longitudinal & cross-sectional): -0.100
- English as a foreign language, wave 7 (cross-sectional): 0.122
- English as a foreign language, wave 7 (longitudinal): 0.230
Starting Cohort 4:
- ICT, wave 7 (cross-sectional): 0.634
- English as a foreign language, wave 9 (longitudinal): 0.216
"
)
}
