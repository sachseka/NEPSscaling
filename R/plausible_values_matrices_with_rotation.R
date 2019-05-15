## Design matrices etc. for PCM with correction for
## test position using TAM:
## Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM:
## Test analysis modules. R package version 2.9-35.
## https://CRAN.R-project.org/package=TAM

matrices_with_rotation <- function(resp, position, ind) {
  formulaA <- ~ 0 + item + item:step + position
  design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA,
                                     facets = position, constraint = 'cases')
  resp2 <- design$gresp$gresp.noStep
  A <- design$A$A.3d
  xsi.elim <- design$xsi.elim
  A <- A[, , -xsi.elim[, 2]]
  B <- design$B$B.3d
  B[ind, , ] <- 0.5 * B[ind, , ]

  res <- list(A = A, B = B, resp2 = resp2)

  return(res)
}
