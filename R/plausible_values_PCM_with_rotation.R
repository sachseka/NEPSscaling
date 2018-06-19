## Estimation of Partial Credit Model with correction for test
## rotation using TAM:
## Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM: Test analysis
## modules. R package version 2.9-35.
## https://CRAN.R-project.org/package=TAM
PCM_with_rotation <- function(xsi, resp, Y, position, ind){
  res <- matrices_with_rotation(resp, position, ind)
  A <- res$A
  B <- res$B
  resp <- res$resp2
  xsi <- xsi[rownames(xsi) %in% dimnames(A)[[3]], ]
  mod <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , Y = Y
                      , xsi.fixed = xsi
                      , A = A
                      , B = B
                      , verbose = FALSE
  )
  return(mod)
}
