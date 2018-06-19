## Compute log posterior of threshold parameters
## adopted from LaRA:
## Jean-Christoph Gaasch (2017). LaRA: Latent Regression
## Analysis. R package version 0.1.1.

lposttau <- function(
  Tau,
  Yj,
  Qj,
  alpha,
  beta,
  Theta
){

  Kappa <- c(-1e+05, 0, cumsum(exp(Tau)), 1e+05)
  lprior <- dmvnorm(Tau, rep(0, Qj), 100*diag(Qj), TRUE)
  llik <- sum(log(pnorm(alpha*Theta - (beta + Kappa[Yj + 1])) -
    pnorm(alpha*Theta - (beta + Kappa[Yj + 2]))))
  lpost <- lprior + llik
  return(-lpost)

}
