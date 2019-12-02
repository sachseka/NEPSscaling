#' simulate background data for plausible values estimation
#' in NEPStools
rm(list = ls())

setwd("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_(p000011)/Methoden/Anna/02_Anleitung_Plausible_Values/R Code/")

set.seed(1234)
reading <- scale(rnorm(nrow(bg_data)) - 0.005 * bg_data$age + 0.017 * bg_data$gender + 0.03 * bg_data$math, scale = F)
beta <- rnorm(30)
rasch_prob <- function(theta, beta) {
  return(1 / (1 + exp(-(theta - beta))))
}

solution_prob <- matrix(NA, nrow = length(reading), ncol = length(beta))
for (i in 1:length(reading)) {
  for (j in 1:length(beta)) {
    solution_prob[i, j] <- rasch_prob(reading[i], beta[j])
  }
}

xTargetCompetencies_sim <- matrix(NA, nrow = length(reading), ncol = length(beta))
for (i in 1:length(reading)) {
  for (j in 1:length(beta)) {
    xTargetCompetencies_sim[i, j] <- rbinom(1, 1, solution_prob[i, j])
  }
}


xTargetCompetencies_sim <- as.data.frame(xTargetCompetencies_sim)
colnames(xTargetCompetencies_sim) <- c(
  "rea30110_c", "rea3012s_c", "rea30130_c", "rea30140_c", "rea3015s_c", "rea30210_c",
  "rea30220_c", "rea30230_c", "rea30240_c", "rea30250_c", "rea3028s_c", "rea30310_c",
  "rea30320_c", "rea30330_c", "rea30340_c", "rea30350_c", "rea30360_c", "rea30370_c",
  "rea3038s_c", "rea30410_c", "rea3042s_c", "rea30430_c", "rea30440_c", "rea30450_c",
  "rea30460_c", "rea30510_c", "rea3052s_c", "rea30530_c", "rea3054s_c", "rea30550_c"
)
wave_w3 <- rep(1, nrow(bg_data))
tx80211_w3 <- rep(0:1, nrow(bg_data) / 2)
xTargetCompetencies_sim <- data.frame(ID_t = bg_data$ID_t, wave_w3, tx80211_w3, xTargetCompetencies_sim)
xTargetCompetencies_sim$rea3_sc1 <- reading

xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3012s_c"] <- rep(2, 2800)
xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3015s_c"] <- rep(2, 2800)
xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3028s_c"] <- rep(2:5, 700)
xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3038s_c"] <- rep(2, 2800)
xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3042s_c"] <- rep(2, 2800)
xTargetCompetencies_sim[sample(nrow(xTargetCompetencies_sim), 2800), "rea3054s_c"] <- rep(2, 2800)


devtools::use_data(xTargetCompetencies_sim, pkg = "NEPStools", overwrite = TRUE)
