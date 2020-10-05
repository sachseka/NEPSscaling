#' simulate background data for plausible values estimation
#' in NEPScaling
rm(list = ls())
xTargetCompetencies <- haven::read_spss("../SUFs/SC6/SC6_xTargetCompetencies_D_9-0-0.sav")

bg_data <- xTargetCompetencies[!is.na(xTargetCompetencies$rea3_sc1), c("ID_t", "rea3_sc1")]

bg_data$age <- 0
bg_data$gender <- 0
set.seed(1234)
for (i in 1:nrow(bg_data)) {
  if (!is.na(bg_data$rea3_sc1[i]) & (bg_data$rea3_sc1[i] < mean(bg_data$rea3_sc1, na.rm = T))) {
    bg_data$age[i] <- round(rnorm(1, mean = 63.57333, sd = 4.5), digits = 0)
  } else if (!is.na(bg_data$rea3_sc1[i]) & (bg_data$rea3_sc1[i] > mean(bg_data$rea3_sc1, na.rm = T))) {
    bg_data$age[i] <- round(rnorm(1, mean = 31.78667, sd = 4.5), digits = 0)
  }
}
for (i in 1:nrow(bg_data)) {
  if (!is.na(bg_data$rea3_sc1[i]) & (bg_data$rea3_sc1[i] < mean(bg_data$rea3_sc1, na.rm = T))) {
    bg_data$gender[i] <- rbinom(1, 1, 0.326)
  } else if (!is.na(bg_data$rea3_sc1[i]) & (bg_data$rea3_sc1[i] > mean(bg_data$rea3_sc1, na.rm = T))) {
    bg_data$gender[i] <- rbinom(1, 1, 0.652)
  }
}
bg_data$math <- scale((bg_data$rea3_sc1 * 0.02 + bg_data$gender * 0.04 - bg_data$age * 0.03 + rnorm(nrow(bg_data))), scale = F)

psych::describe(bg_data)

bg_data <- bg_data[, -2]
bg_data$math <- c(bg_data$math)

usethis::use_data(bg_data, overwrite = TRUE)
