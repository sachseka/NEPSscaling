## Link constants and correction terms for starting cohort 2
library(dplyr)

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- "SC2"

# Reading
link_constant[[SC]][["RE"]][["w9"]] <- 0.49

# Mathematics
link_constant[[SC]][["MA"]][["w3"]] <- 1.3522
link_constant[[SC]][["MA"]][["w4"]] <- 0.874# 2.226
link_constant[[SC]][["MA"]][["w6"]] <- 2.394# 4.620
# data <- haven::read_sav("../SUFs/SC2/SC2_xTargetCompetencies_D_9-0-0_beta.sav") # no TR
link_constant[[SC]][["MA"]][["w9"]] <- 1.225
    # as.numeric(data %>%
    #                filter(wave_w6 == 1 & wave_w9 == 1) %>%
    #                select(mag4_sc1u, mag7_sc1u) %>%
    #                mutate(const = mag7_sc1u - mag4_sc1u) %>%
    #                summarise_at(vars(const), mean, na.rm = TRUE))

# Information and Communication Technology literacy
# link_constant[[SC]][["IC"]][["w5"]] <- 0 # !

# Science
link_constant[[SC]][["SC"]][["w3"]] <- 1.445
link_constant[[SC]][["SC"]][["w5"]] <- 1.225

# Native Russian
# link_constant[[SC]][["NR"]][["w4"]] <- 0 # !

# Native Turkish
# link_constant[[SC]][["NT"]][["w4"]] <- 0 # !

# Vocabulary
link_constant[[SC]][["VO"]][["w3"]] <- 1.238
link_constant[[SC]][["VO"]][["w5"]] <- 1.045

# Grammar
link_constant[[SC]][["GR"]][["w3"]] <- 0 # !

save(link_constant, file = "data-raw/link_constant.RData")


# # Estimate item parameters for SC2 MA wave 9
# library(haven)
# library(dplyr)
# library(TAM)
# suf <- read_spss("../SUFs/SC2/SC2_xTargetCompetencies_D_9-0-0_beta.sav")
# dat <- suf %>%
#     arrange(ID_t) %>%
#     select("mag7q011_c", "mag7d061_sc2g7_c", "mag7r071_c",
#            "mag5v271_sc2g7_c", "mag4q011_sc2g7_c", "mag7r081_sc2g7_c",
#            "mag7v031_sc2g7_c", "mag7d06s_c", "mag5q301_sc2g7_c",
#            "mag7v021_c", "mag7r02s_sc2g7_c", "mag4q060_sc2g7_c",
#            "mag4d031_sc2g7_c", "mag9q181_sc2g7_c", "mag4v111_sc2g7_c",
#            "mag7q041_sc2g7_c", "mag7d042_sc2g7_c", "mag5r251_sc2g7_c",
#            "mag7d031_c", "mag5v321_sc2g7_c", "mag5r191_sc2g7_c",
#            "mag7q041_c", "mag9v091_sc2g7_c", "mag9d151_sc2g7_c",
#            "mag9v121_sc2g7_c", "mag7r091_sc2g7_c", "mag7q051_c",
#            "mag7v071_sc2g7_c", "mag7_sc1") %>% # not correct selection!
#     filter(!is.na(mag7_sc1)) %>%
#     select(-mag7_sc1)
# apply(dat, 2, table, useNA = "always")
# dat[["mag7d06s_c"]][dat[["mag7d06s_c"]] %in% c(1, 2, 3)] <- 0
# dat[["mag7d06s_c"]][dat[["mag7d06s_c"]] %in% c(4, 5)] <- 1
#
# dat[["mag7r02s_sc2g7_c"]][dat[["mag7r02s_sc2g7_c"]] %in% c(1, 2)] <- 0
# dat[["mag7r02s_sc2g7_c"]][dat[["mag7r02s_sc2g7_c"]] == 3] <- 1
# Q <- matrix(1, 28, 1)
# Q[c(8, 11), ] <- 0.5
# mod <- tam.mml(
#     resp = dat, irtmodel = "PCM2", verbose = FALSE, Q = Q
# )
# item_difficulty_SC2_MA_w9 <- mod$xsi.fixed.estimated[1:28, ]
# save(item_difficulty_SC2_MA_w9,
#      file = "data-raw/item_difficulty_SC2_MA_w9.RData")


# Estimate item parameters for SC2 NR/NT wave 4 (no TR)
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC2/SC2_xTargetCompetencies_D_8-0-1.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("nrg2")) %>%
    filter(!is.na(nrg2_sc1)) %>%
    select(-nrg2_sc1, -nrg2_sc2, -nrg2_sc3, -nrg2_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC2_NR_w4 <- mod$xsi.fixed.estimated
save(item_difficulty_SC2_NR_w4,
     file = "data-raw/item_difficulty_SC2_NR_w4.RData")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("ntg2")) %>%
    filter(!is.na(ntg2_sc1)) %>%
    select(-ntg2_sc1, -ntg2_sc2, -ntg2_sc3, -ntg2_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC2_NT_w4 <- mod$xsi.fixed.estimated
save(item_difficulty_SC2_NT_w4,
     file = "data-raw/item_difficulty_SC2_NT_w4.RData")


# Estimate item parameters for SC2 GR waves 1, 3 (no TR)
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC2/SC2_xTargetCompetencies_D_8-0-1.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("grk1")) %>%
    filter(!is.na(grk1_sc3)) %>%
    select(-grk1_sc3)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC2_GR_w1 <- mod$xsi.fixed.estimated
save(item_difficulty_SC2_GR_w1,
     file = "data-raw/item_difficulty_SC2_GR_w1.RData")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("grg1")) %>%
    filter(!is.na(grg1_sc1)) %>%
    select(-grg1_sc1, -grg1_sc2, -grg1_max)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC2_GR_w3 <- mod$xsi.fixed.estimated
# item_difficulty_SC2_GR_w3[, 2] <- item_difficulty_SC2_GR_w3[, 2] + 1.7 --> no more mean shift, but YES more correlation deviation!
save(item_difficulty_SC2_GR_w3,
     file = "data-raw/item_difficulty_SC2_GR_w3.RData")

