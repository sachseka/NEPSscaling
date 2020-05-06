## Link constants and correction terms for starting cohort 3
rm(list = ls())
library(dplyr)

link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- "SC3"

# Reading
link_constant[[SC]][["RE"]][["w3"]] <- 0.667# 0.1371353
link_constant[[SC]][["RE"]][["w6"]] <- 0.579
link_constant[[SC]][["RE"]][["w9"]] <- 0.4989

# Mathematics
link_constant[[SC]][["MA"]][["w3"]] <- 0.726# 0.771
link_constant[[SC]][["MA"]][["w5"]] <- 0.794
data <- haven::read_sav("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav") # no TR
link_constant[[SC]][["MA"]][["w9"]] <- as.numeric(data %>%
    filter(wave_w9 == 1 & wave_w5 == 1) %>%
    select(mag9_sc1u, mag12_sc1u) %>%
    mutate(const = mag12_sc1u - mag9_sc1u) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w5"]] <- 1.042
link_constant[[SC]][["IC"]][["w9"]] <- as.numeric(data %>%
    filter(wave_w9 == 1 & wave_w5 == 1) %>%
    select(icg9_sc1u, icg12_sc1u) %>%
    mutate(const = icg12_sc1u - icg9_sc1u) %>%
    summarise_at(vars(const), mean, na.rm = TRUE)) # !

# Science
link_constant[[SC]][["SC"]][["w2"]] <- 0 # !
link_constant[[SC]][["SC"]][["w5"]] <- 0
link_constant[[SC]][["SC"]][["w8"]] <- 0 # !

# Native Russian
link_constant[[SC]][["NR"]][["w6"]] <- 0 # !

# Native Turkish
link_constant[[SC]][["NT"]][["w6"]] <- 0 # !

# English as a foreign language
link_constant[[SC]][["EF"]][["w7"]] <- -0.201 # to SC4 class 10
link_constant[[SC]][["EF"]][["w9"]] <- 0 # via fixed item parameters

# Vocabulary
# link_constant[[SC]][["VO"]][["w2"]] <- 0 # !

# Orthography -- does not seem to be linked in the SUFs!
data <- haven::read_sav("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav")
link_constant[[SC]][["ORA"]][["w3"]] <- as.numeric(data %>%
    filter(wave_w1 == 1 & wave_w3 == 1) %>%
    select(org5_sc1a, org7_sc1a) %>%
    mutate(const = org7_sc1a - org5_sc1a) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))
link_constant[[SC]][["ORB"]][["w3"]] <- as.numeric(data %>%
    filter(wave_w1 == 1 & wave_w3 == 1) %>%
    select(org5_sc1b, org7_sc1b) %>%
    mutate(const = org7_sc1b - org5_sc1b) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))
link_constant[[SC]][["ORA"]][["w5"]] <- as.numeric(data %>%
    filter(wave_w5 == 1 & wave_w3 == 1) %>%
    select(org9_sc1a, org7_sc1a) %>%
    mutate(const = org9_sc1a - org7_sc1a) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))
link_constant[[SC]][["ORB"]][["w5"]] <- as.numeric(data %>%
    filter(wave_w5 == 1 & wave_w3 == 1) %>%
    select(org9_sc1b, org7_sc1b) %>%
    mutate(const = org9_sc1b - org7_sc1b) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))
rm(data)

save(link_constant, file = "data-raw/link_constant.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w7"]] <- 0 # !
correction[[SC]][["RE"]][["w10"]] <- 0 # !

save(correction, file = "data-raw/correction.RData")


# Difference matrix for English w7, w9
load("data-raw/diffMat.RData")
diffMat$SC3 <- list(w7 = list(), w9 = list())

load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A99_G10/English/Version 5 (final)/out/data/dat.Rdata") # original data w7
dat <- dat[order(dat$ID_t), c("ID_t", items$poly$all)]
dat[dat < 0] <- 0
dat[, -1] <- lapply(dat[, -1], as.numeric)
load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A99_G10/English/Version 5 (final)/out/data/dati.Rdata") # imputed data w7
dati <- dati[order(dati$ID_t), c("ID_t", items$poly$all)]
dati[dati < 0] <- NA
diffMat$SC3$w7$ind_NA <- dati
diffMat$SC3$w7$ind_NA[, -1] <- is.na(diffMat$SC3$w7$ind_NA[, -1])
dati[is.na(dati)] <- 0
dati[, -1] <- lapply(dati[, -1], as.numeric)
dati[, -1] <- dati[, -1] - dat[, -1]
diffMat$SC3$w7$diff <- dati

load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A101_B108_SC3_K12/EF/Version 1/data/dat.Rdata") # original data w9
dat <- dat[order(dat$ID_t), c("ID_t", items$poly_c)]
dat[dat < 0] <- 0
dat[, -1] <- lapply(dat[, -1], as.numeric)
load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A101_B108_SC3_K12/EF/Version 1/data/dati.Rdata") # imputed data w9
dati <- dati[order(dati$ID_t), c("ID_t", items$poly_c)]
dati[dati < 0] <- NA
diffMat$SC3$w9$ind_NA <- dati
diffMat$SC3$w9$ind_NA[, -1] <- is.na(diffMat$SC3$w9$ind_NA[, -1])
dati[is.na(dati)] <- 0
dati[, -1] <- lapply(dati[, -1], as.numeric)
dati[, -1] <- dati[, -1] - dat[, -1]
diffMat$SC3$w9$diff <- dati

rm(dat, dati, items, position, correct)

save(diffMat, file = "data-raw/diffMat.RData")

# Estimate item parameters for SC3 Science wave 5 (substitute until technical
# report is published)
library(haven)
library(dplyr)
library(TAM)
dat <- read_spss("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav") %>%
    arrange(ID_t) %>%
    select(contains("scg9")) %>%
    filter(!is.na(scg9_sc1)) %>%
    select(-scg9_sc1, -scg9_sc2, -scg9_sc1u, -scg9_sc2u)
apply(dat, 2, table, useNA = "always")
dat$scg9042s_sc3g9_c <- recode(as.numeric(dat$scg9042s_sc3g9_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2,
                               .default = NA_real_)
dat$scg9083s_sc3g9_c <- recode(as.numeric(dat$scg9083s_sc3g9_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                               .default = NA_real_)
dat$scg9611s_sc3g9_c <- recode(as.numeric(dat$scg9611s_sc3g9_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2,
                               .default = NA_real_)
dat$scg9012s_sc3g9_c <- recode(as.numeric(dat$scg9012s_sc3g9_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2,
                               .default = NA_real_)
mod <- tam.mml(
    resp = dat, irtmodel = "PCM2", verbose = FALSE,
    Q = as.matrix(ifelse(apply(dat, 2, max, na.rm = TRUE) > 1, 0.5, 1))
)
item_difficulty_SC3_SC_w5 <- mod$xsi.fixed.estimated[1:37, ]
save(item_difficulty_SC3_SC_w5,
     file = "data-raw/item_difficulty_SC3_SC_w5.RData")

# Estimate item parameters for SC3 NR/NT waves 3 and 6 (no TR) and calculate
# link constant from data in the SUF
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("nrg7")) %>%
    filter(!is.na(nrg7_sc1)) %>%
    select(-nrg7_sc1, -nrg7_sc2, -nrg7_sc3, -nrg7_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC3_NR_w3 <- mod$xsi.fixed.estimated
save(item_difficulty_SC3_NR_w3,
     file = "data-raw/item_difficulty_SC3_NR_w3.RData")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("ntg7")) %>%
    filter(!is.na(ntg7_sc1)) %>%
    select(-ntg7_sc1, -ntg7_sc2, -ntg7_sc3, -ntg7_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC3_NT_w3 <- mod$xsi.fixed.estimated
save(item_difficulty_SC3_NT_w3,
     file = "data-raw/item_difficulty_SC3_NT_w3.RData")

dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("nrg9")) %>%
    filter(!is.na(nrg9_sc3g9_sc1)) %>%
    select(-nrg9_sc3g9_sc1, -nrg9_sc3g9_sc2, -nrg9_sc3g9_sc3, -nrg9_sc3g9_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC3_NR_w6 <- mod$xsi.fixed.estimated
save(item_difficulty_SC3_NR_w6,
     file = "data-raw/item_difficulty_SC3_NR_w6.RData")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("ntg9")) %>%
    filter(!is.na(ntg9_sc3g9_sc1)) %>%
    select(-ntg9_sc3g9_sc1, -ntg9_sc3g9_sc2, -ntg9_sc3g9_sc3, -ntg9_sc3g9_sc7)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC3_NT_w6 <- mod$xsi.fixed.estimated
save(item_difficulty_SC3_NT_w6,
     file = "data-raw/item_difficulty_SC3_NT_w6.RData")
