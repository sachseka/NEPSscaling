## Link constants and correction terms for starting cohort 4
library(dplyr)

rm(list = ls())

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- "SC4"

# Reading
link_constant[[SC]][["RE"]][["w7"]] <- 0.045
link_constant[[SC]][["RE"]][["w7special"]] <- 0.25
link_constant[[SC]][["RE"]][["w10"]] <- 0.049406366423006

# Mathematics
link_constant[[SC]][["MA"]][["w7"]] <- 0.496
link_constant[[SC]][["MA"]][["w10"]] <- 0.3194437

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w7"]] <- 0.687
link_constant[[SC]][["IC"]][["w13"]] <- 0 # !

# Science
const <- haven::read_sav("../SUFs/SC4/SC4_xTargetCompetencies_D_10-0-0.sav") %>%
  filter(wave_w1 == 1 & wave_w5 == 1) %>%
  select(scg9_sc1u, scg11_sc1u) %>%
  mutate(const = scg11_sc1u - scg9_sc1u) %>%
  summarise_at(vars(const), mean, na.rm = TRUE)
link_constant[[SC]][["SC"]][["w5"]] <- const[[1]] # !Info not in TR, but in SUF
rm(const)
link_constant[[SC]][["SC"]][["w13"]] <- 0 # !

# Native Russian
# Native Turkish
# Scientific thinking

# English as a foreign language
link_constant[[SC]][["EF"]][["w7"]] <- 0 # ! not linked via mean/mean, but by re-using w3 parameter estimates


save(link_constant, file = "data-raw/link_constant.RData")

# # Corrections because of dropout etc.
# correction <- list()
# load(file = "data-raw/correction.RData")
# correction[[SC]][["RE"]] <- list()
# correction[[SC]][["RE"]][["w2"]] <- 0
# correction[[SC]][["RE"]][["w7"]] <- 0.487882
# correction[[SC]][["RE"]][["w10"]] <- 0 # !
#
# save(correction, file = "data-raw/correction.RData")

# Difference matrix for English w3, w7
diffMat <- list(SC4 = list())
load("data-raw/diffMat.RData")
diffMat$SC4 <- list(w3 = list(), w7 = list())

load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A48_G10/English/Version 2 (final)/out/data/dat.Rdata") # original data w3
dat <- dat[order(dat$ID_t), c("ID_t", items$poly$all)]
dat[dat < 0] <- 0
dat[, -1] <- lapply(dat[, -1], as.numeric)
load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A48_G10/English/Version 2 (final)/out/data/dati.Rdata") # imputed data w3
dati <- dati[order(dati$ID_t), c("ID_t", items$poly$all)]
dati[dati < 0] <- NA
diffMat$SC4$w3$ind_NA <- dati
diffMat$SC4$w3$ind_NA[, -1] <- is.na(diffMat$SC4$w3$ind_NA[, -1])
dati[is.na(dati)] <- 0
dati[, -1] <- lapply(dati[, -1], as.numeric)
dati[, -1] <- dati[, -1] - dat[, -1]
diffMat$SC4$w3$diff <- dati

load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A50_B41_G12/English/Version 3 (final)/out/data/dat.Rdata") # original data w7
dat <- dat[order(dat$ID_t), c("ID_t", items$poly_c)]
dat[dat < 0] <- 0
dat[, -1] <- lapply(dat[, -1], as.numeric)
load("//wipo.lifbi.de/daten/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A50_B41_G12/English/Version 3 (final)/out/data/dati.Rdata") # imputed data w7
dati <- dati[order(dati$ID_t), c("ID_t", items$poly_c)]
dati[dati < 0] <- NA
diffMat$SC4$w7$ind_NA <- dati
diffMat$SC4$w7$ind_NA[, -1] <- is.na(diffMat$SC4$w7$ind_NA[, -1])
dati[is.na(dati)] <- 0
dati[, -1] <- lapply(dati[, -1], as.numeric)
dati[, -1] <- dati[, -1] - dat[, -1]
diffMat$SC4$w7$diff <- dati

rm(dat, dati, items, position, correct)

save(diffMat, file = "data-raw/diffMat.RData")


# item difficulties sc4 ma w7
library(haven)
library(dplyr)
library(TAM)
dat <- read_spss("../SUFs/SC4/SC4_xTargetCompetencies_D_10-0-0.sav") %>%
    arrange(ID_t) %>%
    select(NEPSscaling:::item_labels$SC4$MA$w7, mag12_sc1, tx80211_w7) %>%
    filter(!is.na(mag12_sc1)) %>%
    select(-mag12_sc1)
position <- dat %>% mutate(tx80211_w7 = ifelse(tx80211_w7 %in% c(289, 290, 291, 292, 293, 294, rep(NA, 6)), 1,
                                               ifelse(is.na(tx80211_w7), NA, 2))) %>%
    rename(position = tx80211_w7) %>% select(position)
testletSetting <- dat %>% mutate(difficultTestlet = ifelse(tx80211_w7 %in% c(285, 288, 291:293, 296:303), T,
                                                           ifelse(is.na(tx80211_w7), NA, F)),
                                 atHome = ifelse(tx80211_w7 %in% c(281:295), T,
                                                 ifelse(is.na(tx80211_w7), NA, F))) %>%
    select(difficultTestlet, atHome)
dat <- dat %>% select(-tx80211_w7)
apply(dat, 2, table, useNA = "always")
dat$mas1q02s_sc4g12_c <- recode(as.numeric(dat$mas1q02s_sc4g12_c),
                                `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                                .default = NA_real_)
B <- TAM::designMatrices(modeltype = "PCM", resp = dat)$B
B[apply(dat, 2, max, na.rm = TRUE) > 1, , ] <-
    0.5 * B[apply(dat, 2, max, na.rm = TRUE) > 1, , ]
mod <- tam.mml.mfr(resp = dat[!is.na(position$position), ], irtmodel = "PCM2",
                   verbose = FALSE, B = B,
                   formulaA = ~ 0 + item + item:step + position,
                   facets = position[!is.na(position$position), ])
xsi1 <- mod$xsi.fixed.estimated[1:31, ]

resp <- dat[!is.na(position$position), ]
testletSetting <- testletSetting[!is.na(position$position), ]
resp <- tibble::add_column(resp, mag9d201_sc4g12_c_g = ifelse(!testletSetting[["atHome"]], resp[["mag9d201_sc4g12_c"]],
                                                              NA), .after = "mag9d201_sc4g12_c")
resp <- tibble::add_column(resp, mag9d201_sc4g12_c_i = ifelse(testletSetting[["atHome"]], resp[["mag9d201_sc4g12_c"]],
                                                              NA), .after = "mag9d201_sc4g12_c_g")
resp[["mag9d201_sc4g12_c"]] <- NULL
resp <- tibble::add_column(resp, mag9r051_sc4g12_c_d = ifelse(testletSetting[["difficultTestlet"]], resp[["mag9r051_sc4g12_c"]],
                                                              NA), .after = "mag9r051_sc4g12_c")
resp <- tibble::add_column(resp, mag9r051_sc4g12_c_e = ifelse(!testletSetting[["difficultTestlet"]], resp[["mag9r051_sc4g12_c"]],
                                                              NA), .after = "mag9r051_sc4g12_c_d")
resp[["mag9r051_sc4g12_c"]] <- NULL

B2 <- TAM::designMatrices(modeltype = "PCM", resp = resp)$B
B2[apply(resp, 2, max, na.rm = TRUE) > 1, , ] <-
    0.5 * B2[apply(resp, 2, max, na.rm = TRUE) > 1, , ]
mod4 <- tam.mml.mfr(resp = resp,
                    irtmodel = "PCM2",
                    verbose = FALSE, B = B2,
                    formulaA = ~ 0 + item + item:step + position,
                    facets = position[!is.na(position$position), ])
xsi4 <- mod4$xsi.fixed.estimated[1:31, ]

library(tidyverse)
xsi <- NEPSscaling:::xsi.fixed$cross$MA$SC4$w7 %>%
    as.data.frame() %>%
    rownames_to_column('rn') %>%
    full_join(x = ., y = xsi4 %>%
                  as.data.frame() %>%
                  rownames_to_column('rn'), by = "rn") %>%
    column_to_rownames("rn")

item_difficulty_SC4_MA_w7 <- as.matrix(xsi[, c(1, 4)])
save(item_difficulty_SC4_MA_w7, file = "data-raw/item_difficulty_SC4_MA_w7.RData")


# SC 4 Reading w10 - LONGITUDINAL
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC4/SC4_xTargetCompetencies_D_10-0-0.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select(NEPSscaling:::item_labels$SC4$RE$w10, "rea10_sc1u") %>% # not correct selection!
    filter(!is.na(rea10_sc1u)) %>%
    select(-rea10_sc1u)
apply(dat, 2, table, useNA = "always")
dat <- NEPSscaling:::collapse_categories_pcm(dat, "SC4", "w10", "RE")$resp
Q <- as.matrix(ifelse(apply(dat, 2, max, na.rm = T) > 1, 0.5, 1))
mod <- tam.mml(
    resp = dat, irtmodel = "PCM2", verbose = FALSE, Q = Q
)
item_difficulty_SC4_RE_w10 <- mod$xsi.fixed.estimated[1:36, ]
save(item_difficulty_SC4_RE_w10,
     file = "data-raw/item_difficulty_SC4_RE_w10_long.RData")
