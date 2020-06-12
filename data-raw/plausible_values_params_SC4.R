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
