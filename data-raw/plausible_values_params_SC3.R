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
link_constant[[SC]][["SC"]][["w5"]] <- as.numeric(data %>%
    filter(wave_w2 == 1 & wave_w5 == 1) %>%
    select(scg6_sc1, scg9_sc1u) %>%
    mutate(const = scg9_sc1u - scg6_sc1) %>%
    summarise_at(vars(const), mean, na.rm = TRUE)) # !
link_constant[[SC]][["SC"]][["w8"]] <- as.numeric(data %>%
    filter(wave_w8 == 1 & wave_w5 == 1) %>%
    select(scg9_sc1u, scg11_sc1) %>%
    mutate(const = scg11_sc1 - scg9_sc1u) %>%
    summarise_at(vars(const), mean, na.rm = TRUE)) # !

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
    select(NEPSscaling:::item_labels$SC3$SC$w5, scg9_sc1, tx80211_w5) %>%
    filter(!is.na(scg9_sc1)) %>%
    select(-scg9_sc1)
# position <- dat %>% mutate(tx80211_w5 = ifelse(tx80211_w5 %in% 566:592, 1,
#                                                ifelse(is.na(tx80211_w5), NA, 2))) %>%
#     rename(position = tx80211_w5) %>% select(position)
dat <- dat %>% select(-tx80211_w5)
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
dat[["scg9043s_sc3g9_c"]][dat[["scg9043s_sc3g9_c"]] == 3] <- 2
# B <- TAM::designMatrices(modeltype = "PCM", resp = dat)$B
# B[apply(dat, 2, max, na.rm = TRUE) > 1, , ] <-
#     0.5 * B[apply(dat, 2, max, na.rm = TRUE) > 1, , ]
# mod <- tam.mml.mfr(resp = dat[!is.na(position$position), ], irtmodel = "PCM2",
#                    verbose = FALSE, B = B,
#                    formulaA = ~ 0 + item + item:step + position,
#                    facets = position[!is.na(position$position), ])
# item_difficulty_SC3_SC_w5 <- mod$xsi.fixed.estimated[1:37, ]
# tmp <- dplyr::left_join(data.frame(item = NEPSscaling:::item_labels$SC3$SC$w5),
#                         item_difficulty_SC3_SC_w5 %>%
#                             as.data.frame() %>%
#                             mutate(item = rownames(item_difficulty_SC3_SC_w5)),
#                         by = "item")
# item_difficulty_SC3_SC_w5 <- cbind(1:nrow(tmp),
#                                    xsi = tmp$xsi)
# rownames(item_difficulty_SC3_SC_w5) <- tmp$item
# rm(tmp)
# save(item_difficulty_SC3_SC_w5,
#      file = "data-raw/item_difficulty_SC3_SC_w5.RData")
mod <- tam.mml(
    resp = dat, irtmodel = "PCM2", verbose = FALSE,
    Q = as.matrix(ifelse(apply(dat, 2, max, na.rm = TRUE) > 1, 0.5, 1))
)
item_difficulty_SC3_SC_w5_long <- mod$xsi.fixed.estimated[1:37, ]
save(item_difficulty_SC3_SC_w5_long,
     file = "data-raw/item_difficulty_SC3_SC_w5_long.RData")


# Estimate item parameters for SC3 Science wave 8 (substitute until technical
# report is published)
library(haven)
library(dplyr)
library(TAM)
dat <- read_spss("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav") %>%
    arrange(ID_t) %>%
    select("scg116420_sc3g11_c", "scg110620_sc3g11_c", "scg110630_sc3g11_c",
           "scg11012s_sc3g11_c", "scg11083s_sc3g11_c", "scg110720_sc3g11_c",
           "scg11032s_sc3g11_c", "scg110330_sc3g11_c", "scg116510_sc3g11_c",
           "scg11652s_sc3g11_c", "scs56320_sc3g11_c", "scg110510_sc3g11_c",
           "scg110520_sc3g11_c", "scg110540_sc3g11_c", "scg11123s_sc3g11_c",
           "scg11102s_sc3g11_c", "scg11021s_sc3g11_c", "scg11022s_sc3g11_c",
           "scg11112s_sc3g11_c", "scg116210_sc3g11_c", "scg11622s_sc3g11_c",
           "scg116320_sc3g11_c", "scg110930_sc3g11_c", "scs5131s_sc3g11_c",
           "scs5132s_sc3g11_c", scg11_sc1) %>%
    filter(!is.na(scg11_sc1)) %>%
    select(-scg11_sc1)
apply(dat, 2, table, useNA = "always")
dat$scg11083s_sc3g11_c <- recode(as.numeric(dat$scg11083s_sc3g11_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2,
                               .default = NA_real_)
dat$scg11123s_sc3g11_c <- recode(as.numeric(dat$scg11123s_sc3g11_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                               .default = NA_real_)
dat$scs5132s_sc3g11_c <- recode(as.numeric(dat$scs5132s_sc3g11_c),
                               `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                               .default = NA_real_)
mod <- tam.mml(
    resp = dat, irtmodel = "PCM2", verbose = FALSE,
    Q = as.matrix(ifelse(apply(dat, 2, max, na.rm = TRUE) > 1, 0.5, 1))
)
item_difficulty_SC3_SC_w8 <- mod$xsi.fixed.estimated[1:25, ]
save(item_difficulty_SC3_SC_w8,
     file = "data-raw/item_difficulty_SC3_SC_w8.RData")


# Estimate item parameters for SC3 math wave 9 (substitute until technical
# report is published)
library(haven)
library(dplyr)
library(TAM)
dat <- read_spss("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav") %>%
    arrange(ID_t) %>%
    select("maa3q071_sc3g12_c", "mag12v101_sc3g12_c", "mag12q121_sc3g12_c",
           "mag12v122_sc3g12_c", "mag12r011_sc3g12_c", "mag12v061_sc3g12_c",
           "mag12r091_sc3g12_c", "mag9r051_sc3g12_c", "mag12q081_sc3g12_c",
           "mag12d021_sc3g12_c", "mag12q051_sc3g12_c", "mag9d201_sc3g12_c",
           "mag9v121_sc3g12_c", "mas1q021s_sc3g12_c", "mas1d081_sc3g12_c",
           "maa3d112_sc3g12_c", "mag9r061_sc3g12_c", "maa3r011_sc3g12_c",
           "mag12d071_sc3g12_c", "mag12r041_sc3g12_c", "mag12v131_sc3g12_c",
           "mag12d031_sc3g12_c", "maa3d131_sc3g12_c", "maa3d132_sc3g12_c",
           "mag9v011_sc3g12_c", "maa3r121_sc3g12_c", "mag12q111_sc3g12_c",
           "maa3q101_sc3g12_c", "mag9q101_sc3g12_c", "mag12v132_sc3g12_c",
           "mag12_sc1") %>%
    filter(!is.na(mag12_sc1)) %>%
    select(-mag12_sc1)
apply(dat, 2, table, useNA = "always")
dat$mas1q021s_sc3g12_c <- recode(as.numeric(dat$mas1q021s_sc3g12_c),
                         `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                         .default = NA_real_)
mod <- tam.mml(
    resp = dat, irtmodel = "PCM2", verbose = FALSE,
    Q = as.matrix(ifelse(apply(dat, 2, max, na.rm = TRUE) > 1, 0.5, 1))
)
item_difficulty_SC3_MA_w9 <- mod$xsi.fixed.estimated[1:30, ]
save(item_difficulty_SC3_MA_w9,
     file = "data-raw/item_difficulty_SC3_MA_w9.RData")

# Estimate item parameters for SC3 ict wave 9 (substitute until technical
# report is published)
library(haven)
library(dplyr)
library(TAM)
dat <- read_spss("../SUFs/SC3/SC3_xTargetCompetencies_D_9-0-0.sav") %>%
    arrange(ID_t) %>%
    select("icg12018s_sc3g12_c", "ica4003x_sc3g12_c", "icg12107s_sc3g12_c",
           "icg12004s_sc3g12_c", "icg12010x_sc3g12_c", "icg12011x_sc3g12_c",
           "ica4008x_sc3g12_c", "icg12060s_sc3g12_c", "icg12013s_sc3g12_c",
           "ica4018s_sc3g12_c", "icg12016s_sc3g12_c", "ica4019x_sc3g12_c",
           "icg12121x_sc3g12_c", "icg12028s_sc3g12_c", "ica4023x_sc3g12_c",
           "ica4027x_sc3g12_c", "icg12033x_sc3g12_c", "icg12034x_sc3g12_c",
           "icg12035x_sc3g12_c", "icg12040x_sc3g12_c", "icg12037s_sc3g12_c",
           "icg12138s_sc3g12_c", "icg12047s_sc3g12_c", "icg12041x_sc3g12_c",
           "icg12046s_sc3g12_c", "ica4021s_sc3g12_c", "ica4052s_sc3g12_c",
           "icg12048s_sc3g12_c", "icg12050s_sc3g12_c", "icg12054s_sc3g12_c",
           "icg12109s_sc3g12_c", "icg12119s_sc3g12_c", icg12_sc1, tx80211_w9) %>%
    filter(!is.na(icg12_sc1)) %>%
    select(-icg12_sc1)
position <- dat %>% mutate(tx80211_w9 = ifelse(tx80211_w9 %in% 623:626, 1,
                                               ifelse(is.na(tx80211_w9), NA, 2))) %>%
    rename(position = tx80211_w9) %>% select(position)
dat <- dat %>% select(-tx80211_w9)
apply(dat, 2, table, useNA = "always")
dat$icg12119s_sc3g12_c <- recode(as.numeric(dat$icg12119s_sc3g12_c),
                         `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2, `5` = 3,
                         .default = NA_real_)
dat$icg12109s_sc3g12_c <- recode(as.numeric(dat$icg12109s_sc3g12_c),
                         `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                         .default = NA_real_)
dat$icg12054s_sc3g12_c <- recode(as.numeric(dat$icg12054s_sc3g12_c),
                         `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3,
                         .default = NA_real_)
dat$icg12050s_sc3g12_c <- recode(as.numeric(dat$icg12050s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 0,
                                 `5` = 1, `6` = 2,
                                 .default = NA_real_)
dat$icg12048s_sc3g12_c <- recode(as.numeric(dat$icg12048s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                 `5` = 3,
                                 .default = NA_real_)
dat$ica4052s_sc3g12_c <- recode(as.numeric(dat$ica4052s_sc3g12_c),
                                `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                `5` = 3,
                                .default = NA_real_)
dat$ica4021s_sc3g12_c <- recode(as.numeric(dat$ica4021s_sc3g12_c),
                                `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 0,
                                `5` = 1,
                                .default = NA_real_)
dat$icg12046s_sc3g12_c <- recode(as.numeric(dat$icg12046s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                 `5` = 3, `6` = 4,
                                 .default = NA_real_)
dat$icg12047s_sc3g12_c <- recode(as.numeric(dat$icg12047s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 0,
                                 `5` = 0, `6` = 0, `7` = 1, `8` = 2, `9` = 3,
                                 `10` = 4,
                                 .default = NA_real_)
dat$icg12138s_sc3g12_c <- recode(as.numeric(dat$icg12138s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                 .default = NA_real_)
dat$icg12028s_sc3g12_c <- recode(as.numeric(dat$icg12028s_sc3g12_c),
                                `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 1,
                                `5` = 2,
                                .default = NA_real_)
dat$icg12016s_sc3g12_c <- recode(as.numeric(dat$icg12016s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                 .default = NA_real_)
dat$ica4018s_sc3g12_c <- recode(as.numeric(dat$ica4018s_sc3g12_c),
                                 `0` = 0, `1` = 1, `2` = 2, `3` = 2, `4` = 2,
                                 `5` = 2, `6` = 3, `7` = 3, `8` = 4,
                                 .default = NA_real_)
dat$icg12013s_sc3g12_c <- recode(as.numeric(dat$icg12013s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 1, `3` = 1,
                                 .default = NA_real_)
dat$icg12060s_sc3g12_c <- recode(as.numeric(dat$icg12060s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 1,
                                 .default = NA_real_)
dat$icg12004s_sc3g12_c <- recode(as.numeric(dat$icg12004s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 1, `4` = 2,
                                 `5` = 3, `6` = 4,
                                 .default = NA_real_)
dat$icg12107s_sc3g12_c <- recode(as.numeric(dat$icg12107s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 1,
                                 `5` = 2,
                                 .default = NA_real_)
dat$icg12018s_sc3g12_c <- recode(as.numeric(dat$icg12018s_sc3g12_c),
                                 `0` = 0, `1` = 0, `2` = 0, `3` = 0, `4` = 1,
                                 .default = NA_real_)
B <- TAM::designMatrices(
    modeltype = "PCM",
    resp = dat
)$B
B[apply(dat, 2, max, na.rm = TRUE) > 1, , ] <-
    0.5 * B[apply(dat, 2, max, na.rm = TRUE) > 1, , ]
mod <- tam.mml.mfr(resp = dat[!is.na(position$position), ], irtmodel = "PCM2",
                   verbose = FALSE, B = B,
                   formulaA = ~ 0 + item + item:step + position,
                   facets = position[!is.na(position$position), ])
item_difficulty_SC3_IC_w9 <- mod$xsi.fixed.estimated[1:32, ]
tmp <- dplyr::left_join(data.frame(item = NEPSscaling:::item_labels$SC3$IC$w9),
                        item_difficulty_SC3_IC_w9 %>%
                            as.data.frame() %>%
                            mutate(item = rownames(item_difficulty_SC3_IC_w9)),
                        by = "item")
item_difficulty_SC3_IC_w9 <- cbind(1:nrow(tmp),
                                   xsi = tmp$xsi)
rownames(item_difficulty_SC3_IC_w9) <- tmp$item
rm(tmp)
save(item_difficulty_SC3_IC_w9,
     file = "data-raw/item_difficulty_SC3_IC_w9.RData")
mod <- tam.mml(resp = dat, irtmodel = "PCM2", verbose = FALSE,
               Q = as.matrix(ifelse(apply(dat, 2, max, na.rm = TRUE) > 1, 0.5, 1)))
item_difficulty_SC3_IC_w9_long <- mod$xsi.fixed.estimated[1:32, ]
save(item_difficulty_SC3_IC_w9_long,
     file = "data-raw/item_difficulty_SC3_IC_w9_long.RData")

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
