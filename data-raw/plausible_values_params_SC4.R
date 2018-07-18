## Estimation of item difficulties of SC4 using TAM:
## Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM: Test
## analysis modules. R package version 2.9-35.
## https://CRAN.R-project.org/package=TAM
##
## The item parameters are corrected for test design changes
## and (if possible) linked to a common scale.


# TODO:
# - scaling
# - rotation designs?
# - linking?
# - EF fehlt, längsschnittliche Erhebungen fehlen

rm(list = ls())

path <- '../../SUFs/SC4/'
nvalid <- 3

files <- list.files(path = path)
data <- haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))
rm(files)
names(data)[grep('wave', names(data))]


item_labels <- list(SC4 = list(RE = list(w2 = c("reg90110_c", "reg90120_c", "reg90150_c", "reg9016s_c",
                                                "reg9017s_c", "reg90210_c", "reg90220_c", "reg90230_c",
                                                "reg90240_c", "reg90250_c", "reg90310_c", "reg90320_c",
                                                "reg9033s_c", "reg90340_c", "reg90350_c", "reg90360_c",
                                                "reg90370_c", "reg90410_c", "reg90420_c", "reg90430_c",
                                                "reg90440_c", "reg90450_c", "reg90460_c", "reg9047s_c",
                                                "reg90510_c", "reg90520_c", "reg90530_c", "reg90540_c",
                                                "reg90550_c", "reg90560_c", "reg90570_c"),
                                         w7 = c("reg120110_c", "reg120120_c", "reg120130_c", "reg12014s_c", "reg120150_c",
                                                "reg120160_c", "reg120170_c", "reg12021s_c", "reg120220_c", "reg120230_c",
                                                "reg12024s_c", "reg120250_c", "reg12026s_c", "reg120310_c", "reg120320_c",
                                                "reg120330_c", "reg120340_c", "reg120350_c", "reg120360_c", #"reg12041s_c",
                                                "reg12042s_c", "reg120430_c", "reg12044s_c", "reg120450_c", "reg120510_c",
                                                "reg12052s_c", "reg120530_c", "reg120540_c", "reg12055s_c", "reg120560_c",
                                                "reg120610_c", "reg120620_c", "reg120630_c", "reg120640_c", "reg12065s_c",
                                                "reg120660_c", "reg120670_c", "reg12071s_c", "reg120720_c", "reg120730_c",
                                                "reg120740_c", "reg12075s_c"),
                                         w10 = c()),
                               MA = list(w1 = c("mag9q071_c", "mag9v131_c", "mag9v13s_c", "mag9r261_c",
                                                "mag9r111_c", "mag9d171_c", "mag9d151_c", "mag9r051_c",
                                                "mag9v011_c", "mag9v012_c", "mag9q161_c", "mag9d201_c",
                                                "mag9r191_c", "mag9v121_c", "mag9q181_c", "mag9r25s_c",
                                                "mag9r061_c", "mag9q081_c", "mag9q101_c", "mag9q021_c",
                                                "mag9v091_c", "mag9q211_c"),
                                         w7 = c("maa3q071_sc4g12_c", "mag12v101_c", "mag12q121_c", "mag12v122_c",
                                                "maa3d131_sc4g12_c", "maa3d132_sc4g12_c", "mag12r011_c", "mag12v061_c",
                                                "mag12r091_c", "mag9r051_sc4g12_c", "mag9v011_sc4g12_c", "mag12q081_c",
                                                "mag12d021_c", "mag12q051_c", "mag9d201_sc4g12_c", "mag9v121_sc4g12_c",
                                                "maa3r121_sc4g12_c", "mag12q111_c", "mas1q02s_sc4g12_c", "mas1d081_sc4g12_c",
                                                "maa3d112_sc4g12_c", "mag9r061_sc4g12_c", "maa3q101_sc4g12_c",
                                                "mag9q101_sc4g12_c", "maa3r011_sc4g12_c", "mag12d071_c", "mag12r041_c",
                                                "mag12v131_c", "mag12v132_c", "mag12d031_c"),
                                         w10 = c()),
                               IC = list(w1 = c("icg9101x_c", "icg9102s_c", "icg9103x_c", "icg9104x_c",
                                                "icg9105x_c", "icg9106x_c", "icg9107s_c", "icg9109x_c",
                                                "icg9110x_c", "icg9111x_c", "icg9112x_c", "icg9113x_c",
                                                "icg9114x_c", "icg9116x_c", "icg9117s_c", "icg9118x_c",
                                                "icg9119x_c", "icg9121x_c", "icg9122x_c", "icg9123x_c",
                                                "icg9124x_c", "icg9125s_c", "icg9126x_c", "icg9127x_c",
                                                "icg9128x_c", "icg9129x_c", "icg9130x_c", "icg9131x_c",
                                                "icg9132x_c", "icg9133s_c", "icg9134x_c", "icg9135x_c",
                                                "icg9136s_c", "icg9137x_c", "icg9138x_c", "icg9140s_c"),
                                         w7 = c("icg12018s_c", "ica5003x_c",  "icg12107s_c", "icg12004s_c",
                                                "icg12010x_c", "icg12011x_c", "ica5008x_c", "icg12060s_c",
                                                "icg12013s_c", "ica5018s_c",  "icg12016s_c", "ica5019x_c",
                                                "icg12121x_c", "icg12028s_c", "ica5023x_c", "ica5027x_c",
                                                "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
                                                "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
                                                "icg12046s_c", "ica5021s_c", "ica5052s_c", "icg12048s_c",
                                                "icg12050s_c", "icg12054s_c", "icg12109s_c", "icg12119s_c")),
                               SC = list(w1 = c("scg90110_c", "scg9012s_c", "scg90510_c",
                                                "scg9052s_c", "scg90920_c", "scg90930_c",
                                                "scg9611s_c", "scg96120_c", "scg96410_c",
                                                "scg96420_c", "scg9061s_c", "scg90630_c",
                                                "scg90810_c", "scg9083s_c", "scg91030_c",
                                                "scg91040_c", "scg91050_c", "scg9042s_c",
                                                "scg9043s_c", "scg9651s_c", "scg96530_c",
                                                "scg90320_c", "scg90330_c", "scg9621s_c",
                                                "scg96220_c", "scg91110_c", "scg91120_c",
                                                "scg91130_c"),
                                         w5 = c("scg116420_c", "scg110620_c", "scg110630_c", "scg11012s_c",
                                                "scg11083s_c", "scg110720_c", "scg11032s_c", "scg110330_c",
                                                "scg116510_c", "scg11652s_c", "scg11602s_c", "scg110510_c",
                                                "scg110520_c", "scg110540_c", "scg11123s_c", "scg11102s_c",
                                                "scg11021s_c", "scg11022s_c", "scg11112s_c", "scg116210_c",
                                                "scg11622s_c", "scg116320_c", "scg110930_c",
                                                "scs5131s_sc4g11_c")),
                               NR = list(w2 = c("nrg90101_c", "nrg90102_c", "nrg90103_c", "nrg90201_c",
                                                "nrg90202_c", "nrg90203_c", "nrg90301_c", "nrg90302_c",
                                                "nrg90303_c", "nrg90304_c", "nrg90401_c", "nrg90402_c",
                                                "nrg90403_c", "nrg90404_c", "nrg90405_c", "nrg90502_c",
                                                "nrg90503_c", "nrg90504_c", "nrg90505_c", "nrg90506_c",
                                                "nrg90601_c", "nrg90602_c", "nrg90603_c", "nrg90604_c",
                                                "nrg90605_c", "nrg90701_c", "nrg90702_c", "nrg90703_c",
                                                "nrg90704_c", "nrg90705_c", "nrg90706_c")),
                               NT = list(w2 = c("ntg90101_c", "ntg90102_c", "ntg90103_c", "ntg90201_c",
                                                "ntg90202_c", "ntg90203_c", "ntg90301_c", "ntg90302_c",
                                                "ntg90303_c", "ntg90304_c", "ntg90401_c", "ntg90402_c",
                                                "ntg90403_c", "ntg90404_c", "ntg90405_c", "ntg90502_c",
                                                "ntg90503_c", "ntg90504_c", "ntg90505_c", "ntg90506_c",
                                                "ntg90601_c", "ntg90602_c", "ntg90603_c", "ntg90604_c",
                                                "ntg90605_c", "ntg90701_c", "ntg90702_c", "ntg90703_c",
                                                "ntg90704_c", "ntg90705_c", "ntg90706_c")),
                               EF = list(w3 = c("efg10022s_c", "efg10108s_c", "efg10094s_c", "efg10059s_c",
                                                "efg10002s_c", "efg10008s_c", "efg10098s_c", "efg10065a_c",
                                                "efg10065b_c", #"efg10065c_c",
                                                "efg10065d_c", "efg10075s_c",
                                                "efg10057a_c"),
                                         w7 = c("efg10022s_sc4g12_c", "efg12b00s_c", "efg10108s_sc4g12_c",
                                                "efg12d001_c", "efg12d002_c", "efg12d003_c", "efg12d004_c",
                                                "efg12d005_c")),
                               ST = list(w7 = c("stg12nh01_c", "stg12nh02_c", "stg12nh03_c",
                                                "stg12nh04_c", "stg12nh05_c", "stg12eg01_c",
                                                "stg12eg02_c", "stg12eg03_c", "stg12eg04_c",
                                                "stg12eg05_c", "stg12eg06_c", "stg12eg07_c",
                                                "stg12mt01_c", "stg12mt02_c", "stg12mt03_c",
                                                "stg12mt04_c", "stg12mt05_c", "stg12cmt06_c",
                                                "stg12cw01_c", "stg12cw02_c", "stg12cw03_c",
                                                "stg12cw04_c", "stg12cw05_c", "stg12cw06_c",
                                                "stg12cw07_c", "stg12pd01_c", "stg12pd02_c",
                                                "stg12cpd03_c", "stg12pd04_c", "stg12pd05_c",
                                                "stg12pd06_c", "stg12pd07_c"))))

# select test takers
data <- data[order(data$ID_t), ]

# output object
eaps <- list()
load(file = 'data-raw/eaps.RData')
meanvar <- list()
load(file = "data-raw/meanvar.RData")

data <- haven::read_sav("Z:/Projektgruppen_(08)/Kompetenzen_BA_(p000011)/Methoden/Anna/02_Anleitung_Plausible_Values/SUFs/SC4/SC4_xTargetCompetencies_D_9-1-0.sav")

# starting cohort
SC <- 'SC4'

# Reading
meanvar[[SC]][["RE"]][["w2"]][["cross"]] <- c(mean(data$reg9_sc1, na.rm = TRUE), 1.373)
meanvar[[SC]][["RE"]][["w2"]][["long"]] <- c(mean(data$reg9_sc1u, na.rm = TRUE), 1.373)
meanvar[[SC]][["RE"]][["w7"]][["cross"]] <- c(mean(data$reg12_sc1, na.rm = TRUE), 0.829)
meanvar[[SC]][["RE"]][["w7"]][["long"]] <- c(mean(data$reg12_sc1u, na.rm = TRUE), 0.829)

# Mathematics

# Information and Communication Technology literacy

# Science

# Native Russian

# Native Turkish

# Enlish as a foreign language

# Scientific thinking
save(meanvar, file = "data-raw/meanvar.RData")



# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------

# Reading
domain <- 'RE'
# wave 2
wave <- 'w2'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w2 == 1, ]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp
                    , irtmodel = 'PCM2'
                    , B = B
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(mod, read, resp, Q, B, ind, wave)


# wave 7
wave <- 'w7'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w7 == 1, ]
# collapse items
read$reg12014s_c[read$reg12014s_c == 2] <- 1
read$reg12021s_c[read$reg12021s_c == 1] <- 0
read$reg12021s_c[read$reg12021s_c == 2] <- 1
read$reg12024s_c[read$reg12024s_c == 1] <- 0
read$reg12024s_c[read$reg12024s_c == 2] <- 1
read$reg12024s_c[read$reg12024s_c == 3] <- 2
read$reg12024s_c[read$reg12024s_c == 4] <- 3
read$reg12026s_c[read$reg12026s_c == 1] <- 0
read$reg12026s_c[read$reg12026s_c == 2] <- 1
read$reg12026s_c[read$reg12026s_c == 3] <- 2
read$reg12026s_c[read$reg12026s_c == 4] <- 3
read$reg12026s_c[read$reg12026s_c == 5] <- 4
read$reg12026s_c[read$reg12026s_c == 6] <- 5
read$reg12042s_c[read$reg12042s_c == 1] <- 0
read$reg12042s_c[read$reg12042s_c == 2] <- 1
read$reg12055s_c[read$reg12055s_c == 1] <- 0
read$reg12055s_c[read$reg12055s_c == 2] <- 1
read$reg12055s_c[read$reg12055s_c == 3] <- 2
read$reg12071s_c[read$reg12071s_c == 1] <- 0
read$reg12071s_c[read$reg12071s_c == 2] <- 0
read$reg12071s_c[read$reg12071s_c == 3] <- 1
read$reg12075s_c[read$reg12075s_c == 1] <- 0
read$reg12075s_c[read$reg12075s_c == 2] <- 1
read$reg12075s_c[read$reg12075s_c == 3] <- 2
# rotation variable
position <- data.frame(ID_t = read$ID_t, position = rep(NA, nrow(read)))
position[position$ID_t %in% read$ID_t, 'position'] <- read[, 'tx80211_w7']
position[!is.na(position$position) & (position$position %in% c(283:288,300:303)), 'position'] <- 0 # reading first
position[!is.na(position$position) & (position$position %in% c(281,282,289:292,296:299)), 'position'] <- 1 # math/ict first
# 281	B41: ICT-L1	B41
# 282	B41: ICT-L2	B41
# 283	B41: L1-ICT	B41
# 284	B41: L1-M1	B41
# 285	B41: L1-M2	B41
# 286	B41: L2-ICT	B41
# 287	B41: L2-M1	B41
# 288	B41: L2-M2	B41
# 289	B41: M1-L1	B41
# 290	B41: M1-L2	B41
# 291	B41: M2-L1	B41
# 292	B41: M2-L2	B41
# 293	B41: M2	B41
# 294	B41: M1	B41
# 295	B41: ICT	B41
# 296	A50: ICTA1-LA2a-MA3-STD4-EFLD5	A50
# 297	A50: ICTA1-LA2b-MA3-STD4-EFLD5	A50
# 298	A50: ICTA1-LA2a-MA3-EFLC4-STC5	A50
# 299	A50: ICTA1-LA2b-MA3-EFLC4-STC5	A50
# 300	A50: LB1a-ICTB2-MB3-STD4-EFLD5	A50
# 301	A50: LB1b-ICTB2-MB3-STD4-EFLD5	A50
# 302	A50: LB1a-ICTB2-MB3-EFLC4-STC5	A50
# 303	A50: LB1b-ICTB2-MB3-EFLC4-STC5	A50
position <- position[, 2, drop = FALSE]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , Q = Q
                      , verbose = FALSE
)
# linking
c <- 0.04462366
mod$xsi.fixed.estimated[, 2] <- mod$xsi.fixed.estimated[, 2] + c
mod_l$xsi.fixed.estimated[, 2] <- mod_l$xsi.fixed.estimated[, 2] + c
# # # correction for design change: add half of position main effect to WLEs of thos receiving it second
# # if (position == 1) wle=wle + (0.089 * 2)
# # * Note: There was no rotation design in G9, but a rotation design in G12.
# # *       To correct for the position of the test in G12, we correct the WLEs in G12 for respondents receiving
# # *       the test second using the main effect identified in G12 (3.2 show.txt).
# # *       But keep the mean change across the entire sample unchanged.
# #
# #
# # *COMPUTE one = 1.
# # *AGGREGATE /OUTFILE=* MODE=ADDVARIABLES /BREAK=one /mn1=MEAN(regc_sc1u).
# # IF(regcorder = 1) regc_sc1u = regc_sc1u + (0.089 * 2).
# # *AGGREGATE /OUTFILE=* MODE=ADDVARIABLES /BREAK=one /mn2=MEAN(regc_sc1u).
# # *COMPUTE regc_sc1u = regc_sc1u + (mn1 - mn2).
# # EXECUTE.
# # correction for mean differences long./full sample due to dropout (add to long. WLEs)
# ct <- 0.487882
# add item difficulties/eaps to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- TAM::tam.mml(resp=resp2,irtmodel = "PCM2",A=A,B=B,xsi.fixed = mod$xsi.fixed.estimated,verbose=FALSE)$person
eaps[[SC]][[domain]][[wave]][["long"]] <- TAM::tam.mml(resp=resp,irtmodel = "PCM2",Q=Q,xsi.fixed = mod_l$xsi.fixed.estimated,verbose=FALSE)$person
# clear environment
rm(design, mod, position, read, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l,c)




# wave 10
wave <- 'w10'
# collapse items
# test data
# CROSS-SECTIONAL
# rotation variable
# estimation model
# LONGITUDINAL
# estimation model
# linking
# add item difficulties/eaps to out object
# clear environment




# -------------------------------------------------------------------------------------------------------------

# Math
domain <- 'MA'
# wave 1
wave <- 'w1'
math <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
math <- math[math$wave_w1 == 1, ]
# test data
resp <- math[, names(math) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp
                    , irtmodel = 'PCM2'
                    , B = B
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(mod, math, resp, Q, B, ind, wave)

# wave 7
wave <- 'w7'
math <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
math <- math[math$wave_w7 == 1, ]
# collapse items
math$mas1q02s_sc4g12_c[math$mas1q02s_sc4g12_c == 1] <- 0
math$mas1q02s_sc4g12_c[math$mas1q02s_sc4g12_c == 2] <- 1
math$mas1q02s_sc4g12_c[math$mas1q02s_sc4g12_c == 3] <- 2
math$mas1q02s_sc4g12_c[math$mas1q02s_sc4g12_c == 4] <- 3
# rotation variable
position <- data.frame(ID_t = math$ID_t, position = rep(NA, nrow(math)))
position[position$ID_t %in% math$ID_t, 'position'] <- math[, 'tx80211_w7']
position[!is.na(position$position) & (position$position %in% c(289:294)), 'position'] <- 0 # math first
position[!is.na(position$position) & (position$position %in% c(284,285,287,288,296:303)), 'position'] <- 1 # reading first
# 281	B41: ICT-L1	B41
# 282	B41: ICT-L2	B41
# 283	B41: L1-ICT	B41
# 284	B41: L1-M1	B41
# 285	B41: L1-M2	B41
# 286	B41: L2-ICT	B41
# 287	B41: L2-M1	B41
# 288	B41: L2-M2	B41
# 289	B41: M1-L1	B41
# 290	B41: M1-L2	B41
# 291	B41: M2-L1	B41
# 292	B41: M2-L2	B41
# 293	B41: M2	B41
# 294	B41: M1	B41
# 295	B41: ICT	B41
# 296	A50: ICTA1-LA2a-MA3-STD4-EFLD5	A50
# 297	A50: ICTA1-LA2b-MA3-STD4-EFLD5	A50
# 298	A50: ICTA1-LA2a-MA3-EFLC4-STC5	A50
# 299	A50: ICTA1-LA2b-MA3-EFLC4-STC5	A50
# 300	A50: LB1a-ICTB2-MB3-STD4-EFLD5	A50
# 301	A50: LB1b-ICTB2-MB3-STD4-EFLD5	A50
# 302	A50: LB1a-ICTB2-MB3-EFLC4-STC5	A50
# 303	A50: LB1b-ICTB2-MB3-EFLC4-STC5	A50
position <- position[, 2, drop = FALSE]
# test data
resp <- math[, names(math) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , Q = Q
                      , verbose = FALSE
)
# linking
c <- 0.496
mod$xsi.fixed.estimated[, 2] <- mod$xsi.fixed.estimated[, 2] + c
mod_l$xsi.fixed.estimated[, 2] <- mod_l$xsi.fixed.estimated[, 2] + c
# correction for design changes: add half of position main effect to WLEs of those receiving it first
# add item difficulties/eaps to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- TAM::tam.mml(resp=resp2,irtmodel = "PCM2",A=A,B=B,xsi.fixed = mod$xsi.fixed.estimated,verbose=FALSE)$person
eaps[[SC]][[domain]][[wave]][["long"]] <- TAM::tam.mml(resp=resp,irtmodel = "PCM2",Q=Q,xsi.fixed = mod_l$xsi.fixed.estimated,verbose=FALSE)$person
# clear environment
rm(design, mod, position, math, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l,c)



# wave 10
wave <- 'w10'
# collapse items
# test data
# CROSS-SECTIONAL
# rotation variable
# estimation model
# LONGITUDINAL
# estimation model
# linking
# add item difficulties/eaps to out object
# clear environment




# -------------------------------------------------------------------------------------------------------------

# ICT
domain <- 'IC'
# wave 1
wave <- 'w1'
ict <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ict <- ict[ict$wave_w1 == 1, ]
ict$icg9102s_c[ict$icg9102s_c == 1] <- 0
ict$icg9102s_c[ict$icg9102s_c == 2] <- 1
ict$icg9102s_c[ict$icg9102s_c == 3] <- 2
ict$icg9102s_c[ict$icg9102s_c == 4] <- 3
ict$icg9107s_c[ict$icg9107s_c == 1] <- 0
ict$icg9107s_c[ict$icg9107s_c == 2] <- 1
ict$icg9107s_c[ict$icg9107s_c == 3] <- 2
ict$icg9107s_c[ict$icg9107s_c == 4] <- 3
ict$icg9107s_c[ict$icg9107s_c == 5] <- 4
ict$icg9125s_c[ict$icg9125s_c == 1] <- 0
ict$icg9125s_c[ict$icg9125s_c == 2] <- 1
ict$icg9125s_c[ict$icg9125s_c == 3] <- 2
ict$icg9125s_c[ict$icg9125s_c == 4] <- 3
ict$icg9117s_c[ict$icg9117s_c == 1] <- 0
ict$icg9117s_c[ict$icg9117s_c == 2] <- 1
ict$icg9117s_c[ict$icg9117s_c == 3] <- 2
ict$icg9117s_c[ict$icg9117s_c == 4] <- 3
ict$icg9117s_c[ict$icg9117s_c == 5] <- 4
ict$icg9117s_c[ict$icg9117s_c == 6] <- 5
ict$icg9133s_c[ict$icg9133s_c == 1] <- 0
ict$icg9133s_c[ict$icg9133s_c == 2] <- 1
ict$icg9133s_c[ict$icg9133s_c == 3] <- 2
ict$icg9133s_c[ict$icg9133s_c == 4] <- 3
ict$icg9133s_c[ict$icg9133s_c == 5] <- 4
ict$icg9133s_c[ict$icg9133s_c == 6] <- 5
ict$icg9136s_c[ict$icg9136s_c == 1] <- 0
ict$icg9136s_c[ict$icg9136s_c == 2] <- 1
ict$icg9136s_c[ict$icg9136s_c == 3] <- 2
ict$icg9136s_c[ict$icg9136s_c == 4] <- 3
ict$icg9136s_c[ict$icg9136s_c == 5] <- 4
ict$icg9136s_c[ict$icg9136s_c == 6] <- 5
ict$icg9136s_c[ict$icg9136s_c == 7] <- 6
ict$icg9140s_c[ict$icg9140s_c == 1] <- 0
ict$icg9140s_c[ict$icg9140s_c == 2] <- 1
ict$icg9140s_c[ict$icg9140s_c == 3] <- 2
ict$icg9140s_c[ict$icg9140s_c == 4] <- 3
# rotation variable
position <- data.frame(ID_t = ict$ID_t, position = rep(NA, nrow(ict)))
position[position$ID_t %in% ict$ID_t, 'position'] <- ict[, 'tx80211_w1']
position[!is.na(position$position) & (position$position == 128), 'position'] <- 0 # first
position[!is.na(position$position) & (position$position == 129), 'position'] <- 1 # first
position <- position[, 2, drop = FALSE]
# test data
resp <- ict[, names(ict) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , Q = Q
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, ict, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)



#==================#
#Items with maximum score of 0: ica5021s_c icg12013s_c icg12016s_c icg12018s_c icg12028s_c icg12050s_c icg12054s_c icg12060s_cError in .A.matrix2(resp, formulaA = formulaA, facets = facets, constraint = constraint,  :
#==================#
# wave 7
wave <- 'w7'
ict <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ict <- ict[ict$wave_w7 == 1, ]
# collapse items
ict$icg12013s_c[ict$icg12013s_c %in% c(1,2)] <- 0
ict$icg12013s_c[ict$icg12013s_c == 3] <- 1
ict$icg12018s_c[ict$icg12018s_c %in% c(1,2,3)] <- 0
ict$icg12018s_c[ict$icg12018s_c == 4] <- 1
ict$icg12060s_c[ict$icg12060s_c %in% c(1,2)] <- 0
ict$icg12060s_c[ict$icg12060s_c == 3] <- 1
ict$icg12060s_c[ict$icg12060s_c == 4] <- 2
ict$icg12016s_c[ict$icg12016s_c %in% c(1,2)] <- 0
ict$icg12016s_c[ict$icg12016s_c == 3] <- 1
ict$icg12016s_c[ict$icg12016s_c == 4] <- 2
ict$icg12054s_c[ict$icg12054s_c %in% c(1,2)] <- 0
ict$icg12054s_c[ict$icg12054s_c == 3] <- 1
ict$icg12054s_c[ict$icg12054s_c == 4] <- 2
ict$icg12138s_c[ict$icg12138s_c == 1] <- 0
ict$icg12138s_c[ict$icg12138s_c == 2] <- 1
ict$icg12138s_c[ict$icg12138s_c == 3] <- 2
ict$icg12138s_c[ict$icg12138s_c == 4] <- 3
ict$icg12028s_c[ict$icg12028s_c %in% c(1,2,3)] <- 0
ict$icg12028s_c[ict$icg12028s_c == 4] <- 1
ict$icg12028s_c[ict$icg12028s_c == 5] <- 2
ict$ica5021s_c[ict$ica5021s_c %in% c(1,2,3)] <- 0
ict$ica5021s_c[ict$ica5021s_c == 4] <- 1
ict$ica5021s_c[ict$ica5021s_c == 5] <- 2
ict$icg12107s_c[ict$icg12107s_c %in% c(1,2)] <- 0
ict$icg12107s_c[ict$icg12107s_c == 3] <- 1
ict$icg12107s_c[ict$icg12107s_c == 4] <- 2
ict$icg12107s_c[ict$icg12107s_c == 5] <- 3
ict$ica5052s_c[ict$ica5052s_c %in% c(1,2)] <- 0
ict$ica5052s_c[ict$ica5052s_c == 3] <- 1
ict$ica5052s_c[ict$ica5052s_c == 4] <- 2
ict$ica5052s_c[ict$ica5052s_c == 5] <- 3
ict$icg12048s_c[ict$icg12048s_c == 1] <- 0
ict$icg12048s_c[ict$icg12048s_c == 2] <- 1
ict$icg12048s_c[ict$icg12048s_c == 3] <- 2
ict$icg12048s_c[ict$icg12048s_c == 4] <- 3
ict$icg12048s_c[ict$icg12048s_c == 5] <- 4
ict$icg12119s_c[ict$icg12119s_c == 1] <- 0
ict$icg12119s_c[ict$icg12119s_c == 2] <- 1
ict$icg12119s_c[ict$icg12119s_c == 3] <- 2
ict$icg12119s_c[ict$icg12119s_c == 4] <- 3
ict$icg12119s_c[ict$icg12119s_c == 5] <- 4
ict$icg12050s_c[ict$icg12050s_c %in% c(1,2,3)] <- 0
ict$icg12050s_c[ict$icg12050s_c == 4] <- 1
ict$icg12050s_c[ict$icg12050s_c == 5] <- 2
ict$icg12050s_c[ict$icg12050s_c == 6] <- 3
ict$icg12004s_c[ict$icg12004s_c %in% c(1,2)] <- 0
ict$icg12004s_c[ict$icg12004s_c == 3] <- 1
ict$icg12004s_c[ict$icg12004s_c == 4] <- 2
ict$icg12004s_c[ict$icg12004s_c == 5] <- 3
ict$icg12004s_c[ict$icg12004s_c == 6] <- 4
ict$icg12046s_c[ict$icg12046s_c == 1] <- 0
ict$icg12046s_c[ict$icg12046s_c == 2] <- 1
ict$icg12046s_c[ict$icg12046s_c == 2] <- 2
ict$icg12046s_c[ict$icg12046s_c == 4] <- 3
ict$icg12046s_c[ict$icg12046s_c == 5] <- 4
ict$icg12046s_c[ict$icg12046s_c == 6] <- 5
# CROSS-SECTIONAL
# 281	B41: ICT-L1	B41
# 282	B41: ICT-L2	B41
# 283	B41: L1-ICT	B41
# 284	B41: L1-M1	B41
# 285	B41: L1-M2	B41
# 286	B41: L2-ICT	B41
# 287	B41: L2-M1	B41
# 288	B41: L2-M2	B41
# 289	B41: M1-L1	B41
# 290	B41: M1-L2	B41
# 291	B41: M2-L1	B41
# 292	B41: M2-L2	B41
# 293	B41: M2	B41
# 294	B41: M1	B41
# 295	B41: ICT	B41
# 296	A50: ICTA1-LA2a-MA3-STD4-EFLD5	A50
# 297	A50: ICTA1-LA2b-MA3-STD4-EFLD5	A50
# 298	A50: ICTA1-LA2a-MA3-EFLC4-STC5	A50
# 299	A50: ICTA1-LA2b-MA3-EFLC4-STC5	A50
# 300	A50: LB1a-ICTB2-MB3-STD4-EFLD5	A50
# 301	A50: LB1b-ICTB2-MB3-STD4-EFLD5	A50
# 302	A50: LB1a-ICTB2-MB3-EFLC4-STC5	A50
# 303	A50: LB1b-ICTB2-MB3-EFLC4-STC5	A50
# rotation variable
position <- data.frame(ID_t = ict$ID_t, position = rep(NA, nrow(ict)))
position[position$ID_t %in% ict$ID_t, 'position'] <- ict[, 'tx80211_w7']
position[!is.na(position$position) & (position$position %in% c(281,282,295:299)), 'position'] <- 0 # ict first
position[!is.na(position$position) & (position$position %in% c(283,286,300:303)), 'position'] <- 1 # reading first
position <- position[, 2, drop = FALSE]
# test data
resp <- ict[, names(ict) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# LONGITUDINAL
# estimation model
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , B = B
                      , verbose = FALSE
)
# linking
c <- 0.687
# add item difficulties/eaps to out object
mod$xsi.fixed.estimated[, 2] <- mod$xsi.fixed.estimated[, 2] + c
mod_l$xsi.fixed.estimated[, 2] <- mod_l$xsi.fixed.estimated[, 2] + c
eaps[[SC]][[domain]][[wave]][["cross"]] <- TAM::tam.mml(resp=resp2,irtmodel = "PCM2",A=A,B=B,xsi.fixed = mod$xsi.fixed.estimated,verbose=FALSE)$person
eaps[[SC]][[domain]][[wave]][["long"]] <- TAM::tam.mml(resp=resp,irtmodel = "PCM2",Q=Q,xsi.fixed = mod_l$xsi.fixed.estimated,verbose=FALSE)$person
# clear environment
rm(design, mod, position, ict, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)




# -------------------------------------------------------------------------------------------------------------

# Science
domain <- 'SC'
# wave 1
wave <- 'w1'
scie <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
scie <- scie[scie$wave_w1 == 1, ]
scie$scg9012s_c[scie$scg9012s_c == 1] <- 0
scie$scg9012s_c[scie$scg9012s_c == 2] <- 1
scie$scg9012s_c[scie$scg9012s_c == 3] <- 2
scie$scg9012s_c[scie$scg9012s_c == 4] <- 3
scie$scg9052s_c[scie$scg9052s_c == 1] <- 0
scie$scg9052s_c[scie$scg9052s_c == 2] <- 1
scie$scg9052s_c[scie$scg9052s_c == 3] <- 2
scie$scg9052s_c[scie$scg9052s_c == 4] <- 3
scie$scg9042s_c[scie$scg9042s_c == 1] <- 0
scie$scg9042s_c[scie$scg9042s_c == 2] <- 1
scie$scg9042s_c[scie$scg9042s_c == 3] <- 2
scie$scg9042s_c[scie$scg9042s_c == 4] <- 3
# rotation variable
position <- data.frame(ID_t = scie$ID_t, position = rep(NA, nrow(scie)))
position[position$ID_t %in% scie$ID_t, 'position'] <- scie[, 'tx80211_w1']
position[!is.na(position$position) & (position$position == 128), 'position'] <- 0 # first
position[!is.na(position$position) & (position$position == 129), 'position'] <- 1 # first
position <- position[, 2, drop = FALSE]
# test data
resp <- scie[, names(scie) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , Q = Q
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, scie, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)


# NOT YET IN SUF
# wave 5
wave <- 'w5'
# collapse items
# test data
# CROSS-SECTIONAL
# rotation variable
# estimation model
# LONGITUDINAL
# estimation model
# linking
mod$xsi.fixed.estimated[, 2] <- mod$xsi.fixed.estimated[, 2] + c
mod_l$xsi.fixed.estimated[, 2] <- mod_l$xsi.fixed.estimated[, 2] + c
eaps[[SC]][[domain]][[wave]][["cross"]] <- TAM::tam.mml(resp=resp2,irtmodel = "PCM2",A=A,B=B,xsi.fixed = mod$xsi.fixed.estimated,verbose=FALSE)$person
eaps[[SC]][[domain]][[wave]][["long"]] <- TAM::tam.mml(resp=resp,irtmodel = "PCM2",Q=Q,xsi.fixed = mod_l$xsi.fixed.estimated,verbose=FALSE)$person
# add item difficulties/eaps to out object
# clear environment




# -------------------------------------------------------------------------------------------------------------

# Russian
domain <- 'NR'
# wave 2
wave <- 'w2'
nr <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
nr <- nr[nr$wave_w2 == 1, ]
# test data
resp <- nr[, names(nr) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
mod <- TAM::tam.mml(resp = resp
                    , irtmodel = '1PL'
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(mod, nr, resp, wave)




# -------------------------------------------------------------------------------------------------------------

# Turkish
domain <- 'NT'
# wave 2
wave <- 'w2'
nt <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
nt <- nt[nt$wave_w2 == 1, ]
# test data
resp <- nt[, names(nt) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
mod <- TAM::tam.mml(resp = resp
                    , irtmodel = '1PL'
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(mod, nt, resp, wave)




# -------------------------------------------------------------------------------------------------------------

# English
domain <- 'EF'
# wave 3
wave <- 'w3'
ef <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ef <- ef[ef$wave_w3 == 1, ]
# load scaling results
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A48_G10/English/Version 2 (final)/out/data/pcm.Rdata")
# add item difficulties/eaps to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- pcm$all$mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- pcm$all$mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- pcm$all$mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- pcm$all$mod$person
# clear environment
rm(ef,pcm,wave)



# wave 7
wave <- 'w7'
# load scaling results
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A50_B41_G12/English/Version 3 (final)/out/data/pcm.Rdata")
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/A50_B41_G12/English/Version 3 (final)/out/data/pcml.Rdata")
# add item difficulties/eaps to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- pcm$mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- pcm$mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- pcml$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- pcml$person
# clear environment
rm(pcm,pcml)




# -------------------------------------------------------------------------------------------------------------

# Scientific thinking
domain <- 'ST'
# wave 7
wave <- 'w7'
st <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
st <- st[st$wave_w2 == 1, ]
# test data
resp <- st[, names(st) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
mod <- TAM::tam.mml(resp = resp
                    , irtmodel = '1PL'
                    , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(mod, st, resp, wave)




# -------------------------------------------------------------------------------------------------------------

# save out object
save(item_difficulties, file = 'data-raw/item_difficulties.RData')
save(eaps, file = 'data-raw/eaps.RData')
rm(list = ls())

