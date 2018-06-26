## Estimation of item difficulties of SC5 using TAM:
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
# - missing in SUF: BA, EF, MA/RE w12

rm(list = ls())

path <- '../../SUFs/SC5/'
nvalid <- 3

files <- list.files(path = path)
data <- haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))
rm(files)
names(data)[grep('wave', names(data))]


item_labels <- list(SC5 = list(RE = list(w1 = c("res10110_c", "res1012s_c", "res10130_c", "res10140_c",
                                                "res10160_c", "res10170_c", "res10180_c", "res10190_c",
                                                "res1021s_c", "res1022s_c", "res10230_c", "res1024s_c",
                                                "res10250_c", "res10260_c", "res10270_c", "res10310_c",
                                                "res1032s_c", "res10330_c", "res10340_c", "res10350_c",
                                                "res10360_c", "res10370_c", "res10380_c", "res10410_c",
                                                "res10420_c", "res1043s_c", "res10440_c", "res10450_c"),
                                         w12 = c()),
                               MA = list(w1 = c("maa2q071_sc5s1_c", "mas1r092_c", "mas1v093_c",
                                                "mas1v032_c", "maa2d131_sc5s1_c", "maa2d132_sc5s1_c",
                                                "mas1v062_c", "mas1v063_c", "maa2r081_sc5s1_c",
                                                "maa2v082_sc5s1_c", "mas1q041_c", "mas1v042_c",
                                                "mas1q02s_c", "maa2d111_sc5s1_c", "maa2d112_sc5s1_c",
                                                "maa2r011_sc5s1_c", "mas1q011_c", "mag9r061_sc5s1_c",
                                                "mas1d071_c", "mas1d072_c"),
                                         w12 = c()),
                               IC = list(w5 = c("ics3001x_c", "ics3002s_c", "ics3003x_c", "ics3010x_c", "ics3011x_c",
                                                "ics3012x_c", "ics3014x_c", "ics3024x_c", "ics3015x_c", "ics3031x_c",
                                                "ics3038x_c", "ics3019x_c", "ics3023x_c", "ics3028x_c", "ics3029s_c",
                                                "ics3048x_c", "ics3049s_c", "ics3018s_c", "ics3041x_c", "ics3043x_c",
                                                "ics3030x_c", "ics3032x_c", "ics3033x_c", "ics3034x_c", "ics3045x_c",
                                                "ics3035s_c", "ics3037x_c", "ics3042x_c", "ics3044x_c", "ics3047x_c")),
                               SC = list(w5 = c("scs36310_c", "scs36320_c", "scs36220_c", "scs3623s_c", "scs30510_c",
                                                "scs30520_c", "scs31210_c", "scs31220_c", "scs31240_c", "scs30920_c",
                                                "scs30930_c", "scs30940_c", "scs3021s_c", "scs3022s_c", "scs36020_c",
                                                "scs3643s_c", "scs3642s_c", "scs3031s_c", "scs3033s_c", "scs3112s_c",
                                                "scs3131s_c", "scs3132s_c", "scs3133s_c", "scs3012s_c", "scs30130_c",
                                                "scs3061s_c", "scs30630_c", "scs30640_c", "scs30810_c")),
                               BA = list(w7 = c("bas7mar1_c", "bas7mar2_c", "bas7mar3_c", "bas7mar4_c", "bas7mar5_c",
                                                "bas7mar6_c", "bas7org1_c", "bas7org2_c", "bas7org3_c", "bas7org4_c",
                                                "bas7org5_c", "bas7org6_c", "bas7fin1_c", "bas7fin2_c", "bas7fin3_c",
                                                "bas7fin4_c", "bas7fin5_c", "bas7fin6_c", "bas7acc1_c", "bas7acc2_c",
                                                "bas7acc3_c", "bas7acc4_c", "bas7acc5_c", "bas7acc6_c", "bas7mic1_c",
                                                "bas7mic2_c", "bas7mic3_c", "bas7mic4_c", "bas7mic5_c", #"bas7mic6_c",
                                                #"bas7mac1_c",
                                                "bas7mac2_c", "bas7mac3_c", "bas7mac4_c", "bas7mac5_c",
                                                "bas7mac6_c")),
                               EF = list(w12 = c())))

# select test takers
data <- data[order(data$ID_t), ]

# output object
item_difficulties <- list()
load(file = 'data-raw/item_difficulties.RData')
eaps <- list()
load(file = 'data-raw/eaps.RData')

# starting cohort
SC <- 'SC5'



# -------------------------------------------------------------------------------------------------------------

# Reading
domain <- 'RE'
# wave 1
wave <- 'w1'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w1 == 1, ]
# rotation variable
position <- data.frame(ID_t = read$ID_t, position = rep(NA, nrow(read)))
position[position$ID_t %in% read$ID_t, 'position'] <- read[, 'tx80211_w1']
position[!is.na(position$position) & (position$position == 126), 'position'] <- 0 # reading first
position[!is.na(position$position) & (position$position == 127), 'position'] <- 1 # maths first
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
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , B = B
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, read, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)



# wave 12
wave <- 'w12'
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
math$mas1q02s_c[math$mas1q02s_c %in% c(1, 2)] <- 0
math$mas1q02s_c[math$mas1q02s_c == 3] <- 1
math$mas1q02s_c[math$mas1q02s_c == 4] <- 2
# rotation variable
position <- data.frame(ID_t = math$ID_t, position = rep(NA, nrow(math)))
position[position$ID_t %in% math$ID_t, 'position'] <- math[, 'tx80211_w1']
position[!is.na(position$position) & (position$position == 126), 'position'] <- 0 # reading first
position[!is.na(position$position) & (position$position == 127), 'position'] <- 1 # maths first
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
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
# LONGITUDINAL
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , B = B
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, math, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)



# wave 12
wave <- 'w12'
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
wave <- 'w5'
ict <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ict <- ict[ict$wave_w5 == 1, ]
ict$ics3002s_c[ict$ics3002s_c == 1] <- 0
ict$ics3002s_c[ict$ics3002s_c == 2] <- 1
ict$ics3029s_c[ict$ics3029s_c == 1] <- 0
ict$ics3029s_c[ict$ics3029s_c == 2] <- 1
ict$ics3029s_c[ict$ics3029s_c == 3] <- 2
ict$ics3035s_c[ict$ics3035s_c == 1] <- 0
ict$ics3035s_c[ict$ics3035s_c == 2] <- 1
ict$ics3035s_c[ict$ics3035s_c == 3] <- 2
ict$ics3035s_c[ict$ics3035s_c == 4] <- 3
ict$ics3049s_c[ict$ics3049s_c == 1] <- 0
ict$ics3049s_c[ict$ics3049s_c == 2] <- 1
ict$ics3049s_c[ict$ics3049s_c == 3] <- 2
# rotation variable
position <- data.frame(ID_t = ict$ID_t, position = rep(NA, nrow(ict)))
position[position$ID_t %in% ict$ID_t, 'position'] <- ict[, 'tx80211_w5']
position[!is.na(position$position) & (position$position == 330), 'position'] <- 0 # sc first
position[!is.na(position$position) & (position$position == 331), 'position'] <- 1 # ict first
position[!is.na(position$position) & (position$position == 332), 'position'] <- 0 # sc first
position[!is.na(position$position) & (position$position == 333), 'position'] <- 1 # ict first
position[!is.na(position$position) & (position$position == 334), 'position'] <- 0 # sc first
position[!is.na(position$position) & (position$position == 335), 'position'] <- 1 # ict first
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
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , B = B
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, ict, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)




# -------------------------------------------------------------------------------------------------------------

# Science
domain <- 'SC'
wave <- 'w5'
scie <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
scie <- scie[scie$wave_w5 == 1, ]
scie$scs3623s_c[scie$scs3623s_c == 1] <- 0
scie$scs3623s_c[scie$scs3623s_c == 2] <- 1
scie$scs3623s_c[scie$scs3623s_c == 3] <- 2
scie$scs3021s_c[scie$scs3021s_c == 1] <- 0
scie$scs3021s_c[scie$scs3021s_c == 2] <- 1
scie$scs3021s_c[scie$scs3021s_c == 3] <- 2
scie$scs3022s_c[scie$scs3022s_c == 1] <- 0
scie$scs3022s_c[scie$scs3022s_c == 2] <- 1
scie$scs3022s_c[scie$scs3022s_c == 3] <- 2
scie$scs3022s_c[scie$scs3022s_c == 4] <- 3
scie$scs3643s_c[scie$scs3643s_c == 1] <- 0
scie$scs3643s_c[scie$scs3643s_c == 2] <- 1
scie$scs3643s_c[scie$scs3643s_c == 3] <- 2
scie$scs3643s_c[scie$scs3643s_c == 4] <- 3
scie$scs3642s_c[scie$scs3642s_c == 1] <- 0
scie$scs3642s_c[scie$scs3642s_c == 2] <- 1
scie$scs3642s_c[scie$scs3642s_c == 3] <- 2
scie$scs3031s_c[scie$scs3031s_c == 1] <- 0
scie$scs3031s_c[scie$scs3031s_c == 2] <- 1
scie$scs3031s_c[scie$scs3031s_c == 3] <- 2
scie$scs3112s_c[scie$scs3112s_c == 1] <- 0
scie$scs3112s_c[scie$scs3112s_c == 2] <- 1
scie$scs3112s_c[scie$scs3112s_c == 3] <- 2
scie$scs3132s_c[scie$scs3132s_c == 1] <- 0
scie$scs3132s_c[scie$scs3132s_c == 2] <- 1
scie$scs3132s_c[scie$scs3132s_c == 3] <- 2
scie$scs3132s_c[scie$scs3132s_c == 4] <- 3
scie$scs3133s_c[scie$scs3133s_c %in% c(1, 2)] <- 0
scie$scs3133s_c[scie$scs3133s_c == 3] <- 1
scie$scs3133s_c[scie$scs3133s_c == 4] <- 2
scie$scs3012s_c[scie$scs3012s_c %in% c(1, 2)] <- 0
scie$scs3012s_c[scie$scs3012s_c == 3] <- 1
scie$scs3012s_c[scie$scs3012s_c == 4] <- 2
scie$scs3061s_c[scie$scs3061s_c %in% c(1, 2, 3)] <- 0
scie$scs3061s_c[scie$scs3061s_c == 4] <- 1
# rotation variable
position <- data.frame(ID_t = scie$ID_t, position = rep(NA, nrow(scie)))
position[position$ID_t %in% scie$ID_t, 'position'] <- scie[, 'tx80211_w5']
position[!is.na(position$position) & (position$position %in% c(330,332,334)), 'position'] <- 0 # sc first
position[!is.na(position$position) & (position$position %in% c(331,333,335)), 'position'] <- 1 # ict first
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
B <- TAM::designMatrices(modeltype = 'PCM', Q = Q, resp = resp)$B
B[ind, , ] <- 0.5 * B[ind, , ]
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = 'PCM2'
                      , B = B
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, scie, resp, resp2, xsi.elim, A, B, ind, wave, formulaA,Q,mod_l)




# -------------------------------------------------------------------------------------------------------------
# NOT YET IN SUF
# Business
domain <- 'BA'
wave <- 'w7'
ba <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ba <- ba[ba$wave_w7 == 1, ]
# test data
resp <- ba[, names(ba) %in% item_labels[[SC]][[domain]][[wave]]]
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
rm(mod, ba, resp, wave)




# -------------------------------------------------------------------------------------------------------------
# NOT YET IN SUF
# English
domain <- 'EF'
wave <- 'w12'
ef <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ef <- ef[ef$wave_w12 == 1, ]
# PCM adjustments

# test data
resp <- ef[, names(ef) %in% item_labels[[SC]][[domain]][[wave]]]
# CROSS-SECTIONAL
# rotation variable
# estimation model
# LONGITUDINAL
# estimation model
# linking
# add item difficulties/eaps to out object
# clear environment
rm(mod,ef,resp,wave)


# -------------------------------------------------------------------------------------------------------------

# save out object
save(item_difficulties, file = 'data-raw/item_difficulties.RData')
save(eaps, file = 'data-raw/eaps.RData')
rm(list = ls())
