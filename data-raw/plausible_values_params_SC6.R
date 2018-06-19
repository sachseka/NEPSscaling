## Estimation of item difficulties of SC6 using TAM:
## Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM: Test
## analysis modules. R package version 2.9-35.
## https://CRAN.R-project.org/package=TAM
##
## The item parameters are corrected for test design changes
## and (if possible) linked to a common scale.
rm(list = ls())
.rs.restartR()

path <- '../../SUFs/SC6/'
nvalid <- 3

files <- list.files(path = path)
data <- haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))
rm(files)

item_labels <- list(SC6 = list(RE = list(w3 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c'
                                                , 'rea3012s_c_1', 'rea3015s_c_1', 'rea3028s_c_1', 'rea3028s_c_2'
                                                , 'rea3028s_c_3', 'rea3028s_c_4', 'rea3038s_c_1', 'rea3042s_c_1'
                                                , 'rea3054s_c_1')
                                         , w5 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                  , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                  , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                  , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                  , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c'
                                                  , 'rea3012s_c_1', 'rea3015s_c_1', 'rea3028s_c_1', 'rea3028s_c_2'
                                                  , 'rea3028s_c_3', 'rea3028s_c_4', 'rea3038s_c_1', 'rea3042s_c_1'
                                                  , 'rea3054s_c_1')
                                         , w9 = c())
                               , MA = list(w3 = c('maa3q071_c', 'mag9v131_sc6a3_c', 'mag9r261_sc6a3_c', 'mag9r111_sc6a3_c'
                                                  , 'maa3d131_c', 'maa3d132_c', 'mag9r051_sc6a3_c', 'maa3d041_c', 'maa3r081_c'
                                                  , 'maa3v082_c', 'mag9d201_sc6a3_c', 'maa3r091_c', 'mag9v121_sc6a3_c'
                                                  , 'maa3r121_c', 'maa3d112_c', 'maa3r011_c', 'maa3q101_c', 'mag5v321_sc6a3_c'
                                                  , 'mag9q021_sc6a3_c', 'maa3v061_c', 'maa3q021_c'),
                                           w9 = c())
                               , SC = list(w5 = c('sca56120_c', 'sca56130_c', 'sca51110_c', 'sca51140_c', 'sca50410_c', 'sca5652s_c'
                                                  , 'sca56540_c', 'sca51430_c', 'sca51440_c', 'sca50210_c', 'sca50220_c', 'sca50710_c'
                                                  , 'sca50720_c', 'sca56310_c', 'sca56320_c', 'sca5091s_c', 'sca56020_c', 'sca56030_c'
                                                  , 'sca50520_c', 'sca50530_c', 'sca51020_c','sca51030_c', 'sca5652s_c_1', 'sca5652s_c_2'
                                                  , 'sca5091s_c_1', 'sca5091s_c_2'))
                               , IC = list(w5 = c('ica5001x_c','ica5003x_c','ica5005x_c','ica5004s_c','ica5006x_c','ica5007x_c','ica5008x_c'
                                                  ,'ica5010x_c','ica5017s_c','ica5018s_c','ica5015s_c','ica5019x_c','ica5016s_c','ica5020s_c'
                                                  ,'ica5023x_c','ica5027x_c','ica5026x_c','ica5029x_c','ica5028x_c','ica5030x_c'
                                                  ,'icg9119x_sc6a5_c','ica5050s_c','icg9122x_sc6a5_c','ica5047s_c','ica5046x_c','ica5021s_c'
                                                  ,'ica5052s_c','ica5054x_c','ica5057x_c','ica5004s_c_1','ica5004s_c_2','ica5017s_c_1'
                                                  ,'ica5017s_c_2','ica5018s_c_1','ica5018s_c_2','ica5015s_c_1','ica5016s_c_1','ica5016s_c_2'
                                                  ,'ica5020s_c_1','ica5020s_c_2','ica5050s_c_1','ica5050s_c_2','ica5047s_c_1','ica5047s_c_2'
                                                  ,'ica5021s_c_1','ica5021s_c_2','ica5052s_c_1','ica5052s_c_2'))))

# select test takers
data <- data[order(data$ID_t), ]

# output object
item_difficulties <- list()
load(file = 'data-raw/item_difficulties.RData')
eaps <- list()
load(file = 'data-raw/eaps.RData')

# starting cohort
SC <- 'SC6'



# -------------------------------------------------------------------------------------------------------------

# Reading
domain <- 'RE'
# wave 3
wave <- 'w3'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 1, ]
# CROSS-SECTIONAL
# rotation variable
position <- data.frame(ID_t = read$ID_t, position = rep(NA, nrow(read)))
position[position$ID_t %in% read$ID_t, 'position'] <- read[, 'tx80211_w3']
position[!is.na(position$position) & (position$position == 122 | position$position == 124), 'position'] <- 0 # maths first
position[!is.na(position$position) & (position$position == 123 | position$position == 125), 'position'] <- 1 # reading first
if (sum(is.na(position[, 2])) > 0) {
    position[is.na(position$position), 'position'] <- sample(0:1, length(position[is.na(position$position), 'position']), replace = T)
}
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
eaps[[SC]][[domain]][[wave]][["long"]] <- mod$person
# clear environment
rm(design, mod, position, read, resp, resp2, xsi.elim, A, B, ind, wave, formulaA, mod_l, Q)


# wave 5
wave <- 'w5'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 0 & read$wave_w5 == 1, ]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# model estimation
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
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod$xsi.fixed.estimated
# clear environment
rm(mod, read, resp, B, Q, ind, wave)


# NOT YET IN SUF
# wave 9
wave <- 'w9'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w9 == 1, ]
# Rotation in w3, keine Rotation in w5 --> einer von beiden muss angepasst werden
position <- data.frame()
# category collapse

# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# CROSS-SECTIONAL: with SC4 w10 and SC5 w12
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
# rotation design: correction for change

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
# rotation design: correction for change
# linking
c <- c()
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(mod, read, resp, B, Q, ind, wave)






# -------------------------------------------------------------------------------------------------------------

# Mathematics
domain <- 'MA'
wave <- 'w3'
math <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
math <- math[math$wave_w3 == 1, ]
# CROSS-SECTIONAL
# rotation variable
position <- data.frame(ID_t = math$ID_t, position = rep(NA, nrow(math)))
position[position$ID_t %in% math$ID_t, 'position'] <- math[, 'tx80211_w3']
position[!is.na(position$position) & (position$position == 122 | position$position == 124), 'position'] <- 0 # maths first
position[!is.na(position$position) & (position$position == 123 | position$position == 125), 'position'] <- 1 # reading first
if (sum(is.na(position[, 2])) > 0) {
    position[is.na(position$position), 'position'] <- sample(0:1, length(position[is.na(position$position), 'position']), replace = T)
}
position <- position[, 2, drop = FALSE]
# test data
resp <- math[, names(math) %in% item_labels[[SC]][[domain]][[wave]]]
# model estimation
formulaA <- ~ 0 + item + position
mod <- TAM::tam.mml.mfr(resp = resp
                        , irtmodel = '1PL'
                        , formulaA = formulaA
                        , facets = position
                        , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["cross"]] <- mod$xsi.fixed.estimated
# LONGITUDINAL
mod_l <- TAM::tam.mml(resp = resp
                      , irtmodel = '1PL'
                      , verbose = FALSE
)
# add difficulties to out object
item_difficulties[[SC]][[domain]][[wave]][["long"]] <- mod_l$xsi.fixed.estimated
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(mod, math, resp, wave, domain, position, formulaA, mod_l)


# wave 9



# -------------------------------------------------------------------------------------------------------------

# Information and communication technology
domain <- 'IC'
wave <- 'w5'
ict <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
ict <- ict[ict$wave_w5 == 1, ]
# CROSS-SECTIONAL
# rotation variable
position <- data.frame(ID_t = ict$ID_t, position = rep(NA, nrow(ict)))
position[position$ID_t %in% ict$ID_t, 'position'] <- ict[, 'tx80211_w5']
position[!is.na(position$position) & position$position == 247, 'position'] <- 0 # science first
position[!is.na(position$position) & position$position == 248, 'position'] <- 1 # ict first
position[is.na(position$position), 'position'] <- 2 # reading first (should be: 249 -- all NA in SUF v.7)
position[position$position == 2, 'position'] <- NA
if (sum(is.na(position[, 2])) > 0) {
    position[is.na(position$position), 'position'] <- sample(0:1, length(position[is.na(position$position), 'position']), replace = T)
}
position <- position[, 2, drop = FALSE]
# test data
resp <- ict[, names(ict) %in% item_labels[[SC]][[domain]][[wave]]]
# PCM adjustments
resp$ica5018s_c[resp$ica5018s_c %in% c(2,3)] <- 1
resp$ica5018s_c[resp$ica5018s_c %in% c(4,5,6,7)] <- 2
resp$ica5018s_c[resp$ica5018s_c == 8] <- 3
resp$ica5015s_c[resp$ica5015s_c %in% c(0,1)] <- 0
resp$ica5015s_c[resp$ica5015s_c == 2] <- 1
resp$ica5015s_c[resp$ica5015s_c == 3] <- 2
resp$ica5020s_c[resp$ica5020s_c %in% c(0,1)] <- 0
resp$ica5020s_c[resp$ica5020s_c == 2] <- 1
resp$ica5020s_c[resp$ica5020s_c == 3] <- 2
resp$ica5020s_c[resp$ica5020s_c == 4] <- 3
resp$ica5050s_c[resp$ica5050s_c %in% c(0,1)] <- 0
resp$ica5050s_c[resp$ica5050s_c == 2] <- 1
resp$ica5050s_c[resp$ica5050s_c == 3] <- 2
resp$ica5050s_c[resp$ica5050s_c == 4] <- 3
resp$ica5004s_c[resp$ica5004s_c %in% c(0,1,2)] <- 0
resp$ica5004s_c[resp$ica5004s_c == 3] <- 1
resp$ica5004s_c[resp$ica5004s_c == 4] <- 2
resp$ica5004s_c[resp$ica5004s_c == 5] <- 3
resp$ica5017s_c[resp$ica5017s_c %in% c(0,1,2)] <- 0
resp$ica5017s_c[resp$ica5017s_c == 3] <- 1
resp$ica5017s_c[resp$ica5017s_c == 4] <- 2
resp$ica5017s_c[resp$ica5017s_c == 5] <- 3
resp$ica5016s_c[resp$ica5016s_c %in% c(0,1,2)] <- 0
resp$ica5016s_c[resp$ica5016s_c == 3] <- 1
resp$ica5016s_c[resp$ica5016s_c == 4] <- 2
resp$ica5016s_c[resp$ica5016s_c == 5] <- 3
resp$ica5047s_c[resp$ica5047s_c %in% c(0,1,2)] <- 0
resp$ica5047s_c[resp$ica5047s_c == 3] <- 1
resp$ica5047s_c[resp$ica5047s_c == 4] <- 2
resp$ica5047s_c[resp$ica5047s_c == 5] <- 3
resp$ica5021s_c[resp$ica5021s_c %in% c(0,1,2)] <- 0
resp$ica5021s_c[resp$ica5021s_c == 3] <- 1
resp$ica5021s_c[resp$ica5021s_c == 4] <- 2
resp$ica5021s_c[resp$ica5021s_c == 5] <- 3
resp$ica5052s_c[resp$ica5052s_c %in% c(0,1,2)] <- 0
resp$ica5052s_c[resp$ica5052s_c == 3] <- 1
resp$ica5052s_c[resp$ica5052s_c == 4] <- 2
resp$ica5052s_c[resp$ica5052s_c == 5] <- 3
# model estimation
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
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, ict, resp, resp2, xsi.elim, A, B, ind, wave, domain, formulaA, mod_l, Q)




# -------------------------------------------------------------------------------------------------------------

# Science
domain <- 'SC'
wave <- 'w5'
scie <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
scie <- scie[scie$wave_w5 == 1, ]
# CROSS-SECTIONAL
# rotation variable
position <- data.frame(ID_t = scie$ID_t, position = rep(NA, nrow(scie)))
position[position$ID_t %in% scie$ID_t, 'position'] <- scie[, 'tx80211_w5']
position[!is.na(position$position) & position$position == 247, 'position'] <- 0 # science first
position[!is.na(position$position) & position$position == 248, 'position'] <- 1 # ict first
position[is.na(position$position), 'position'] <- 2 # reading first (should be: 249 -- all NA in SUF v.7)
position[position$position == 2, 'position'] <- NA
if (sum(is.na(position[, 2])) > 0) {
    position[is.na(position$position), 'position'] <- sample(0:1, length(position[is.na(position$position), 'position']), replace = T)
}
position <- position[, 2, drop = FALSE]
# test data
resp <- scie[, names(scie) %in% item_labels[[SC]][[domain]][[wave]]]
# PCM adjustments
resp$sca5652s_c[resp$sca5652s_c %in% c(0, 1)] <- 0
resp$sca5652s_c[resp$sca5652s_c == 2] <- 1
resp$sca5652s_c[resp$sca5652s_c == 3] <- 2
resp$sca5652s_c[resp$sca5652s_c == 4] <- 3
resp$sca5091s_c[resp$sca5091s_c %in% c(0, 1)] <- 0
resp$sca5091s_c[resp$sca5091s_c == 2] <- 1
resp$sca5091s_c[resp$sca5091s_c == 3] <- 2
resp$sca5091s_c[resp$sca5091s_c == 4] <- 3
# model estimation
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
eaps[[SC]][[domain]][[wave]][["cross"]] <- mod$person
eaps[[SC]][[domain]][[wave]][["long"]] <- mod_l$person
# clear environment
rm(design, mod, position, scie, resp, resp2, xsi.elim, A, B, ind, wave, domain, formulaA, mod_l, Q)



# -------------------------------------------------------------------------------------------------------------

# save out object
save(item_difficulties, file = 'data-raw/item_difficulties.RData')
save(eaps, file = 'data-raw/eaps.RData')
rm(list = ls())

