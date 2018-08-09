## Estimation of item difficulties of SC6 using TAM:
## Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM: Test
## analysis modules. R package version 2.9-35.
## https://CRAN.R-project.org/package=TAM
##
## The item parameters are corrected for test design changes
## and (if possible) linked to a common scale.
rm(list = ls())
.rs.restartR()

nvalid <- 3

data <- haven::read_sav("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Plausible Values/SUFs/SC6/SC6_xTargetCompetencies_D_8-0-0.sav")

item_labels <- list(SC6 = list(RE = list(w3 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c')
                                         , w5 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                  , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                  , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                  , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                  , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c')
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
                                                  , 'sca50520_c', 'sca50530_c', 'sca51020_c','sca51030_c'))
                               , IC = list(w5 = c('ica5001x_c','ica5003x_c','ica5005x_c','ica5004s_c','ica5006x_c','ica5007x_c','ica5008x_c'
                                                  ,'ica5010x_c','ica5017s_c','ica5018s_c','ica5015s_c','ica5019x_c','ica5016s_c','ica5020s_c'
                                                  ,'ica5023x_c','ica5027x_c','ica5026x_c','ica5029x_c','ica5028x_c','ica5030x_c'
                                                  ,'icg9119x_sc6a5_c','ica5050s_c','icg9122x_sc6a5_c','ica5047s_c','ica5046x_c','ica5021s_c'
                                                  ,'ica5052s_c','ica5054x_c','ica5057x_c'))))

# select test takers
data <- data[order(data$ID_t), ]

# output object
meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC6'

# Reading
# meanvar[[SC]][["RE"]][["w3"]][["cross"]] <- meanvar[[SC]][["RE"]][["w5"]][["cross"]] <- c(mean(data$rea3_sc1, na.rm = TRUE), 1.390)
meanvar[[SC]][["RE"]][["w3"]][["long"]] <- meanvar[[SC]][["RE"]][["w5"]][["long"]] <- c(mean(data$rea3_sc1u, na.rm = TRUE), 1.390)
# meanvar[[SC]][["RE"]][["w9"]][["cross"]] <- c(0, 1)
meanvar[[SC]][["RE"]][["w9"]][["long"]] <- c(0, 1)

# Mathematics
# meanvar[[SC]][["MA"]][["w3"]][["cross"]] <- c(mean(data$maa3_sc1, na.rm = TRUE), 1.679)
meanvar[[SC]][["MA"]][["w3"]][["long"]] <- c(mean(data$maa3_sc1u, na.rm = TRUE), 1.679)
# meanvar[[SC]][["MA"]][["w9"]][["cross"]] <- c(0, 1)
meanvar[[SC]][["MA"]][["w9"]][["long"]] <- c(0, 1)

# Information and Communication Technology literacy
# meanvar[[SC]][["IC"]][["w5"]][["cross"]] <- c(mean(data$ica5_sc1, na.rm = TRUE), 1.26)
meanvar[[SC]][["IC"]][["w5"]][["long"]] <- c(mean(data$ica5_sc1u, na.rm = TRUE), 1.26)

# Science
# meanvar[[SC]][["SC"]][["w5"]][["cross"]] <- c(mean(data$sca5_sc1, na.rm = TRUE), 1.003)
meanvar[[SC]][["SC"]][["w5"]][["long"]] <- c(mean(data$sca5_sc1u, na.rm = TRUE), 1.003)

# Scientific thinking
save(meanvar, file = "data-raw/meanvar.RData")


# Item difficulties for reading w3 (to be reused in w5)
domain <- 'RE'; wave <- 'w3'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 1, ]
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
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
mod1 <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# wave 5
wave <- 'w5'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 0 & read$wave_w5 == 1, ]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# model estimation
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod2 <- TAM::tam.mml(resp = resp
                    , irtmodel = 'PCM2'
                    , Q = Q
                    , verbose = FALSE
)
rownames(mod1$xsi.fixed.estimated) <- gsub(':', '_', rownames(mod1$xsi.fixed.estimated))
l <- merge(mod1$xsi.fixed.estimated, mod2$xsi.fixed.estimated, by = 0)
rownames(l) <- l[, 1]
l <- l[order(l[, 4]), c(4,3)]
colnames(l) <- colnames(mod2$xsi.fixed.estimated)
item_diff_SC6_RE_w3 <- as.matrix(l)
# clear environment
rm(mod1, mod2, read, resp, ind, wave, Q, A, B, design, l, position, resp2, xsi.elim, formulaA)
save(item_diff_SC6_RE_w3, file = 'data-raw/item_diff_SC6_RE_w3.RData')
