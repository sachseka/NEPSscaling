## Code providing the internal data (item difficulties and WLEs)
## for plausible values estimation.
## The parameters are prepared in advance to ensure that proper
## corrections for test design changes, dropout etc. are applied
rm(list = ls())

# Item difficulties estimated in TAM
load(file = 'data-raw/item_difficulties.RData')
# Item difficulties in SC6 RE w3 are re-used in w5
# CROSS-SECTIONAL
rownames(item_difficulties$SC6$RE$w3$cross) <- gsub(':', '_', rownames(item_difficulties$SC6$RE$w3$cross))
l <- merge(item_difficulties$SC6$RE$w3$cross, item_difficulties$SC6$RE$w5$cross, by = 0)
rownames(l) <- l[, 1]
l <- l[order(l[, 4]), c(4,3)]
colnames(l) <- colnames(item_difficulties$SC6$RE$w5$cross)
item_difficulties$SC6$RE$w5$cross <- as.matrix(l)
# LONGITUDINAL
l <- merge(item_difficulties$SC6$RE$w3$long, item_difficulties$SC6$RE$w5$long, by = 0)
rownames(l) <- l[, 1]
l <- l[order(l[, 4]), c(4,3)]
colnames(l) <- colnames(item_difficulties$SC6$RE$w5$long)
item_difficulties$SC6$RE$w5$long <- as.matrix(l)
save(item_difficulties, file = 'data-raw/item_difficulties.RData')


# WLEs for IND
# TODO: verlinkte WLEs reinschreiben!
# _sc1: unverlinkt
# _sc1u: verlinkt
all.wle <- list(SC1 = list(w1 = list(cross = list(CD = 'cdn1_sc1'))
                           ,w2 = c()
                           ,w3 = c()
                           ,w4 = c()
                           ,w5 = c()
                           ,w6 = c()
                           ,w7 = c()
                           ,w8 = c()
                           ,w9 = c()
                           ,w10 = c()
                           ,w11 = c()
                           ,w12 = c()
)
, SC2 = list(w1 = list(cross = list(SC = 'sck1_sc1'),
                       long = list(SC = 'sck1_sc1u'))
             ,w2 = list(cross = list(MA = 'mak2_sc1'),
                        long = list(MA = 'mak2_sc1u'))
             ,w3 = list(cross = list(MA = 'mag1_sc1', SC = 'scg1_sc1', GR = 'grg1_sc1'),
                        long = list(MA = 'mag1_sc1u', SC = 'scg1_sc1u', GR = 'grg1_sc1u'))
             ,w4 = list(cross = list(MA = 'mag2_sc1', NR = 'nrg2_sc1', NT = 'ntg2_sc1', RE = c()),
                        long = list(MA = 'mag2_sc1u', NR = 'nrg2_sc1u', NT = 'ntg2_sc1u', RE = c()))
             ,w5 = list(cross = list(SC = 'scg3_sc1', IC = 'icg3_sc1'),
                        long = list(SC = 'scg3_sc1u', IC = 'icg3_sc1u'))
             ,w6 = list(cross = list(RE = c(), MA = c(), OR = c()),
                        long = list(RE = c(), MA = c(), OR = c()))
             ,w7 = c()
             ,w8 = c()
             ,w9 = c()
             ,w10 = c()
             ,w11 = c()
             ,w12 = c()
)
, SC3 =list(w1 = list(cross = list(MA = 'mag5_sc1', RE = 'reg5_sc1', OR = c('org5_sc1a','org5_sc1b')),
                      long = list(MA = 'mag5_sc1u', RE = 'reg5_sc1u', OR = c('org5_sc1au','org5_sc1bu')))
            ,w2 = list(cross = list(SC = 'scg6_sc1', IC = 'icg6_sc1'),
                       long = list(SC = 'scg6_sc1u', IC = 'icg6_sc1u'))
            ,w3 = list(cross = list(NR = 'nrg7_sc1', NT = 'ntg7_sc1', MA = 'mag7_sc1', RE = 'reg7_sc1', OR = c('org7_sc1a','org7_sc1b')),
                       long = list(NR = 'nrg7_sc1u', NT = 'ntg7_sc1u', MA = 'mag7_sc1u', RE = 'reg7_sc1u', OR = c('org7_sc1au','org7_sc1bu')))
            ,w4 = c()
            ,w5 = list(cross = list(SC = 'scg9_sc1', IC = 'icg9_sc1', OR = c('org9_sc1a','org9_sc1b'), MA = c()),
                       long = list(SC = 'scg9_sc1u', IC = 'icg9_sc1u', OR = c('org9_sc1au','org9_sc1bu'), MA = c()))
            ,w6 = list(cross = list(LI = 'lig9_sc1', NR = 'nrg9_sc3g9_sc1', NT = 'ntg9_sc3g9_sc1', RE = 'reg9_sc1'),
                       long = list(LI = 'lig9_sc1u', NR = 'nrg9_sc3g9_sc1u', NT = 'ntg9_sc3g9_sc1u', RE = 'reg9_sc1u'))
            ,w7 = list(cross = list(EF = 'efg10_sc1'),
                       long = list(EF = 'efg10_sc1u'))
            ,w8 = c()
            ,w9 = c()
            ,w10 = c()
            ,w11 = c()
            ,w12 = c()
)
, SC4 =list(w1 = list(cross = list(SC = 'scg9_sc1', MA = 'mag9_sc1', IC = 'icg9_sc1'),
                      long = list(SC = 'scg9_sc1u', MA = 'mag9_sc1u', IC = 'icg9_sc1u'))
            ,w2 = list(cross = list(NR = 'nrg9_sc1', NT = 'ntg9_sc1', RE = 'reg9_sc1'),
                       long = list(NR = 'nrg9_sc1u', NT = 'ntg9_sc1u', RE = 'reg9_sc1u'))
            ,w3 = list(cross = list(EF = 'efg10_sc1'),
                       long = list(EF = 'efg10_sc1u'))
            ,w4 = c()
            ,w5 = list(cross = list(SC = 'scg11_sc1'),
                       long = list(SC = 'scg11_sc1u'))
            ,w6 = c()
            ,w7 = list(cross = list(IC = 'icg12_sc1', MA = 'mag12_sc1', ST = 'stg12_sc1', RE = 'reg12_sc1', EF = 'efg12_sc1'),
                       long = list(IC = 'icg12_sc1u', MA = 'mag12_sc1u', ST = 'stg12_sc1u', RE = 'reg12_sc1u', EF = 'efg12_sc1u'))
            ,w8 = c()
            ,w9 = c()
            ,w10 = list(cross = list(RE = c(), MA = c()),
                        long = list(RE = c(), MA = c()))
            ,w11 = c()
            ,w12 = c()
)
, SC5 =list(w1 = list(cross = list(MA = 'mas1_sc1', RE = 'res1_sc1'),
                      long = list(MA = 'mas1_sc1u', RE = 'res1_sc1u'))
            ,w2 = c()
            ,w3 = c()
            ,w4 = c()
            ,w5 = list(cross = list(IC = 'ics3_sc1', SC ='scs3_sc1u'),# so im SUF
                       long = list(IC = 'ics3_sc1u', SC ='scs3_sc1u'))
            ,w6 = c()
            ,w7 = list(cross = list(BA = 'bas7_sc1'),
                       long = list(BA = 'bas7_sc1u'))
            ,w8 = c()
            ,w9 = c()
            ,w10 = c()
            ,w11 = c()
            ,w12 = list(cross = list(RE = c(), MA = c(), EF = c()),
                        long = list(RE = c(), MA = c(), EF = c()))
)
, SC6 = list(w1 = c()
             ,w2 = c()
             ,w3 = list(cross = list(MA = 'maa3_sc1', RE = 'rea3_sc1'),
                        long = list(MA = 'maa3_sc1u', RE = 'rea3_sc1u'))
             ,w4 = c()
             ,w5 = list(cross = list(IC = 'ica5_sc1', RE = 'rea5_sc1', SC = 'sca5_sc1'),
                        long = list(IC = 'ica5_sc1u', RE = 'rea5_sc1u', SC = 'sca5_sc1u'))
             ,w6 = c()
             ,w7 = c()
             ,w8 = c()
             ,w9 = list(cross = list(RE = c(), MA = c()),
                        long = list(RE = c(), MA = c()))
             ,w10 = c()
             ,w11 = c()
             ,w12 = c()
))

# EAPs without background model
load(file = 'data-raw/eaps.RData')

# estimate EAPs for SC6 RE w5 with correct item difficulties
path <- '../../SUFs/SC6/'
nvalid <- 3
files <- list.files(path = path)
data <- haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))
data <- data[rowSums(!is.na(data[, names(data) %in% rownames(item_difficulties$SC6$RE$w5$cross)])) >= nvalid, ]
data <- data[data$wave_w3 == 0 & data$wave_w5 == 1, ]
data <- data[, names(data) %in% rownames(item_difficulties$SC6$RE$w5$cross)]
ind <- which(apply(data, 2, max, na.rm = TRUE) > 1)
Q <- matrix(1, nrow = ncol(data), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod <- TAM::tam.mml(resp = data
                    , irtmodel = 'PCM2'
                    , Q = Q
                    , xsi.fixed = item_difficulties$SC6$RE$w5$cross
                    , verbose = FALSE
)
eaps[["SC6"]][["RE"]][["w5"]][["cross"]] <- mod$person
data <- haven::read_spss(file = paste0(path, files[grep('xTargetCompetencies', files)]))
data <- data[rowSums(!is.na(data[, names(data) %in% rownames(item_difficulties$SC6$RE$w5$long)])) >= nvalid, ]
data <- data[data$wave_w3 == 0 & data$wave_w5 == 1, ]
data <- data[, names(data) %in% rownames(item_difficulties$SC6$RE$w5$long)]
ind <- which(apply(data, 2, max, na.rm = TRUE) > 1)
Q <- matrix(1, nrow = ncol(data), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod <- TAM::tam.mml(resp = data
                    , irtmodel = 'PCM2'
                    , Q = Q
                    , xsi.fixed = item_difficulties$SC6$RE$w5$long
                    , verbose = FALSE
)
eaps[["SC6"]][["RE"]][["w5"]][["long"]] <- mod$person
rm(path, nvalid, files, data, mod, ind, Q, B)


rm(l)
setwd('Z:/Projektgruppen_(08)/Kompetenzen_BA_(p000011)/Methoden/Anna/02_Anleitung_Plausible_Values/R Code/')
devtools::use_data(all.wle, item_difficulties, eaps, pkg = 'NEPStools', internal = TRUE, overwrite = TRUE)
