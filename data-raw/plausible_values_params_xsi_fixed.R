## All item parameters are fixed to the parameters estimated in original
## scalings to counter differences in the item parameters caused by
## background models.

## Fixed item parameters for:
## - SC4 math/reading wave 10
## - SC5 math/reading wave 12
## - SC6 math/reading wave 9
## The same test forms were administered to the three starting cohorts and
## scaled together.
## To ensure comparability the item parameters of main scalings are used.
## Furthermore:
## - SC6 reading wave 5: refreshment sample was scaled with same item
##   parameters as original sample
## - SC5 English as a foreign language wave 12: same item parameters as in
##   SC4 were used

## ConQuest and TAM give different values for PC item difficulties
## --> TAM ~ ConQuest/2
## This has to be corrected for!
rm(list = ls())

# fixed item parameters
xsi.fixed <- list(cross = list(), long = list())

# auxiliary for half scoring of ConQuest-Values
pcitems <- function(SC, domain, wave){
  NEPSscaling:::get_indicators_for_half_scoring(SC, domain, wave)
}

### ----------------------------------------------------------------------------
### mathematical competence
### ----------------------------------------------------------------------------

# SC1
items <- list(
    w5 = c("man5z17s_c", "man5z021_c", "man5v181_c", "man5z161_c", "man5r14s_c",
           "man5d191_c", "man5z051_c", "man5g151_c", "man5r131_c", "man5g111_c",
           "man5z121_c", "man5v041_c", "man5z081_c", "man5d091_c", "man5z201_c",
           "man5g101_c", "man5z011_c", "man5r071_c", "man5d031_c", "man5v061_c"),
    w7 = c("man7z211_c", "man7z201_c", "man5v181_sc1n7_c", "man7z101_c",
           "man7r111_c", "man7g051_c", "man7g061_c", "man7v011_c", "man7r151_c",
           "man7g131_c", "man7z041_c", "man7d071_c", "man7g191_c", "man7r121_c",
           "man7z081_c", "man7v091_c", "man7z171_c", "man7d021_c", "man7z221_c",
           "man5z081_sc1n7_c", "man7g031_c", "man7z231_c", "man7r181_c", "man7v161_c",
           "man7z141_c"),
    w9 = c()
)
x1w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-0.353, -0.407, 0.706, -1.143, -0.872, -0.026, 0.669,
                      -1.121, 0.627, 1.765, -2.434, 0.558, 2.279, -2.830,
                      0.436, -1.835, 0.597, -0.153, 0.870, 0.185))
rownames(x1w5) <- items$w5
colnames(x1w5)[1] <- ""
x1w5[, 2][rownames(x1w5) %in% pcitems("SC1", "MA", "w5")] <-
  x1w5[, 2][rownames(x1w5) %in% pcitems("SC1", "MA", "w5")]/2
load("data-raw/item_difficulty_SC1_MA_w7.RData")
x1w7 <- item_difficulty_SC1_MA_w7
rm(item_difficulty_SC1_MA_w7)

# SC2
items <- list(
      w2 = c("mak2z221_c","mak2z231_c","mak2z101_c","mak2r111_c",
"mak2g041_c","mak2g051_c","mak2v001_c","mak2r151_c",
"mak2z031_c","mak2d062_c","mak2z161_c","mak2z171_c",
"mak2g211_c","mak2r131_c","mak2z091_c","mak2v08s_c",
"mak2z201_c","mak2d011_c","mak2z241_c","mak2z121_c",
"mak2v071_c","mak2g021_c","mak2z251_c","mak2r191_c",
"mak2v181_c","mak2z141_c"),
      w3 = c("mag1v051_c","mag1r141_c","mag1g171_c","mag1d131_c",
"mag1d132_c","mag1z061_c","mag1v01s_c","mag1z20s_c",
"mag1d09s_c","mag1z121_c","mag1g181_c","mag1d081_c",
"mag1r151_c","mag1z111_c","mag1v021_c","mag1z071_c",
"mag1d041_c","mag1g031_c","mag1z161_c","mag1v101_c",
"mag1r19s_c"),
      w4 = c("mag1v051_sc2g2_c","mag2v071_c","mag2r031_c","mag2d061_c",
"mag1d131_sc2g2_c","mag2r131_c","mag2v121_c","mag2q061_c",
"mag2r111_c","mag1d09s_sc2g2_c","mag1z121_sc2g2_c",
"mag2g12s_c","mag1d081_sc2g2_c","mag2g021_c","mag2r151_c",
"mag1v021_sc2g2_c","mag1z071_sc2g2_c","mag2d101_c",
"mag1g031_sc2g2_c","mag2v041_c","mag2q011_c",
"mag1r19s_sc2g2_c","mag2g091_c","mag2q051_c"),
      w6 = c("mag5d041_sc2g4_c", "mag4q101_c", "mag4r021_c", "mag5v271_sc2g4_c",
      "mag4q011_c", "mag4r071_c", "mag4d131_c",
     "mag5q231_sc2g4_c", "mag5q301_sc2g4_c", "mag4v121_c", "mag5d051_sc2g4_c",
     # "mag4q060_c",
     "mag4d031_c", #"mag5q140_sc2g4_c",
     "mag4v111_c", "mag4r041_c", "mag4r042_c",
     "mag4q051_c", "mag4q091_c", "mag4q092_c", "mag4d14s_c", "mag5v071_sc2g4_c",
     "mag5r191_sc2g4_c", "mag4d081_c")
)
x2w2 <- cbind(item = 1:length(items$w2),
              xsi = c(-2.688, -0.269, -0.031, -0.209, 1.624, 1.978,
                      -0.096, 0.174, 0.386, 1.098, 0.762, -0.934,
                      0.805, 1.646, 0.567, 0.101, -0.147, -0.903,
                      1.514, -0.466, -0.468, -1.341, 1.370, -0.882,
                      1.644, -0.669))
rownames(x2w2) <- items$w2
colnames(x2w2)[1] <- ""
x2w2[, 2][rownames(x2w2) %in% pcitems("SC2", "MA", "w2")] <-
    x2w2[, 2][rownames(x2w2) %in% pcitems("SC2", "MA", "w2")]/2
x2w3 <- cbind(item = 1:length(items$w3),
              xsi = c(-0.344, 1.301, -1.809, 0.123, -0.364,
                      -0.226, -1.054, -1.540, -0.390, 2.422,
                      -0.089, -0.611, -0.432, -1.059, 0.589,
                      0.7212, -1.069, -0.006, 0.155, -1.256, -0.114))
rownames(x2w3) <- items$w3
colnames(x2w3)[1] <- ""
x2w3[, 2][rownames(x2w3) %in% pcitems("SC2", "MA", "w3")] <-
  x2w3[, 2][rownames(x2w3) %in% pcitems("SC2", "MA", "w3")]/2
x2w4 <- cbind(item = 1:length(items$w4),
              xsi = c(-1.250, -0.779, -2.007, -0.584, -0.267, 0.253,
                      -0.633, 2.054, -0.056, -0.549, 1.609, -1.483,
                      -1.934, 0.512, -0.741, -0.199, -0.449, -1.614,
                      -1.383, -0.718, -0.288, -0.829, -0.603, -1.828))
rownames(x2w4) <- items$w4
colnames(x2w4)[1] <- ""
x2w4[, 2][rownames(x2w4) %in% pcitems("SC2", "MA", "w4")] <-
    x2w4[, 2][rownames(x2w4) %in% pcitems("SC2", "MA", "w4")]/2
x2w6 <- cbind(item = 1:length(items$w6),
              xsi = c(-0.268, 2.291, 0.507, 1.179, 1.955, 0.998, -2.839, 1.171,
                      1.051, -0.068, -1.582, #2.630,
                      0.547, #-0.026,
                      2.135,
                      -2.624, -0.783, -1.226, -0.881, 0.001, -1.723, -1.723,
                      0.686, -1.510))
rownames(x2w6) <- items$w6
colnames(x2w6)[1] <- ""
x2w6[, 2][rownames(x2w6) %in% pcitems("SC2", "MA", "w6")] <-
    x2w6[, 2][rownames(x2w6) %in% pcitems("SC2", "MA", "w6")]/2



# SC3
items <- list(
  w1 = c("mag5d041_c","mag5q291_c","mag5q292_c","mag5v271_c",
         "mag5r171_c","mag5q231_c","mag5q301_c","mag5q221_c",
         "mag5d051_c","mag5d052_c","mag5q14s_c","mag5q121_c",
         "mag5r101_c","mag5r201_c","mag5q131_c","mag5d02s_c",
         "mag5d023_c","mag5v024_c","mag5r251_c","mag5v01s_c",
         "mag5v321_c","mag5v071_c","mag5r191_c","mag5v091_c"),
  w3 = c("mag9q071_sc3g7_c","mag7v071_c","mag7r081_c","mag7q051_c",
         "mag5q301_sc3g7_c","mag9d151_sc3g7_c","mag5d051_sc3g7_c",
         "mag5d052_sc3g7_c","mag9v011_sc3g7_c","mag9v012_sc3g7_c",
         "mag7q041_c","mag7d042_c","mag7r091_c","mag9q181_sc3g7_c",
         "mag7d011_c","mag7v012_c","mag7v031_c","mag5r251_sc3g7_c",
         "mag7d061_c","mag5v321_sc3g7_c","mag9v091_sc3g7_c",
         "mag5r191_sc3g7_c","mag7r02s_c"),
  w5 = c("mag9d151_sc3g9_c","mag9d201_sc3g9_c","mag9d05s_c","mag9d061_c",
         "mag9d111_c","mag9d09s_c","mag9d131_c","mag9q021_sc3g9_c",
         "mag9q071_sc3g9_c","mag9q081_sc3g9_c","mag9q101_sc3g9_c",
         "mag9q181_sc3g9_c","mag9q211_sc3g9_c","mag9q121_c","mag9q151_c",
         "mag9q191_c","mag9q021_c","mag9q041_c","mag9q011_c","mag9q031_c",
         "mag9r051_sc3g9_c","mag9r061_sc3g9_c","mag9r111_sc3g9_c",
         "mag9r191_sc3g9_c","mag9r261_sc3g9_c","mag9r10s_c","mag9r14s_c",
         "mag9v011_sc3g9_c","mag9v012_sc3g9_c","mag9v091_sc3g9_c",
         "mag9v121_sc3g9_c","mag9v131_sc3g9_c","mag9v13s_sc3g9_c",
         "mag9v081_c"),
  w9 = c("maa3q071_sc3g12_c", "mag12v101_sc3g12_c", "mag12q121_sc3g12_c",
         "mag12v122_sc3g12_c", "mag12r011_sc3g12_c", "mag12v061_sc3g12_c",
         "mag12r091_sc3g12_c", "mag9r051_sc3g12_c", "mag12q081_sc3g12_c",
         "mag12d021_sc3g12_c", "mag12q051_sc3g12_c", "mag9d201_sc3g12_c",
         "mag9v121_sc3g12_c", "mas1q021s_sc3g12_c", "mas1d081_sc3g12_c",
         "maa3d112_sc3g12_c", "mag9r061_sc3g12_c", "maa3r011_sc3g12_c",
         "mag12d071_sc3g12_c", "mag12r041_sc3g12_c", "mag12v131_sc3g12_c",
         "mag12d031_sc3g12_c", "maa3d131_sc3g12_c", "maa3d132_sc3g12_c",
         "mag9v011_sc3g12_c", "maa3r121_sc3g12_c", "mag12q111_sc3g12_c",
         "maa3q101_sc3g12_c", "mag9q101_sc3g12_c", "mag12v132_sc3g12_c")
)
x3w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-0.37, -1.04, -0.799, 0.922, -0.082, 0.478, 0.603, -1.855,
                      -2.471, -0.5, -0.721, 1.514, -0.117, -1.225, -1.401,
                      -2.094, -0.428, -0.127, 0.219, -1.176, 0.977, -2.549,
                      -0.271, 0.333))
rownames(x3w1) <- items$w1
colnames(x3w1)[1] <- ""
x3w1[, 2][rownames(x3w1) %in% pcitems("SC3", "MA", "w1")] <-
  x3w1[, 2][rownames(x3w1) %in% pcitems("SC3", "MA", "w1")]/2
x3w3 <- cbind(item = 1:length(items$w3),
              xsi = c(-0.284, 0.533, 0.247, 0.348, -0.153, -1.328, -3.132,
                      -1.780, -0.543, 0.238, -0.634, -1.860, -0.090, -1.823,
                      -1.321, -0.213, -0.360, -0.490, 0.684, 0.259, 1.183,
                      -1.142, -1.191))
rownames(x3w3) <- items$w3
colnames(x3w3)[1] <- ""
x3w3[, 2][rownames(x3w3) %in% pcitems("SC3", "MA", "w3")] <-
  x3w3[, 2][rownames(x3w3) %in% pcitems("SC3", "MA", "w3")]/2
x3w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-1.499, 0.068, -1.923, -1.679, 0.028, 0.792, 0.236, 0.113,
                      -0.537, 0.432, -0.365, -2.23, 0.036, 0.577, -0.051, 0.404,
                      -1.643, 1.375, -0.619, 2.015, 0.2, 1.14, -0.509, -0.82,
                      2.342, -1.096, -1.276, -1.042, -0.288, 0.142, 1.187, 0.018,
                      -1.087, 1.115))
rownames(x3w5) <- items$w5
colnames(x3w5)[1] <- ""
x3w5[, 2][rownames(x3w5) %in% pcitems("SC3", "MA", "w5")] <-
  x3w5[, 2][rownames(x3w5) %in% pcitems("SC3", "MA", "w5")]/2
load("data-raw/item_difficulty_SC3_MA_w9.RData")
x3w9 <- item_difficulty_SC3_MA_w9
rm(item_difficulty_SC3_MA_w9)



#-----#

load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/MA/Version 3/output/data/data.Rdata")
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/MA/Version 3/output/data/pcm.Rdata")

xsi <- pcm$capi$mod$xsi.fixed.estimated
xsi <- xsi[1:length(items$poly$suf4), ]

# SC4
items <- list(
  w1 = c(
    "mag9q071_c", "mag9v131_c", "mag9v13s_c", "mag9r261_c",
    "mag9r111_c", "mag9d171_c", "mag9d151_c", "mag9r051_c",
    "mag9v011_c", "mag9v012_c", "mag9q161_c", "mag9d201_c",
    "mag9r191_c", "mag9v121_c", "mag9q181_c", "mag9r25s_c",
    "mag9r061_c", "mag9q081_c", "mag9q101_c", "mag9q021_c",
    "mag9v091_c", "mag9q211_c"
  ),
  w7 = c(
    "maa3q071_sc4g12_c", "mag12v101_c", "mag12q121_c", "mag12v122_c",
    "maa3d131_sc4g12_c", "maa3d132_sc4g12_c", "mag12r011_c", "mag12v061_c",
    "mag12r091_c", "mag9r051_sc4g12_c_d", "mag9r051_sc4g12_c_e",
    "mag9v011_sc4g12_c", "mag12q081_c", "mag12d021_c", "mag12q051_c",
    "mag9d201_sc4g12_c_g", "mag9d201_sc4g12_c_i", "mag9v121_sc4g12_c",
    "maa3r121_sc4g12_c", "mag12q111_c", "mas1q02s_sc4g12_c",
    "mas1d081_sc4g12_c", "maa3d112_sc4g12_c", "mag9r061_sc4g12_c",
    "maa3q101_sc4g12_c", "mag9q101_sc4g12_c", "maa3r011_sc4g12_c",
    "mag12r041_c", "mag12v131_c", "mag12v132_c",
    "mag12d031_c"
  )
)
x4w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-0.61, -0.01, -0.986, 2.585, -0.761, 0.029, -1.394,
                      0.439, -0.897, -0.17, 0.971, 0.222, -0.836, 1.289,
                      -2.144, -1.219, 1.053, 0.304, -0.724, 0.222, -0.314,
                      0.08))
rownames(x4w1) <- items$w1
colnames(x4w1)[1] <- ""
x4w1[, 2][rownames(x4w1) %in% pcitems("SC4", "MA", "w1")] <-
  x4w1[, 2][rownames(x4w1) %in% pcitems("SC4", "MA", "w1")]/2
x4w7long <- cbind(item = 1:length(items$w7),
              xsi = c(-0.510, -0.375, 0.747, 0.070, -0.099, 1.328, 0.299, 0.894,
                      0.512, -0.891, 0.064, -0.984, 1.545, -0.342, 1.143,
                      -1.295, -0.217, 0.090, -1.377, -0.270, -1.247, -0.934,
                      1.109, -0.033, 0.188, -1.354, -0.331, -0.514, -0.151,
                      -1.337, -0.340))
rownames(x4w7long) <- items$w7
colnames(x4w7long)[1] <- ""
x4w7long[, 2][rownames(x4w7long) %in% pcitems("SC4", "MA", "w7")] <-
  x4w7long[, 2][rownames(x4w7long) %in% pcitems("SC4", "MA", "w7")]/2
load("data-raw/item_difficulty_SC4_MA_w7.RData")
x4w7 <- item_difficulty_SC4_MA_w7
rm(item_difficulty_SC4_MA_w7)
x4w10 <- xsi
rownames(x4w10) <- NEPSscaling:::item_labels$SC4$MA$w10

# SC5

items <- c(
  "maa2q071_sc5s1_c", "mas1r092_c", "mas1v093_c",
  "mas1v032_c", "maa2d131_sc5s1_c", "maa2d132_sc5s1_c",
  "mas1v062_c", "mas1v063_c", "maa2r081_sc5s1_c",
  "maa2v082_sc5s1_c", "mas1q041_c", "mas1v042_c",
  "mas1q02s_c", "maa2d111_sc5s1_c", "maa2d112_sc5s1_c",
  "maa2r011_sc5s1_c", "mas1q011_c", "mag9r061_sc5s1_c",
  "mas1d071_c", "mas1d072_c"
)
x5w1 <- cbind(item = 1:length(items),
              xsi = c(
                -1.206, 1.748, -0.822, 0.450, -2.203, -0.646, 0.461, 0.344, -0.863,
                -0.472, -0.768, 0.710, -2.458, -1.279, 0.147, -1.212, -1.754, -0.522,
                -0.147, 1.171
              ))
rownames(x5w1) <- items
colnames(x5w1)[1] <- ""
x5w1[, 2][rownames(x5w1) %in% pcitems("SC5", "MA", "w1")] <-
  x5w1[, 2][rownames(x5w1) %in% pcitems("SC5", "MA", "w1")]/2
x5w12 <- xsi
rownames(x5w12) <- NEPSscaling:::item_labels$SC5$MA$w12

# SC6

items <- c(
  "maa3q071_c", "mag9v131_sc6a3_c", "mag9r261_sc6a3_c", "mag9r111_sc6a3_c",
  "maa3d131_c", "maa3d132_c", "mag9r051_sc6a3_c", "maa3d041_c", "maa3r081_c",
  "maa3v082_c", "mag9d201_sc6a3_c", "maa3r091_c", "mag9v121_sc6a3_c",
  "maa3r121_c", "maa3d112_c", "maa3r011_c", "maa3q101_c", "mag5v321_sc6a3_c",
  "mag9q021_sc6a3_c", "maa3v061_c", "maa3q021_c"
)
x6w3 <- cbind(item = 1:length(items),
              xsi = c(
                -0.527, -1.803, 1.550, -2.248, -0.813, 0.702, -0.229, -2.019, -0.215,
                0.691, -0.349, 1.481, -0.086, -1.195, 1.579, -0.124, -0.075, -1.120,
                -0.824, -3.183, 1.007
              ))
rownames(x6w3) <- items
colnames(x6w3)[1] <- ""
x6w9 <- xsi
rownames(x6w9) <- NEPSscaling:::item_labels$SC6$MA$w9

# store pre-scaled item parameters in list
xsi.fixed$cross[["MA"]] <-
  list(
    SC1 = list(
      w5 = x1w5,
      w7 = x1w7
    ),
    SC2 = list(
      w2 = x2w2,
      w3 = x2w3,
      w4 = x2w4,
      w6 = x2w6
    ),
    SC3 = list(
      w1 = x3w1,
      w3 = x3w3,
      w5 = x3w5,
      w9 = x3w9
    ),
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w10 = x4w10
    ),
    SC5 = list(
      w1 = x5w1,
      w12 = x5w12
    ),
    SC6 = list(
      w3 = x6w3,
      w9 = x6w9
    )
  )
xsi.fixed$long[["MA"]] <-
  list(
    SC1 = list(
      w5 = x1w5,
      w7 = x1w7
    ),
    SC2 = list(
      w2 = x2w2,
      w3 = x2w3,
      w4 = x2w4,
      w6 = x2w6
    ),
    SC3 = list(
      w1 = x3w1,
      w3 = x3w3,
      w5 = x3w5,
      w9 = x3w9
    ),
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7long,
      w10 = x4w10
    ),
    SC5 = list(
      w1 = x5w1,
      w12 = x5w12
    ),
    SC6 = list(
      w3 = x6w3,
      w9 = x6w9
    )
  )
rm(xsi, x4w10, x5w1, x6w3, x5w12, x6w9, items, dat, pcm)

### ----------------------------------------------------------------------------
### reading competence
### ----------------------------------------------------------------------------

# SC2
items <- list(
        w6 = c("reg50110_sc2g4_c","reg5012s_sc2g4_c","reg50130_sc2g4_c",
               "reg50140_sc2g4_c",
               "reg50150_sc2g4_c","reg5016s_sc2g4_c","reg50170_sc2g4_c",
               "reg50210_sc2g4_c",
               "reg50220_sc2g4_c","reg50230_sc2g4_c","reg50240_sc2g4_c",
               "reg50250_sc2g4_c",
               "reg5026s_sc2g4_c","reg50310_sc2g4_c","reg50320_sc2g4_c",
               "reg50330_sc2g4_c",
               "reg50340_sc2g4_c","reg50350_sc2g4_c","reg50360_sc2g4_c",
               "reg50370_sc2g4_c",
               "reg50410_sc2g4_c","reg5042s_sc2g4_c","reg50430_sc2g4_c",
               "reg50440_sc2g4_c",
               "reg50460_sc2g4_c","reg50510_sc2g4_c","reg5052s_sc2g4_c",
               "reg50530_sc2g4_c",
               "reg50540_sc2g4_c","reg5055s_sc2g4_c",#"reg50560_sc2g4_c",
               "reg50570_sc2g4_c")
      )
x2w6 <- cbind(item = 1:length(items$w6),
              xsi = c(-2.68794, -1.97647, -1.52571, -1.11327, -0.42908,
                      -1.05177,  1.42569, -2.27764,  0.08299, -1.91762,
                      -1.05172, -0.50207,  1.44749, -1.68471, -2.03109,
                      -2.05036, -0.98054, -0.22250, -1.68061, -0.75722,
                      -0.10508, -1.64459,  1.40923,  0.62386,  0.25773,
                      -1.62467, -0.82118,  0.50125, -0.87791, -0.77332,
                      -0.49841))
rownames(x2w6) <- items$w6
colnames(x2w6)[1] <- ""
x2w6[, 2][rownames(x2w6) %in% pcitems("SC2", "RE", "w6")] <-
    x2w6[, 2][rownames(x2w6) %in% pcitems("SC2", "RE", "w6")]/2
x2w6[, 2] <- x2w6[, 2] + (-0.567) # link to SC3 grade 5


# SC3
items <- list(
  w1 = c("reg50110_c","reg5012s_c","reg50130_c","reg50140_c",
         "reg50150_c","reg5016s_c","reg50170_c","reg50210_c",
         "reg50220_c","reg50230_c","reg50240_c","reg50250_c",
         "reg5026s_c","reg50310_c","reg50320_c","reg50330_c",
         "reg50340_c","reg50350_c","reg50360_c","reg50370_c",
         "reg50410_c","reg5042s_c","reg50430_c","reg50440_c",
         "reg50460_c","reg50510_c","reg5052s_c","reg50530_c",
         "reg50540_c","reg5055s_c","reg50560_c","reg50570_c"),
  w3 = c("reg70110_c","reg70120_c","reg7013s_c","reg70140_c",
         "reg7015s_c","reg7016s_c","reg70610_c","reg70620_c",
         "reg7063s_c",
         "reg70640_c","reg70650_c","reg7066s_c","reg70210_c",
         "reg70220_c",
         "reg7023s_c","reg7024s_c","reg70250_c","reg7026s_c",
         "reg70310_c","reg70320_c","reg7033s_c","reg70340_c",
         "reg70350_c","reg70360_c","reg70410_c","reg70420_c",
         "reg70430_c","reg70440_c","reg7045s_c","reg70460_c",
         "reg7051s_c","reg70520_c","reg7053s_c","reg7055s_c",
         "reg70560_c","reg7071s_c",
         "reg70720_c","reg70730_c","reg70740_c","reg7075s_c"),
  w6 = c("reg90610_c","reg90620_c","reg9063s_c","reg90640_c",
         #"reg90650_c",
         "reg90660_c","reg90670_c","reg90680_c",
         "reg90810_c","reg90820_c","reg9083s_c","reg90840_c",
         "reg90850_c","reg90860_c","reg90870_c","reg90210_sc3g9_c",
         "reg90220_sc3g9_c","reg90230_sc3g9_c",#"reg90240_sc3g9_c",
         "reg90250_sc3g9_c","reg90710_c","reg90720_c","reg90730_c",
         "reg9074s_c","reg90750_c","reg9091s_c","reg90920_c",
         "reg90930_c","reg90940_c","reg90950_c","reg90960_c",
         "reg9097s_c","reg90410_sc3g9_c","reg90420_sc3g9_c",
         "reg90430_sc3g9_c","reg90440_sc3g9_c","reg90450_sc3g9_c",
         "reg90460_sc3g9_c","reg9047s_sc3g9_c","reg90510_sc3g9_c",
         "reg90520_sc3g9_c","reg90530_sc3g9_c","reg90540_sc3g9_c",
         "reg90550_sc3g9_c","reg90560_sc3g9_c","reg90570_sc3g9_c"),
  w9 = c("reg1205010_sc3g12_c", "reg1205020_sc3g12_c",
         "reg1205030_sc3g12_c", "reg120504s_sc3g12_c",
         "reg1205050_sc3g12_c", "reg1205060_sc3g12_c",
         "reg1205070_sc3g12_c", "reg122301s_sc3g12_c",
         "reg1223020_sc3g12_c", "reg1223040_sc3g12_c",
         "reg122305s_sc3g12_c", "reg1223060_sc3g12_c",
         "reg1226020_sc3g12_c", "reg1226030_sc3g12_c",
         "reg1226040_sc3g12_c", "reg1226050_sc3g12_c",
         "reg1226060_sc3g12_c", "reg1226080_sc3g12_c",
         "reg121602s_sc3g12_c", "reg121603s_sc3g12_c",
         "reg1216040_sc3g12_c", "reg121605s_sc3g12_c",
         "reg1216060_sc3g12_c", "reg1220010_sc3g12_c",
         "reg122002s_sc3g12_c", "reg1220030_sc3g12_c",
         "reg1220040_sc3g12_c", "reg122005s_sc3g12_c",
         "reg1220060_sc3g12_c", "reg1229010_sc3g12_c",
         "reg1229020_sc3g12_c", "reg1229030_sc3g12_c",
         "reg1229060_sc3g12_c", "reg122907s_sc3g12_c",
         "reg1229080_sc3g12_c", "reg1229100_sc3g12_c",
         "reg122501s_sc3g12_c", "reg1225030_sc3g12_c",
         "reg1225060_sc3g12_c", "reg1225050_sc3g12_c",
         "reg122504s_sc3g12_c")
)
x3w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-3.383, -2.865, -2.280, -1.623, -0.609, -1.496, 0.842,
                      -2.886, -0.053, -2.662, -1.428, -0.816, 1.012, -2.341,
                      -2.900, -2.660, -1.633, -0.643, -2.362, -1.337, -0.598,
                      -2.072, 0.878, 0.436, -0.123, -2.383, -1.533, -0.001,
                      -1.418, -1.355, -0.362, -0.968))
rownames(x3w1) <- items$w1
colnames(x3w1)[1] <- ""
x3w1[, 2][rownames(x3w1) %in% pcitems("SC3", "RE", "w1")] <-
  x3w1[, 2][rownames(x3w1) %in% pcitems("SC3", "RE", "w1")]/2
x3w3 <- cbind(item = 1:length(items$w3),
              xsi = c(-0.375, -2.524, -2.594, -3.456, -2.940, -1.099, -2.847,
                      -0.613, -2.706, 0.464, 0.229, -1.208, -2.792, -1.941,
                      -1.932, -0.754, -1.003, -1.419, -2.629, -1.627, -1.215,
                      -1.533, -2.040, -1.252, -2.350, -1.892, -2.403, -1.917,
                      -0.469, 0.801, -1.963, -1.292, -1.164, 0.124, 0.522,
                      -1.482, 0.918, 0.631, -0.911, 0.318))
rownames(x3w3) <- items$w3
colnames(x3w3)[1] <- ""
x3w3[, 2][rownames(x3w3) %in% pcitems("SC3", "RE", "w3")] <-
  x3w3[, 2][rownames(x3w3) %in% pcitems("SC3", "RE", "w3")]/2
x3w6 <- cbind(item = 1:length(items$w6),
              xsi = c(-0.12149, 0.63415, 0.16412, -0.26008, 0.74353, -0.31853,
                      0.47501, 0.10495, 0.42921, -0.55437, 1.51681, 0.07977,
                      0.31241, -0.05699, -2.35615, -0.79565, -1.80772, 0.22741,
                      -0.11381, -0.73522, 0.64298, -0.28021, -0.37366, -0.18588,
                      0.26582, 0.34081, -0.24743, 1.09507, -0.43401, -0.33022,
                      -2.27955, -1.17658, -0.86077, -1.55659, -1.85723,
                      -0.72024, -1.92447, -0.08271, -0.56101, -0.35275, 0.31567,
                      -0.14051, -0.62121, -1.43628))
rownames(x3w6) <- items$w6
colnames(x3w6)[1] <- ""
x3w6[, 2][rownames(x3w6) %in% pcitems("SC3", "RE", "w6")] <-
  x3w6[, 2][rownames(x3w6) %in% pcitems("SC3", "RE", "w6")]/2
x3w9 <- cbind(item = 1:length(items$w9),
              xsi = c(-1.256, -0.642, -2.234, -2.306, -0.792, -1.263, -2.181,
                      -1.076, 0.208, 0.243, -0.544, -0.599, -2.237, -0.642,
                      -1.953, -1.59, -0.165, -1.44, -0.842, -0.467, -1.806,
                      -0.552, -1.73, -0.877, -1.475, -0.53, 0.319, 0.091,
                      -0.163, -1.479, -0.862, 0.634, -0.189, -0.72, -0.373,
                      -0.942, -1.551, -0.583, -0.531, 1.085, -1.594))
rownames(x3w9) <- items$w9
colnames(x3w9)[1] <- ""

# SC4
items <- list(
  w2 = c(
    "reg90110_c", "reg90120_c", "reg90150_c", "reg9016s_c",
    "reg9017s_c", "reg90210_c", "reg90220_c", "reg90230_c",
    "reg90240_c", "reg90250_c", "reg90310_c", "reg90320_c",
    "reg9033s_c", "reg90340_c", "reg90350_c", "reg90360_c",
    "reg90370_c", "reg90410_c", "reg90420_c", "reg90430_c",
    "reg90440_c", "reg90450_c", "reg90460_c", "reg9047s_c",
    "reg90510_c", "reg90520_c", "reg90530_c", "reg90540_c",
    "reg90550_c", "reg90560_c", "reg90570_c"
  ),
  w7 = c(
    "reg120110_c", "reg120120_c", "reg120130_c", "reg12014s_c", "reg120150_c",
    "reg120160_c", "reg120170_c", "reg12021s_c", "reg120220_c", "reg120230_c",
    "reg12024s_c", "reg120250_c", "reg12026s_c", "reg120310_c", "reg120320_c",
    "reg120330_c", "reg120340_c", "reg120350_c", "reg120360_c", # "reg12041s_c",
    "reg12042s_c", "reg120430_c", "reg12044s_c", "reg120450_c", "reg120510_c",
    "reg12052s_c", "reg120530_c", "reg120540_c", "reg12055s_c", "reg120560_c",
    "reg120610_c", "reg120620_c", "reg120630_c", "reg120640_c", "reg12065s_c",
    "reg120660_c", "reg120670_c", "reg12071s_c", "reg120720_c", "reg120730_c",
    "reg120740_c", "reg12075s_c"
  )
)
x4w2 <- cbind(item = 1:length(items$w2),
              xsi = c(-3.801, -4.613, -1.726, -1.627, -3.753, -2.375,
                      -0.696, -2.456, -2.460, 0.572, -2.345, -3.176,
                      -3.125, -2.597, -2.818, -1.531, -0.852,
                      -2.362, -1.421, -0.983, -1.734, -2.105, -0.766,
                      -1.968, -0.171, -0.661, -0.498, 0.129, -0.053,
                      -0.754, -1.966))
rownames(x4w2) <- items$w2
colnames(x4w2)[1] <- ""
x4w2[, 2][rownames(x4w2) %in% pcitems("SC4", "RE", "w2")] <-
  x4w2[, 2][rownames(x4w2) %in% pcitems("SC4", "RE", "w2")]/2
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-1.289, -0.821, -2.313, -2.679, -0.907, -1.141, -2.146,
                      -0.807, -0.488, -1.691, -1.142, -1.831, -0.223, -0.145,
                      -1.627, -0.940, 0.412, -0.397, -0.809, 0.167, -0.031,
                      -1.285, -0.710, -2.223, -1.178, -2.016, -1.499, -0.423,
                      -1.176, -0.422, -0.881, -1.517, -0.475, -1.460, 1.132,
                      -1.500, -0.942, -1.672, -0.073, 0.066, -1.288))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
x4w7[, 2][rownames(x4w7) %in% pcitems("SC4", "RE", "w7")] <-
  x4w7[, 2][rownames(x4w7) %in% pcitems("SC4", "RE", "w7")]/2
# x4w2_special <- cbind(item = 1:length(items$w2),
#                       xsi = c(-1.46, -2.18, 0.54, 0.61, -0.87, 0.20,
#                               0.97, -0.07, 0.65, 1.45, 0.19, -0.20,
#                               -0.66, -0.41, -0.39, -0.50, 0.47, -0.91,
#                               0.39, 0.61, 0.01, -0.68, 0.75, -0.33,
#                               0.54, 0.12, 0.04, 0.23, 0.11, 0.14,
#                               -0.08, 0.27, 0.14, 0.46))
# rownames(x4w2_special) <- items$w2
# colnames(x4w7_special)[1] <- ""
# x4w7_special <- cbind(item = 1:length(items$w7),
#                       xsi = c(-2.41, -2.21, 0.33, 0.16, 0.24, -1.27, -1.28,
#                               0.42, -1.62, -0.16, 1.28, -0.81, -1.96, -1.04,
#                               -1.11, -0.94, -0.62, 0.05, -1.19, -0.21, 0.22,
#                               -0.89, -1.21, 0.09))
# rownames(x4w7_special) <- items$w7
# colnames(x4w7_special)[1] <- ""
# load("data-raw/item_difficulty_SC4_RE_w10_long.RData")
# x4w10long <- item_difficulty_SC4_RE_w10
# rm(item_difficulty_SC4_RE_w10)

# SC5
items <- c(
  "res10110_c", "res1012s_c", "res10130_c", "res10140_c",
  "res10160_c", "res10170_c", "res10180_c", "res10190_c",
  "res1021s_c", "res1022s_c", "res10230_c", "res1024s_c",
  "res10250_c", "res10260_c", "res10270_c", "res10310_c",
  "res1032s_c", "res10330_c", "res10340_c", "res10350_c",
  "res10360_c", "res10370_c", "res10380_c", "res10410_c",
  "res10420_c", "res1043s_c", "res10440_c", "res10450_c"
)
x5w1 <- cbind(item = 1:length(items),
              xsi = c(
                -1.154, -4.048, -3.441, -0.793, 0.124, -1.172, -2.441, -0.162, -2.538,
                -3.151, -0.768, -1.384, -0.639, -1.892, -1.132, -1.909, -0.533,
                -1.384, 0.259, -0.274, -0.822, -0.504, -1.949, -1.896, -2.603,
                -1.197, -0.838, -1.344
              ))
rownames(x5w1) <- items
colnames(x5w1)[1] <- ""
x5w1[, 2][rownames(x5w1) %in% pcitems("SC5", "RE", "w1")] <-
  x5w1[, 2][rownames(x5w1) %in% pcitems("SC5", "RE", "w1")]/2


# B110, B116, B114
xsi <- readODS::read_ods(
  path = "data-raw/neps_raw/b110_114_116_alte_schwierigkeiten.ods",
  col_names = FALSE
)

# match correct difficulties to cohort
items <- list(
  w4 = c(1:10, 12:17, 20, 22:26, 28:31, 33:42),
  w5 = c(6:10, 12, 13, 15, 16, 19, 20, 22:24, 27:29, 32, 39, 40, 42),
  w6 = c(1:9, 11:16, 18, 21:26, 28:31, 33:42)
)
x4w10 <- xsi[items$w4, ]
rownames(x4w10) <- NEPSscaling:::item_labels$SC4$RE$w10
colnames(x4w10)[1] <- ""
x4w10[, 1] <- 1:nrow(x4w10)
x4w10 <- as.matrix(x4w10)
x5w12 <- xsi[items$w5, ]
rownames(x5w12) <- NEPSscaling:::item_labels$SC5$RE$w12
colnames(x5w12)[1] <- ""
x5w12[, 1] <- 1:nrow(x5w12)
x5w12 <- as.matrix(x5w12)
x6w9 <- xsi[items$w6, ]
rownames(x6w9) <- NEPSscaling:::item_labels$SC6$RE$w9
colnames(x6w9)[1] <- ""
x6w9[, 1] <- 1:nrow(x6w9)
x6w9 <- as.matrix(x6w9)

# wave 5 sample refreshment
load("data-raw/item_diff_SC6_RE_w3.RData")
item_diff_SC6_RE_w3 <- item_diff_SC6_RE_w3[1:30, ]

# store pre-scaled item parameters in list
xsi.fixed$cross[["RE"]] <-
  list(
    SC2 = list(
      w6 = x2w6
    ),
    SC3 = list(
      w1 = x3w1,
      w3 = x3w3,
      w6 = x3w6,
      w9 = x3w9
    ),
    SC4 = list(
      w2 = x4w2,
      w7 = x4w7,
      w10 = x4w10
    ),
    SC5 = list(
      w1 = x5w1,
      w12 = x5w12
    ),
    SC6 = list(
      w3 = item_diff_SC6_RE_w3,
      w5 = item_diff_SC6_RE_w3,
      w9 = x6w9
    )
  )
xsi.fixed$long[["RE"]] <-
  list(
    SC2 = list(
      w6 = x2w6
    ),
    SC3 = list(
      w1 = x3w1,
      w3 = x3w3,
      w6 = x3w6,
      w9 = x3w9
    ),
    SC4 = list(
      w2 = x4w2,
      w7 = x4w7,
      w10 = x4w10
    ),
    SC5 = list(
      w1 = x5w1,
      w12 = x5w12
    ),
    SC6 = list(
      w3 = item_diff_SC6_RE_w3,
      w5 = item_diff_SC6_RE_w3,
      w9 = x6w9
    )
  )


### ----------------------------------------------------------------------------
### ICT literacy
### ----------------------------------------------------------------------------

# SC2
items <- list(
        w5 = c("icg3052x_c","icg3350x_c","icg3021x_c","icg3610x_c",
               "icg3621x_c","icg3371x_c","icg3081x_c","icg3102x_c",
               "icg3591x_c","icg3092x_c","icg3381x_c","icg3400x_c",
               "icg3661x_c","icg3410x_c","icg3420x_c","icg3432x_c",
               "icg3440x_c","icg3322x_c","icg3461x_c","icg3211x_c",
               "icg3510x_c","icg3221x_c","icg3601x_c","icg3260x_c",
               "icg3301x_c","icg3270x_c","icg3292x_c","icg3481x_c",
               "icg3541x_c","icg3550x_c")
)
x2w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-0.26, 0.65, -0.52, -1.00, 0.77, 1.93, 0.97,
                      1.07, 1.04, 0.77, -0.69, -1.04, 0.49, -1.04,
                      0.92, -0.30, -0.25, 0.56, 0.44, -1.03, 0.00,
                      0.13, -0.27, 0.21, -1.29, -0.21, 1.31, 0.51,
                      1.02, -0.25))
rownames(x2w5) <- items$w5
colnames(x2w5)[1] <- ""

# SC3
items <- list(
  w2 = c("icg6001x_c","icg6003x_c","icg6005x_c","icg6006x_c",
         "icg6009x_c","icg6011x_c","icg6012x_c","icg6013x_c",
         "icg6014x_c","icg6015x_c","icg6020x_c","icg6016x_c",
         "icg6018x_c","icg6021x_c","icg6024x_c","icg6025x_c",
         "icg6031x_c","icg6032x_c","icg6033x_c","icg6034x_c",
         "icg6036x_c","icg6039x_c","icg6042x_c","icg6047x_c",
         "icg6048x_c","icg6049x_c","icg6046x_c","icg6053x_c",
         "icg6054x_c","icg6059x_c"),
  w5 = c("icg5005x_sc3g9_c","icg5034x_sc3g9_c","icg5009x_sc3g9_c",
         "icg5051x_c","icg5018x_sc3g9_c","icg9106x_sc3g9_c",
         "icg5015x_sc3g9_c","icg5046x_sc3g9_c","icg5033x_sc3g9_c",
         "icg9110x_sc3g9_c","icg5045x_c","icg5054x_sc3g9_c",
         "icg5021x_sc3g9_c","icg9114x_sc3g9_c","icg5059x_sc3g9_c",
         "icg9116x_sc3g9_c","icg5035x_c","icg9118x_sc3g9_c",
         "icg9119x_sc3g9_c","icg5003x_sc3g9_c","icg5029x_c",
         "icg9122x_sc3g9_c","icg9123x_sc3g9_c","icg12041x_sc3g9_c",
         "icg12042x_c","icg12060s_sc3g9_c","icg12036x_c",
         "icg5039x_sc3g9_c","icg12018s_sc3g9_c","icg5053x_sc3g9_c",
         "icg9131x_sc3g9_c","icg9132x_sc3g9_c","icg5049x_sc3g9_c",
         "icg12022x_c","icg9138x_sc3g9_c","icg9140s_sc3g9_c",
         "icg9102s_sc3g9_c","icg5047x_sc3g9_c","icg12034x_sc3g9_c",
         "icg9113x_sc3g9_c","icg12040x_sc3g9_c","icg12043x_c",
         "icg9117s_sc3g9_c","ica5021s_sc3g9_c","icg9128x_sc3g9_c" ,
         "icg9133s_sc3g9_c","icg9136s_sc3g9_c","icg12027x_c",
         "icg9101x_sc3g9_c","icg9103x_sc3g9_c","icg9107s_sc3g9_c",
         "icg12138s_sc3g9_c","icg12016s_sc3g9_c","icg9111x_sc3g9_c",
         "icg12047s_sc3g9_c","icg12046s_sc3g9_c","ica5052s_sc3g9_c",
         "icg9125s_sc3g9_c","icg9129x_sc3g9_c","icg12050s_sc3g9_c"),
  w9 = c("icg12018s_sc3g12_c", "ica4003x_sc3g12_c", "icg12107s_sc3g12_c",
         "icg12004s_sc3g12_c", "icg12010x_sc3g12_c", "icg12011x_sc3g12_c",
         "ica4008x_sc3g12_c", "icg12060s_sc3g12_c", "icg12013s_sc3g12_c",
         "ica4018s_sc3g12_c", "icg12016s_sc3g12_c", "ica4019x_sc3g12_c",
         "icg12121x_sc3g12_c", "icg12028s_sc3g12_c", "ica4023x_sc3g12_c",
         "ica4027x_sc3g12_c", "icg12033x_sc3g12_c", "icg12034x_sc3g12_c",
         "icg12035x_sc3g12_c", "icg12040x_sc3g12_c", "icg12037s_sc3g12_c",
         "icg12138s_sc3g12_c", "icg12047s_sc3g12_c", "icg12041x_sc3g12_c",
         "icg12046s_sc3g12_c", "ica4021s_sc3g12_c", "ica4052s_sc3g12_c",
         "icg12048s_sc3g12_c", "icg12050s_sc3g12_c", "icg12054s_sc3g12_c",
         "icg12109s_sc3g12_c", "icg12119s_sc3g12_c")
)
x3w2 <- cbind(item = 1:length(items$w2),
              xsi = c(1.21, 0.56, 1.04, -1.40, -0.41, -0.05, -1.69, -0.42,
                      -1.43, 0.05, -2.17, -0.75, -0.81, -0.14, -1.32, 0.12,
                      0.27, -0.76, 0.13, -0.58, -1.28, -0.89, -0.51, 0.79,
                      -0.83, -0.37, -0.74, 0.68, -0.08, -0.29))
rownames(x3w2) <- items$w2
colnames(x3w2)[1] <- ""
x3w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-0.117, -1.020, -0.637, -0.826, -1.498, -0.672, -1.002,
                      -1.154, -0.610, 0.273, -0.543, -0.624, -1.590, -0.752,
                      -0.753, -1.406, -1.823, -0.436, -0.847, -0.668, -0.770,
                      -0.034, -0.833, -0.256, -1.044, -1.520, -0.232, -1.505,
                      -1.494, -0.762, -0.597, -0.733, -1.087, -1.441, -0.773,
                      -2.259, -0.603, 0.602, -0.116, 0.828, 1.443, 0.109,
                      -1.465, -1.846, 0.477, -0.833, -0.221, 0.769, 0.808,
                      1.156, -1.226, -0.720, -0.997, 0.382, 0.667, 0.311,
                      -0.528, -1.200, 1.393, -0.467))
rownames(x3w5) <- items$w5
colnames(x3w5)[1] <- ""
x3w5[, 2][rownames(x3w5) %in% pcitems("SC3", "IC", "w5")] <-
  x3w5[, 2][rownames(x3w5) %in% pcitems("SC3", "IC", "w5")]/2
load("data-raw/item_difficulty_SC3_IC_w9.RData")
x3w9 <- item_difficulty_SC3_IC_w9
rm(item_difficulty_SC3_IC_w9)
load("data-raw/item_difficulty_SC3_IC_w9_long.RData")
x3w9long <- item_difficulty_SC3_IC_w9_long
rm(item_difficulty_SC3_IC_w9_long)



# SC4
items <- list(
  w1 = c(
    "icg9101x_c", "icg9102s_c", "icg9103x_c", "icg9104x_c",
    "icg9105x_c", "icg9106x_c", "icg9107s_c", "icg9109x_c",
    "icg9110x_c", "icg9111x_c", "icg9112x_c", "icg9113x_c",
    "icg9114x_c", "icg9116x_c", "icg9117s_c", "icg9118x_c",
    "icg9119x_c", "icg9121x_c", "icg9122x_c", "icg9123x_c",
    "icg9124x_c", "icg9125s_c", "icg9126x_c", "icg9127x_c",
    "icg9128x_c", "icg9129x_c", "icg9130x_c", "icg9131x_c",
    "icg9132x_c", "icg9133s_c", "icg9134x_c", "icg9135x_c",
    "icg9136s_c", "icg9137x_c", "icg9138x_c", "icg9140s_c"
  ),
  w7 = c(
          "icg12018s_c", "ica5003x_c", "icg12107s_c", "icg12004s_c",
          "icg12010x_c", "icg12011x_c", "ica5008x_c", "icg12060s_c",
          "icg12013s_c", "icg12016s_c", "ica5019x_c",
          "icg12121x_c", "icg12028s_c", "ica5023x_c", "ica5027x_c",
          "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
          "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
          "icg12046s_c", "ica5021s_c", "ica5052s_c", "icg12048s_c",
          "icg12050s_c", "icg12054s_c", "icg12109s_c", "icg12119s_c"
    # "icg12018s_c", "ica5003x_sc4g12_c", "icg12107s_c", "icg12004s_c",
    # "icg12010x_c", "icg12011x_c", "ica5008x_sc4g12_c", "icg12060s_c",
    # "icg12013s_c", "icg12016s_c", "ica5019x_sc4g12_c",
    # "icg12121x_c", "icg12028s_c", "ica5023x_sc4g12_c", "ica5027x_sc4g12_c",
    # "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
    # "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
    # "icg12046s_c", "ica5021s_sc4g12_c", "ica5052s_sc4g12_c", "icg12048s_c",
    # "icg12050s_c", "icg12054s_c", "icg12109s_c", "icg12119s_c"
  ),
  w13 = c()
)
x4w1 <- cbind(item = 1:length(items$w1),
              xsi = c(0.82, -1.64, 0.92, 1.12, -1.17, -0.47, -1.86, -1.60, 0.33,
                      0.42, -1.00, 0.68, -0.79, -1.59, -1.55, -0.23, -0.87,
                      -0.57, 0.03, -0.85, -2.05, -1.73, -0.56, -0.71, 0.07,
                      1.56, -0.23, -0.44, -0.77, -1.29, -1.63, -0.06, -0.84,
                      -0.62, -0.97, -1.72))
rownames(x4w1) <- items$w1
colnames(x4w1)[1] <- ""
x4w1[, 2][rownames(x4w1) %in% pcitems("SC4", "IC", "w1")] <-
  x4w1[, 2][rownames(x4w1) %in% pcitems("SC4", "IC", "w1")]/2
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-1.164, 1.320, -0.587, -0.124, 0.041, 1.323, -0.850,
                      -1.512, -1.308, -0.365, 1.124, 0.430, -1.359, 0.109,
                      0.100, -0.443, -0.889, -0.082, 0.258, -0.342, -1.584,
                      -0.287, -0.719, -0.533, -2.191, -0.285, -1.010, -0.928,
                      -0.713, -0.826, -0.840))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
x4w7[, 2][rownames(x4w7) %in% pcitems("SC4", "IC", "w7")] <-
  x4w7[, 2][rownames(x4w7) %in% pcitems("SC4", "IC", "w7")]/2
x4w13 <- cbind(item = 1:length(items$w13),
              xsi = c())
rownames(x4w13) <- items$w13
colnames(x4w13)[1] <- ""

# SC5
items <- c(
  "ics3001x_c", "ics3002s_c", "ics3003x_c", "ics3010x_c", "ics3011x_c",
  "ics3012x_c", "ics3014x_c", "ics3024x_c", "ics3015x_c", "ics3031x_c",
  "ics3038x_c", "ics3019x_c", "ics3023x_c", "ics3028x_c", "ics3029s_c",
  "ics3048x_c", "ics3049s_c", "ics3018s_c", "ics3041x_c", "ics3043x_c",
  "ics3030x_c", "ics3032x_c", "ics3033x_c", "ics3034x_c", "ics3045x_c",
  "ics3035s_c", "ics3037x_c", "ics3042x_c", "ics3044x_c", "ics3047x_c"
)
x5w5 <- cbind(item = 1:length(items),
              xsi = c(
                1.40366579, -0.86991232, -1.20855800, -0.83436944, -0.90459723,
                -0.37176353, -0.32118536, -0.62767290, -0.56493133, -1.70029390,
                1.98597554, -0.79915894, -1.62065130, 0.41288635, -0.41379726,
                -0.06746113, -0.05061254, -1.32526649, -2.80908060, -0.35641518,
                1.30212156, 0.44628818, -0.54315593, 0.49712400, -0.51068062,
                -0.29597091, 0.86393283, -1.07652203, 0.35764623, -0.85212525
              ) # NEPSscaling SC5 IC without additional info. or rotation
              # don't know any more where those values come frome
              # no deviation in wle comparison -- leave the way it is
              )
rownames(x5w5) <- items
colnames(x5w5)[1] <- ""

# SC6
items <- c(
  "ica5001x_c", "ica5003x_c", "ica5005x_c", "ica5004s_c", "ica5006x_c",
  "ica5007x_c", "ica5008x_c", "ica5010x_c", "ica5017s_c", "ica5018s_c",
  "ica5015s_c", "ica5019x_c", "ica5016s_c", "ica5020s_c", "ica5023x_c",
  "ica5027x_c", "ica5026x_c", "ica5029x_c", "ica5028x_c", "ica5030x_c",
  "icg9119x_sc6a5_c", "ica5050s_c", "icg9122x_sc6a5_c", "ica5047s_c",
  "ica5046x_c", "ica5021s_c", "ica5052s_c", "ica5054x_c", "ica5057x_c"
)
x6w5 <- cbind(item = 1:length(items),
            xsi = c(
              -1.78, 1.04, 1.47, -1.82, -0.93, -0.68, -1.32, -0.01, -1.74, -0.64,
              -2.23, 0.31, -2.16, -2.22, 0.39, 0.35, 0.58, -1.72, -1.25, -0.96,
              -1.32, -2.47, -0.21, -2.10, -0.96, -1.80, -0.99, -1.00, -1.45
            ))
rownames(x6w5) <- items
colnames(x6w5)[1] <- ""

# store pre-scaled item parameters in list
xsi.fixed$cross[["IC"]] <-
  list(
    SC2 = list(
      w5 = x2w5
    ),
    SC3 = list(
      w2 = x3w2,
      w5 = x3w5,
      w9 = x3w9
    ),
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5w5),
    SC6 = list(w5 = x6w5)
  )
xsi.fixed$long[["IC"]] <-
  list(
    SC2 = list(
      w5 = x2w5
    ),
    SC3 = list(
      w2 = x3w2,
      w5 = x3w5,
      w9 = x3w9#long
    ),
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5w5),
    SC6 = list(w5 = x6w5)
  )


### ----------------------------------------------------------------------------
### Science competence
### ----------------------------------------------------------------------------

# SC1
items <- list(
        w6 = c(
        "sck10420_sc1n6_c", "scn6130s_c", #"scn66000_c",
        "sck16120_sc1n6_c", "sck1102s_sc1n6_c", "sck11030_sc1n6_c",
        "sck11110_sc1n6_c", "sck11120_sc1n6_c", "sck16010_sc1n6_c",
        "sck16020_sc1n6_c", "sck10510_sc1n6_c", "sck10530_sc1n6_c",
        "sck1162s_sc1n6_c", "sck10710_sc1n6_c", "sck10720_sc1n6_c",
        "scn60100_c", "sck11330_sc1n6_c", "sck10910_sc1n6_c",
        "scn61800_c", "sck16210_sc1n6_c"),
        w8 = c()
      )
x1w6 <- cbind(item = 1:length(items$w6),
              xsi = c(-1.852, -0.695, -2.219, 0.215, -0.433, -1.558,
                      -0.733, -2.022, 0.593, -1.535, -0.367, 0.163,
                      -1.300, -0.387, 0.605, -0.212, -0.714, -0.720,
                      -1.130))
rownames(x1w6) <- items$w6
colnames(x1w6)[1] <- ""
x1w6[, 2][rownames(x1w6) %in% pcitems("SC1", "SC", "w6")] <-
  x1w6[, 2][rownames(x1w6) %in% pcitems("SC1", "SC", "w6")]/2

# SC2
items <- list(
w1 = c("sck10420_c","sck10430_c","sck16120_c","sck16130_c",
"sck1102s_c","sck11030_c","sck1033s_c","sck10210_c",
"sck1023s_c","sck11110_c","sck11120_c","sck16010_c",
"sck16020_c","sck10510_c","sck10530_c","sck11610_c",
"sck1162s_c","sck10710_c","sck10720_c","sck11310_c",
"sck11330_c","sck10910_c","sck10920_c","sck16210_c",
"sck16220_c"),
w3 = c("scg10820_c","scg10840_c","scg11510_c","scg10650_c",
"scg16510_c","scg1652s_c","scg16110_c","scg1091s_c",
"scg10920_c","scg1011s_c","scg10120_c","scg11210_c",
"scg11110_c","scg11130_c","scg16530_c","scg16020_c",
"scg16030_c","scg11610_c","scg11710_c","scg10310_c",
"scg10520_c","scg16310_c","scg16220_c","scg11440_c",
"scg10410_c"),
w5 = c("scg30109_c","scg33510_c","scg3181s_c","scg37110_c",
"scg31010_c","scg36710_c","scg3131s_c","scg34010_c",
"scg32220_c","scg33710_c","scg36210_c","scg36920_c",
"scg32620_c","scg31510_c","scg30310_c","scg3641s_c",
"scg30520_c","scg37410_c","scg33310_c","scg3091s_c",
"scg33610_c","scg32910_c")
)
x2w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-1.481, -0.515, -1.749, -0.568, -1.337, 0.026,
                      -2.061, -2.369, -2.435, -1.181, -0.185, -0.935,
                      0.758, -1.487, -0.717, -2.473, -0.652, -1.683,
                      -0.703, 0.926, -0.931, -0.270, -1.994, -0.388,
                      0.923))
rownames(x2w1) <- items$w1
colnames(x2w1)[1] <- ""
x2w1[, 2][rownames(x2w1) %in% pcitems("SC2", "SC", "w1")] <-
  x2w1[, 2][rownames(x2w1) %in% pcitems("SC2", "SC", "w1")]/2
x2w3 <- cbind(item = 1:length(items$w3),
              xsi = c(-1.000, -0.141, 0.870, -2.064, 0.579, 0.230,
                      -0.763, -0.601, -0.684, 0.017, -0.184, -0.766,
                      -1.343, -0.697, 1.396, 1.895, -0.209, 0.304,
                      1.164, 0.296, -0.944, -0.957, -0.298, 1.009,
                      -0.375))
rownames(x2w3) <- items$w3
colnames(x2w3)[1] <- ""
x2w3[, 2][rownames(x2w3) %in% pcitems("SC2", "SC", "w3")] <-
  x2w3[, 2][rownames(x2w3) %in% pcitems("SC2", "SC", "w3")]/2
x2w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-1.130, -1.038, -0.362, -0.371, -0.832, -0.043,
                      -0.354, 0.031, 0.409, -0.374, 1.556, 0.072,
                      -0.758, 0.569, 0.609, -1.194, 1.682, -0.054,
                      -0.803, -0.585, -0.466, 0.594))
rownames(x2w5) <- items$w5
colnames(x2w5)[1] <- ""
x2w5[, 2][rownames(x2w5) %in% pcitems("SC2", "SC", "w5")] <-
  x2w5[, 2][rownames(x2w5) %in% pcitems("SC2", "SC", "w5")]/2


# SC3
items <- list(
  w2 = c("scg6103s_c","scg61050_c","scg60120_c",
         "scg60410_c","scg60430_c","scg66310_c","scg66320_c",
         "scg66340_c","scg61410_c","scg6142s_c","scg61430_c",
         "scg6144s_c","scg60510_c","scg60530_c","scg6661s_c",
         "scg66620_c","scg66630_c","scg6664s_c","scg6111s_c",
         "scg6113s_c","scg66040_c","scg61310_c",
         "scg61330_c","scg6061s_c","scg60620_c"),
  w5 = c("scg90110_sc3g9_c","scg9012s_sc3g9_c","scg90510_sc3g9_c",
         "scg9052s_sc3g9_c","scg90920_sc3g9_c","scg90930_sc3g9_c",
         "scg9611s_sc3g9_c","scg96120_sc3g9_c","scg96410_sc3g9_c",
         "scg96420_sc3g9_c","scg9061s_sc3g9_c","scg90630_sc3g9_c",
         "scg90810_sc3g9_c","scg9083s_sc3g9_c","scg91030_sc3g9_c",
         "scg91040_sc3g9_c","scg91050_sc3g9_c","scg9042s_sc3g9_c",
         "scg9043s_sc3g9_c","scg9651s_sc3g9_c","scg96530_sc3g9_c",
         "scg90320_sc3g9_c","scg90330_sc3g9_c","scg9621s_sc3g9_c",
         "scg96220_sc3g9_c","scg91110_sc3g9_c","scg91120_sc3g9_c",
         "scg91130_sc3g9_c","scg97410_sc3g9_c","scg9771s_c",
         #"scg116320_sc3g9_c",
         "scg98910_c","scg9751s_c","scg9752s_c",
         "scg98010_c","scg97910_c","scg98210_c","scg98310_c"#,
         #"scg116210_sc3g9_c"
  ),
  w8 = c("scg116420_sc3g11_c", "scg110620_sc3g11_c", "scg110630_sc3g11_c",
         "scg11012s_sc3g11_c", "scg11083s_sc3g11_c", "scg110720_sc3g11_c",
         "scg11032s_sc3g11_c", "scg110330_sc3g11_c", "scg116510_sc3g11_c",
         "scg11652s_sc3g11_c", "scs56320_sc3g11_c", "scg110510_sc3g11_c",
         "scg110520_sc3g11_c", "scg110540_sc3g11_c", "scg11123s_sc3g11_c",
         "scg11102s_sc3g11_c", "scg11021s_sc3g11_c", "scg11022s_sc3g11_c",
         "scg11112s_sc3g11_c", "scg116210_sc3g11_c", "scg11622s_sc3g11_c",
         "scg116320_sc3g11_c", "scg110930_sc3g11_c", "scs5131s_sc3g11_c",
         "scs5132s_sc3g11_c")
)
x3w2 <- cbind(item = 1:length(items$w2),
              xsi = c(0.103, -0.736, -1.065, -0.945, -1.237, -0.140, -1.059,
                      -0.780, -1.923, 0.069, -1.135, -0.299, -2.185, -1.989,
                      -0.721, -0.906, -0.710, -0.190, -1.311, -1.448, -1.377,
                      -0.268, 0.460, -0.283, -0.931))
rownames(x3w2) <- items$w2
colnames(x3w2)[1] <- ""
x3w2[, 2][rownames(x3w2) %in% pcitems("SC3", "SC", "w2")] <-
  x3w2[, 2][rownames(x3w2) %in% pcitems("SC3", "SC", "w2")]/2
load("data-raw/item_difficulty_SC3_SC_w5.RData")
x3w5 <- item_difficulty_SC3_SC_w5
rm(item_difficulty_SC3_SC_w5)
load("data-raw/item_difficulty_SC3_SC_w5_long.RData")
x3w5long <- item_difficulty_SC3_SC_w5_long
rm(item_difficulty_SC3_SC_w5_long)
load("data-raw/item_difficulty_SC3_SC_w8.RData")
x3w8 <- item_difficulty_SC3_SC_w8
rm(item_difficulty_SC3_SC_w8)

# SC4
items <- list(
  w1 = c(
    "scg90110_c", "scg9012s_c", "scg90510_c",
    "scg9052s_c", "scg90920_c", "scg90930_c",
    "scg9611s_c", "scg96120_c", "scg96410_c",
    "scg96420_c", "scg9061s_c", "scg90630_c",
    "scg90810_c", "scg9083s_c", "scg91030_c",
    "scg91040_c", "scg91050_c", "scg9042s_c",
    "scg9043s_c", "scg9651s_c", "scg96530_c",
    "scg90320_c", "scg90330_c", "scg9621s_c",
    "scg96220_c", "scg91110_c", "scg91120_c",
    "scg91130_c"
  ),
  w5 = c(
    "scg116420_c", "scg110620_c", "scg110630_c", "scg11012s_c",
    "scg11083s_c", "scg110720_c", "scg11032s_c", "scg110330_c",
    "scg116510_c", "scg11652s_c", "scg11602s_c", "scg110510_c",
    "scg110520_c", "scg110540_c", "scg11123s_c", "scg11102s_c",
    "scg11021s_c", "scg11022s_c", "scg11112s_c", "scg116210_c",
    "scg11622s_c", "scg116320_c", "scg110930_c",
    "scs5131s_sc4g11_c"
  ),
  w13 = c() #TODO
)
x4w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-0.608, -2.103, -0.641, -0.743, -0.099, -1.027, -1.673,
                      -0.912, -1.752, -0.236, 0.346, -0.789, -2.199, -1.558,
                      0.030, -1.369, -0.913, -2.478, -1.264, -1.150, -0.319,
                      -0.553, 0.401, -1.215, -0.320, 0.149, 1.160, 0.835))
rownames(x4w1) <- items$w1
colnames(x4w1)[1] <- ""
x4w1[, 2][rownames(x4w1) %in% unlist(pcitems("SC4", "SC", "w1"))] <-
  x4w1[, 2][rownames(x4w1) %in% unlist(pcitems("SC4", "SC", "w1"))]/2
x4w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-0.391, 0.087, 0.171, 0.390, -0.879, -0.528, -1.170,
                      -0.273, -0.608, -0.147, -0.661, -0.407, -0.654, 1.516,
                      -0.406, 1.403, 1.329, 1.895, -0.678, 0.024, 1.458, 0.619,
                      -0.618, 0.031))
rownames(x4w5) <- items$w5
colnames(x4w5)[1] <- ""
x4w5[, 2][rownames(x4w5) %in% pcitems("SC4", "SC", "w5")] <-
  x4w5[, 2][rownames(x4w5) %in% pcitems("SC4", "SC", "w5")]/2
x4w13 <- cbind(item = 1:length(items$w13),
              xsi = c(rep(0, length(items$w13))))#TODO
rownames(x4w13) <- items$w13
colnames(x4w13)[1] <- ""

# SC5
items <- c(
  "scs36310_c", "scs36320_c", "scs36220_c", "scs3623s_c", "scs30510_c",
  "scs30520_c", "scs31210_c", "scs31220_c", "scs31240_c", "scs30920_c",
  "scs30930_c", "scs30940_c", "scs3021s_c", "scs3022s_c", "scs36020_c",
  "scs3643s_c", "scs3642s_c", "scs3031s_c", "scs3033s_c", "scs3112s_c",
  "scs3131s_c", "scs3132s_c", "scs3133s_c", "scs3012s_c", "scs30130_c",
  "scs3061s_c", "scs30630_c", "scs30640_c", "scs30810_c"
)
x5w5 <- cbind(item = 1:length(items),
              xsi = c(
                0.92237716, 1.46643444, -0.75800539, -0.13650864, -1.07382160,
                -0.46738747, -0.80254731, -0.18597379, -0.57964106, 0.11567871,
                -0.13176793, 0.25520290, -0.57676871, -0.22201618, 0.10421985,
                -0.24081756, -0.61244643, -0.79890169, 0.02618897, -0.42064414,
                -0.39688836, -0.45444353, -0.64151218, -0.62774150, 0.54444123,
                -0.75949384, -0.20641310, 0.12653751, -0.02551815
              ) # NEPSscaling SC5 SC without additional info. or rotation
              # don't know any more where those values come from
              # no deviations in wle comparison -- leave like that
)
rownames(x5w5) <- items
colnames(x5w5)[1] <- ""

# SC6
items <- c(
  "sca56120_c", "sca56130_c", "sca51110_c", "sca51140_c", "sca50410_c",
  "sca5652s_c", "sca56540_c", "sca51430_c", "sca51440_c", "sca50210_c",
  "sca50220_c", "sca50710_c", "sca50720_c", "sca56310_c", "sca56320_c",
  "sca5091s_c", "sca56020_c", "sca56030_c", "sca50520_c", "sca50530_c",
  "sca51020_c", "sca51030_c"
)
x6w5 <- cbind(item = 1:length(items),
              xsi = c(
                0.633, -1.601, -1.774, -1.407, -1.462, -1.634, -0.100, -1.758, -0.628,
                -0.585, -1.073, -1.377, 0.757, -1.306, 0.769, -1.697, -0.984, -2.008,
                -0.501, 0.416, 1.276, -1.721
              ))
rownames(x6w5) <- items
colnames(x6w5)[1] <- ""


# store pre-scaled item parameters in list
xsi.fixed$cross[["SC"]] <-
  list(
    SC1 = list(
      w6 = x1w6
    ),
    SC2 = list(
      w1 = x2w1,
      w3 = x2w3,
      w5 = x2w5
    ),
    SC3 = list(
      w2 = x3w2,
      w5 = x3w5,
      w8 = x3w8
    ),
    SC4 = list(
      w1 = x4w1,
      w5 = x4w5,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5w5),
    SC6 = list(w5 = x6w5)
  )
xsi.fixed$long[["SC"]] <-
  list(
    SC1 = list(
      w6 = x1w6
    ),
    SC2 = list(
      w1 = x2w1,
      w3 = x2w3,
      w5 = x2w5
    ),
    SC3 = list(
      w2 = x3w2,
      w5 = x3w5long,
      w8 = x3w8
    ),
    SC4 = list(
      w1 = x4w1,
      w5 = x4w5,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5w5),
    SC6 = list(w5 = x6w5)
  )

### ----------------------------------------------------------------------------
### English as a foreign language
### ----------------------------------------------------------------------------

# SC3
items <- list(
  w7 = c("efg10022s_sc3g10_c","efg10108s_sc3g10_c","efg10094s_sc3g10_c",
         "efg10059s_sc3g10_c","efg10002s_sc3g10_c","efg10008s_sc3g10_c",
         "efg10098s_sc3g10_c","efg10065a_sc3g10_c","efg10065b_sc3g10_c",
         #"efg10065c_sc3g10_c",
         "efg10065d_sc3g10_c","efg10075s_sc3g10_c",
         "efg10057a_sc3g10_c"),
  w9 = c("efg10022s_sc3g12_c", "efg12b00s_sc3g12_c", "efg10108s_sc3g12_c",
         "efg12d001_sc3g12_c", "efg12d002_sc3g12_c", "efg12d003_sc3g12_c",
         "efg12d004_sc3g12_c", "efg12d005_sc3g12_c")
)
scoring <- rep(1, length(items$w7))
scoring[grepl("s_", items$w7)] <- 0.5
x3w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-0.35, -0.82, -0.74, 0.05, -1.20, 0.10, 0.01, -0.26,
                      -0.83, -0.18, 0.09, -0.83) - # subtracted in original scaling
                  NEPSscaling:::link_constant$SC3$EF[["w7"]] * scoring)
rownames(x3w7) <- items$w7
colnames(x3w7)[1] <- ""
rm(scoring)
x3w9 <- cbind(item = 1:length(items$w9),
              xsi = c(-0.87, -0.21, -1.15, -0.62, 0.20, 1.82, -0.89, 0.21))
rownames(x3w9) <- items$w9
colnames(x3w9)[1] <- ""
# x3w9 <- x3w9[order(rownames(x3w9)), ]
# x3w9[, 1] <- 1:nrow(x3w9)
# Link via setting the common items in g12 to g10 values, not mean/mean!
x3w9long <- cbind(item = 1:length(items$w9),
                  xsi = c(-0.25, -0.21, -0.72, -0.62, 0.20, 1.82, -0.89, 0.21))
rownames(x3w9long) <- items$w9
colnames(x3w9long)[1] <- ""
### ! restricting the step parameters works via AXsi for WLEs, not for tam.mml
###   via xsi.fixed

# SC4
items <- list(
  w3 = c(
    "efg10022s_c", "efg10108s_c", "efg10094s_c", "efg10059s_c",
    "efg10002s_c", "efg10008s_c", "efg10098s_c", "efg10065a_c",
    "efg10065b_c", # "efg10065c_c",
    "efg10065d_c", "efg10075s_c",
    "efg10057a_c"
  ),
  w7 = c(
    "efg10022s_sc4g12_c", "efg12b00s_c", "efg10108s_sc4g12_c",
    "efg12d001_c", "efg12d002_c", "efg12d003_c", "efg12d004_c",
    "efg12d005_c"
  )
)
x4w3 <- cbind(item = 1:length(items$w3),
              xsi = c(-0.25, -0.77, -0.63, 0.24, -0.77, 0.37, 0.20, -0.19,
                      -0.66, -0.32, 0.17, -0.80))
rownames(x4w3) <- items$w3
colnames(x4w3)[1] <- ""
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-0.85, -0.08, -1.12, -0.40, 0.37, 2.53, -0.66, 0.43))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
# Link via setting the common items in g12 to g10 values, not mean/mean!
x4w7long <- cbind(item = 1:length(items$w7),
              xsi = c(-0.25, -0.08, -0.77, -0.40, 0.37, 2.53, -0.66, 0.43))
rownames(x4w7long) <- items$w7
colnames(x4w7long)[1] <- ""

# SC5

items <- c(
  "efs121010_c", "efs121020_c", "efs121030_c", "efs121040_c",
  "efs121050_c", "efs121060_c", "efs121070_c", "efs121080_c",
  "efs121090_c", "efs121100_c", "efs122010_c", "efs122020_c",
  "efs122030_c", "efs122040_c", "efs122050_c", "efs123011_c",
  "efs123012_c", "efs124010_c", "efs12402s_c", "efs124030_c",
  "efs125010_c", "efs125020_c", "efs125030_c"
)
x5w12 <- cbind(item = 1:length(items),
               xsi = c(
                 -1.18, -0.25, -2.51, -2.02, -0.88, -1.63, -2.26, -2.35, -2.24,
                 -0.21, -2.37, -1.33, -1.28, -0.32, -0.84, -1.37, 0.25, -1.12,
                 -0.24, -0.67, 0.27, -1.06, -0.32
               ))
rownames(x5w12) <- items
colnames(x5w12)[1] <- ""

# store pre-scaled item parameters in list
xsi.fixed$cross[["EF"]] <-
  list(
    SC3 = list(
      w7 = x3w7,
      w9 = x3w9
    ),
    SC4 = list(
      w3 = x4w3,
      w7 = x4w7
    ),
    SC5 = list(w12 = x5w12)
  )
xsi.fixed$long[["EF"]] <-
  list(
    SC3 = list(
      w7 = x3w7,
      w9 = x3w9long
    ),
    SC4 = list(
      w3 = x4w3,
      w7 = x4w7long
    ),
    SC5 = list(w12 = x5w12)
  )

### ----------------------------------------------------------------------------
### Business Administration competence
### ----------------------------------------------------------------------------

# SC5

items <- c(
  "bas7mar1_c", "bas7mar2_c", "bas7mar3_c", "bas7mar4_c", "bas7mar5_c",
  "bas7mar6_c", "bas7org1_c", "bas7org2_c", "bas7org3_c", "bas7org4_c",
  "bas7org5_c", "bas7org6_c", "bas7fin1_c", "bas7fin2_c", "bas7fin3_c",
  "bas7fin4_c", "bas7fin5_c", "bas7fin6_c", "bas7acc1_c", "bas7acc2_c",
  "bas7acc3_c", "bas7acc4_c", "bas7acc5_c", "bas7acc6_c", "bas7mic1_c",
  "bas7mic2_c", "bas7mic3_c", "bas7mic4_c", "bas7mic5_c", # "bas7mic6_c",
  # "bas7mac1_c",
  "bas7mac2_c", "bas7mac3_c", "bas7mac4_c", "bas7mac5_c",
  "bas7mac6_c"
)
x5w7 <- cbind(item = 1:length(items),
            xsi = c(
              -1.74, -.21, -1.10, -.87, -.09, -.79, -1.33, -.32, -.74, -.95,
              -1.36, -.23, -1.45, .11, -.14, -1.84, 1.07, -.36, -1.23, .08,
              -1.72, -.88, .30, -.52, -.82, -.04, .38, .56, .45, # .69#, .62,
              .63, -1.05, -.17, .52, -.27
            ))
rownames(x5w7) <- items
colnames(x5w7)[1] <- ""

xsi.fixed$cross[["BA"]][["SC5"]][["w7"]] <- x5w7

### ----------------------------------------------------------------------------
### Listening comprehension Russian
### ----------------------------------------------------------------------------

# SC2
load("data-raw/item_difficulty_SC2_NR_w4.RData")
x2w4 <- item_difficulty_SC2_NR_w4
rm(item_difficulty_SC2_NR_w4)

# SC3
load("data-raw/item_difficulty_SC3_NR_w3.RData")
x3w3 <- item_difficulty_SC3_NR_w3
load("data-raw/item_difficulty_SC3_NR_w6.RData")
x3w6 <- item_difficulty_SC3_NR_w6
rm(item_difficulty_SC3_NR_w3, item_difficulty_SC3_NR_w6)

# SC4

items <- c(
  "nrg90101_c", "nrg90102_c", "nrg90103_c", "nrg90201_c",
  "nrg90202_c", "nrg90203_c", "nrg90301_c", "nrg90302_c",
  "nrg90303_c", "nrg90304_c", "nrg90401_c", "nrg90402_c",
  "nrg90403_c", "nrg90404_c", "nrg90405_c", "nrg90502_c",
  "nrg90503_c", "nrg90504_c", "nrg90505_c", "nrg90506_c",
  "nrg90601_c", "nrg90602_c", "nrg90603_c", "nrg90604_c",
  "nrg90605_c", "nrg90701_c", "nrg90702_c", "nrg90703_c",
  "nrg90704_c", "nrg90705_c", "nrg90706_c"
)
x4w2 <- cbind(item = 1:length(items),
            xsi = c(-1.48, 0.86, -0.29, -1.23, -0.51, -0.37, 0.01, -0.66,
                    0.89, -0.13, -0.89, -0.02, 0.76, 0.55, -0.10, -0.28,
                    -0.73, -0.16, 0.79, 0.16, -0.34, -0.18, -0.36, -0.44,
                    0.75, -0.56, 0.14, -0.55, -1.17, 1.41, 0.26))
rownames(x4w2) <- items
colnames(x4w2)[1] <- ""

xsi.fixed$cross[["NR"]] <- list(
  SC2 = list(
    w4 = x2w4
  ),
  SC3 = list(
    w3 = x3w3,
    w6 = x3w6
  ),
  SC4 = list(w2 = x4w2)
)
xsi.fixed$long[["NR"]] <- list(
  SC2 = list(
    w4 = x2w4
  ),
  SC3 = list(
    w3 = x3w3,
    w6 = x3w6
  ),
  SC4 = list(w2 = x4w2)
)

### ----------------------------------------------------------------------------
### Listening comprehension Turkish
### ----------------------------------------------------------------------------

# SC2
load("data-raw/item_difficulty_SC2_NT_w4.RData")
x2w4 <- item_difficulty_SC2_NT_w4
rm(item_difficulty_SC2_NT_w4)

# SC3
load("data-raw/item_difficulty_SC3_NT_w3.RData")
x3w3 <- item_difficulty_SC3_NT_w3
load("data-raw/item_difficulty_SC3_NT_w6.RData")
x3w6 <- item_difficulty_SC3_NT_w6
rm(item_difficulty_SC3_NT_w3, item_difficulty_SC3_NT_w6)

# SC4

items <- c(
  "ntg90101_c", "ntg90102_c", "ntg90103_c", "ntg90201_c",
  "ntg90202_c", "ntg90203_c", "ntg90301_c", "ntg90302_c",
  "ntg90303_c", "ntg90304_c", "ntg90401_c", "ntg90402_c",
  "ntg90403_c", "ntg90404_c", "ntg90405_c", "ntg90502_c",
  "ntg90503_c", "ntg90504_c", "ntg90505_c", "ntg90506_c",
  "ntg90601_c", "ntg90602_c", "ntg90603_c", "ntg90604_c",
  "ntg90605_c", "ntg90701_c", "ntg90702_c", "ntg90703_c",
  "ntg90704_c", "ntg90705_c", "ntg90706_c"
)
x4w2 <- cbind(item = 1:length(items),
            xsi = c(-1.46, -0.90, -0.53, -1.78, -0.28, -0.82, -0.41, -0.01,
                    0.06, 0.39, -0.96, 0.01, 0.71, 0.86, -0.15, -0.37, -0.13,
                    -1.33, 0.95, -0.31, -1.02, 0.02, -0.15, -0.34, 1.30,
                    -0.05, 0.02, -0.44, -0.22, 0.29, -0.03))
rownames(x4w2) <- items
colnames(x4w2)[1] <- ""

xsi.fixed$cross[["NT"]] <- list(
  SC2 = list(
    w4 = x2w4
  ),
  SC3 = list(
    w3 = x3w3,
    w6 = x3w6
  ),
  SC4 = list(w2 = x4w2)
)
xsi.fixed$long[["NT"]] <- list(
  SC2 = list(
    w4 = x2w4
  ),
  SC3 = list(
    w3 = x3w3,
    w6 = x3w6
  ),
  SC4 = list(w2 = x4w2)
)

### ----------------------------------------------------------------------------
### Scientific thinking
### ----------------------------------------------------------------------------

# SC3

items <- c(
  "stg12nhs_sc3g12_c", "stg12egs_sc3g12_c", "stg12mts_sc3g12_c",
  "stg12cws_sc3g12_c", "stg12pds_sc3g12_c"
)
x3w9 <- cbind(item = 1:length(items),
            xsi = c(-0.24, 0.12, -0.19, -0.34, -0.30))
rownames(x3w9) <- items
colnames(x3w9)[1] <- ""

xsi.fixed$cross[["ST"]][["SC3"]][["w9"]] <- x3w9

# SC4

items <- c(
  "stg12nhs_c", "stg12egs_c", "stg12mts_c", "stg12cws_c", "stg12pds_c"
)
x4w7 <- cbind(item = 1:length(items),
            xsi = c(-0.24, 0.12, -0.19, -0.34, -0.30))
rownames(x4w7) <- items
colnames(x4w7)[1] <- ""

xsi.fixed$cross[["ST"]][["SC4"]][["w7"]] <- x4w7

### ----------------------------------------------------------------------------
### Listening comprehension German
### ----------------------------------------------------------------------------

# SC3
items <- c("lig9011s_c","lig9012s_c","lig9013s_c",
           "lig9014s_c","lig9015s_c","lig9016s_c",
           "lig9017s_c","lig9018s_c","lig9021s_c",
           "lig9022s_c","lig9023s_c","lig9024s_c",
           "lig9025s_c","lig9026s_c","lig9027s_c",
           "lig9028s_c")
x3w6 <- cbind(item = 1:length(items),
            xsi = c(-0.659, -1.451, -1.092, -2.207, -2.116, -1.997, -1.612,
                    -1.332, -0.194, -1.721, -2.620, -1.572, -2.113, -1.957,
                    -1.693, -2.654))
rownames(x3w6) <- items
colnames(x3w6)[1] <- ""
x3w6[, 2][rownames(x3w6) %in% pcitems("SC3", "LI", "w6")] <-
  x3w6[, 2][rownames(x3w6) %in% pcitems("SC3", "LI", "w6")]/2

xsi.fixed$cross[["LI"]][["SC3"]][["w6"]] <- x3w6


### ----------------------------------------------------------------------------
### Orthography: A - whole word, B - structural unit
### ----------------------------------------------------------------------------

# SC2
items <- list(
A = list(
w6 = c("org41001_c","org41002_c","org41003_c","org41004_c",
"org41005_c","org41006_c","org41007_c","org41008_c",
"org41009_c","org41010_c","org41011_c","org41012_c",
"org41013_c","org41014_c","org41015_c","org41016_c",
"org41017_c","org41018_c","org41019_c","org41020_c",
"org41021_c","org41022_c","org41023_c","org41024_c",
"org41025_c","org41026_c","org41027_c","org41028_c",
"org41029_c","org41030_c","org41031_c","org41032_c",
"org41033_c","org41034_c","org41035_c","org41036_c",
"org41037_c")
),
B = list(
w6 = c("org42001_c","org42002_c","org42003_c","org42004_c",
"org42005_c","org42006_c","org42007_c","org42008_c",
"org42009_c","org42010_c","org42011_c","org42012_c",
"org42013_c","org42014_c","org42015_c","org42016_c",
"org42017_c","org42018_c","org42019_c","org42020_c",
"org42021_c","org42022_c","org42023_c","org42024_c",
"org42025_c","org42026_c","org42027_c","org42028_c",
"org42029_c","org42030_c","org42031_c","org42032_c",
"org42033_c","org42034_c","org42035_c","org42036_c",
"org42037_c","org42038_c","org42039_c","org42040_c",
"org42041_c","org42042_c","org42043_c","org42044_c",
"org42045_c","org42046_c","org42047_c","org42048_c",
"org42049_c","org42050_c","org42051_c","org42052_c",
"org42053_c","org42054_c","org42055_c","org42056_c",
"org42057_c","org42058_c","org42059_c","org42060_c",
"org42061_c","org42062_c","org42063_c","org42064_c",
"org42065_c","org42066_c","org42067_c","org42068_c",
"org42069_c","org42070_c","org42071_c","org42072_c",
"org42073_c","org42074_c","org42075_c","org42076_c",
"org42077_c","org42078_c","org42079_c","org42080_c",
"org42081_c","org42082_c","org42083_c","org42084_c",
"org42085_c","org42086_c","org42087_c","org42088_c",
"org42089_c","org42090_c","org42091_c","org42092_c",
"org42093_c","org42094_c","org42095_c","org42096_c",
"org42097_c","org42098_c","org42099_c","org42100_c",
"org42101_c","org42102_c","org42103_c","org42104_c",
"org42105_c","org42106_c","org42107_c","org42108_c",
"org42109_c","org42110_c","org42111_c","org42112_c",
"org42113_c","org42114_c","org42115_c","org42116_c",
"org42117_c","org42118_c","org42119_c","org42120_c",
"org42121_c","org42122_c","org42123_c","org42124_c",
"org42125_c","org42126_c","org42127_c","org42128_c",
"org42129_c","org42130_c")
)
)
x2w6a <- cbind(item = 1:length(items$A$w6),
               xsi = c(-3.400, -1.326, 1.543, 1.078, 1.875, 1.018,
                       0.257, -1.834, 1.016, 0.304, 2.555, 0.197,
                       1.120, 1.962, 0.110, -0.384, 1.890, 1.477,
                       -0.481, -1.069, -0.403, -1.969, 1.735,
                       -3.141, 2.920, -0.520, -1.388, 0.985, -0.706,
                       0.761, -0.997, 1.803, 0.768, -3.825, 1.051,
                       1.213, -1.976))
rownames(x2w6a) <- items$A$w6
colnames(x2w6a)[1] <- ""
x2w6b <- cbind(item = 1:length(items$B$w6),
               xsi = c(-4.382, -2.202, -4.121, -3.404, -1.388, -1.551,
                       -2.829, -2.393, -1.327, -1.373, -3.988, -1.470,
                       -1.369, 0.407, -3.570, -3.437, -0.898, -3.021,
                       -3.292, -2.313, -3.188, -4.702, -3.030, -3.924,
                       -2.029, -3.514, -3.397, -4.235, -3.837, -1.299,
                       -4.222, -2.213,
                       -1.832, -4.133, -2.786, 1.240, 0.323, -2.144,
                       -3.808, 0.258, -0.351, -0.739, -1.650, 0.439,
                       -4.459, 0.094, -0.493, -2.177, -1.136, -3.292,
                       -3.666, -0.499, -3.306, 0.678, -1.576, -3.494,
                       -3.169, -2.925, -1.869, 0.871, 1.063, -0.064,
                       -2.821, 1.351, -0.957, -0.052, -1.721, -1.945,
                       -1.123, -0.986, -1.713, 0.816, -3.303, -3.430,
                       0.258, -1.257, -3.010, -0.775, -1.371, -1.988,
                       -4.097, -3.327, -3.502, 0.773, 0.248, -3.574,
                       -3.114, -4.420, 0.615, -1.689, -4.276, -3.977,
                       -3.842, 0.824, -3.940, -1.642, -2.389, -2.844,
                       -2.439, -3.108, -1.030, -0.953, -3.136, -2.811,
                       -2.328, -3.365, -3.166, -3.013, -3.296, -3.929,
                       -4.016, -1.781, -3.249, -2.819, -2.031, -2.484,
                       -3.246, -1.966, -3.096, -2.330, -3.490, -2.755,
                       -3.093, -3.770, -2.909, -0.744, -1.546, -3.775,
                       -1.938, -1.860))
rownames(x2w6b) <- items$B$w6
colnames(x2w6b)[1] <- ""

# SC3
items <- list(
  A = list(
    w1 = c("org51001_c","org51002_c","org51003_c","org51004_c",
           "org51005_c","org51006_c","org51007_c","org51008_c",
           "org51009_c","org51010_c","org51011_c","org51012_c",
           "org51013_c","org51014_c","org51015_c","org51016_c",
           "org51017_c","org51018_c","org51019_c","org51020_c",
           "org51021_c","org51022_c","org51023_c","org51024_c",
           "org51025_c","org51026_c","org51027_c","org51028_c",
           "org51029_c","org51030_c","org51031_c","org51032_c",
           "org51033_c","org51034_c","org51035_c","org51036_c",
           "org51037_c","org51038_c","org51039_c","org51040_c",
           "org51041_c","org51042_c","org51043_c","org51044_c",
           "org51045_c","org51046_c","org51047_c","org51048_c",
           "org51049_c","org51050_c"),
    w3 = c("org51023_sc3g7_c","org51030_sc3g7_c","org51040_sc3g7_c",
           "org51003_sc3g7_c","org51015_sc3g7_c","org51011_sc3g7_c",
           "org51004_sc3g7_c","org51042_sc3g7_c","org51038_sc3g7_c",
           "org51037_sc3g7_c","org51041_sc3g7_c","org51033_sc3g7_c",
           "org51009_sc3g7_c","org51031_sc3g7_c","org51010_sc3g7_c",
           "org51032_sc3g7_c","org51035_sc3g7_c","org71001_c",
           "org71002_c","org71003_c","org71004_c","org71005_c",
           "org71006_c","org71007_c","org71008_c","org71009_c",
           "org71010_c","org71011_c","org71012_c","org71013_c",
           "org71014_c","org71015_c","org71016_c","org71017_c",
           "org71018_c","org71019_c","org71020_c","org71021_c",
           "org71022_c","org71023_c","org71024_c","org71025_c",
           "org71026_c","org71027_c","org71028_c","org71029_c",
           "org71030_c","org71031_c","org71032_c","org71033_c",
           "org71034_c","org71035_c","org71036_c","org71037_c",
           "org71038_c","org71039_c","org71040_c","org71041_c",
           "org71042_c","org71043_c","org71044_c","org71045_c",
           "org71046_c","org71047_c","org71048_c","org71049_c",
           "org71050_c","org71051_c","org71052_c","org71053_c",
           "org71054_c","org71055_c","org71056_c","org71057_c",
           "org71058_c","org71059_c","org71060_c","org71061_c"),
    w5 = c("org51030_sc3g9_c","org51040_sc3g9_c","org51011_sc3g9_c",
           "org51004_sc3g9_c","org51042_sc3g9_c","org51038_sc3g9_c",
           "org51037_sc3g9_c","org51009_sc3g9_c","org51031_sc3g9_c",
           "org51032_sc3g9_c","org71001_sc3g9_c","org71002_sc3g9_c",
           "org71003_sc3g9_c","org71004_sc3g9_c","org71005_sc3g9_c",
           "org71006_sc3g9_c","org71007_sc3g9_c","org71008_sc3g9_c",
           "org71009_sc3g9_c","org71010_sc3g9_c","org71011_sc3g9_c",
           "org71012_sc3g9_c","org71013_sc3g9_c","org71014_sc3g9_c",
           "org71015_sc3g9_c","org71016_sc3g9_c","org71017_sc3g9_c",
           "org71018_sc3g9_c","org71019_sc3g9_c","org71020_sc3g9_c",
           "org71021_sc3g9_c","org71022_sc3g9_c","org71023_sc3g9_c",
           "org71024_sc3g9_c","org71025_sc3g9_c","org71026_sc3g9_c",
           "org71027_sc3g9_c","org71028_sc3g9_c","org71029_sc3g9_c",
           "org71030_sc3g9_c","org71031_sc3g9_c","org71032_sc3g9_c",
           "org91001_c","org91002_c","org91003_c","org91004_c",
           "org91005_c","org91006_c","org91007_c","org91008_c",
           "org91009_c","org91010_c","org91011_c","org91012_c",
           "org91013_c","org91014_c","org91015_c","org91016_c",
           "org91017_c","org91018_c","org91019_c","org91020_c",
           "org91021_c","org91022_c","org91023_c","org91024_c",
           "org91025_c","org91026_c","org91027_c","org91028_c",
           "org91029_c","org91030_c","org91031_c","org91032_c",
           "org91033_c","org91034_c","org91035_c")
  ),
  B = list(
    w1 = c("org52001_c","org52002_c","org52003_c","org52004_c",
           "org52005_c","org52006_c","org52007_c","org52008_c",
           "org52009_c","org52010_c","org52011_c","org52012_c",
           "org52013_c","org52014_c","org52015_c","org52016_c",
           "org52017_c","org52018_c","org52019_c","org52020_c",
           "org52021_c","org52022_c","org52023_c","org52024_c",
           "org52025_c","org52026_c","org52027_c","org52028_c",
           "org52029_c","org52030_c","org52031_c","org52032_c",
           "org52033_c","org52034_c","org52035_c","org52036_c",
           "org52037_c","org52038_c","org52039_c","org52040_c",
           "org52041_c","org52042_c","org52043_c","org52044_c",
           "org52045_c","org52046_c","org52047_c","org52048_c",
           "org52049_c","org52050_c","org52051_c","org52052_c",
           "org52053_c","org52054_c","org52055_c","org52056_c",
           "org52057_c","org52058_c","org52059_c","org52060_c",
           "org52061_c","org52062_c","org52063_c","org52064_c",
           "org52065_c","org52066_c","org52067_c","org52068_c",
           "org52069_c","org52070_c","org52071_c","org52072_c",
           "org52073_c","org52074_c","org52075_c","org52076_c",
           "org52077_c","org52078_c","org52079_c","org52080_c",
           "org52081_c","org52082_c","org52083_c","org52084_c",
           "org52085_c","org52086_c","org52087_c","org52088_c",
           "org52089_c","org52090_c","org52091_c","org52092_c",
           "org52093_c","org52094_c","org52095_c","org52096_c",
           "org52097_c","org52098_c","org52099_c","org52100_c",
           "org52101_c","org52102_c","org52103_c","org52104_c",
           "org52105_c","org52106_c","org52107_c","org52108_c",
           "org52109_c","org52110_c","org52111_c","org52112_c",
           "org52113_c","org52114_c","org52115_c","org52116_c",
           "org52117_c","org52118_c","org52119_c","org52120_c",
           "org52121_c","org52122_c","org52123_c","org52124_c",
           "org52125_c","org52126_c","org52127_c","org52128_c",
           "org52129_c","org52130_c","org52131_c","org52132_c",
           "org52133_c","org52134_c","org52135_c","org52136_c",
           "org52137_c","org52138_c","org52139_c","org52140_c",
           "org52141_c","org52142_c","org52143_c","org52144_c",
           "org52145_c","org52146_c","org52147_c","org52148_c",
           "org52149_c","org52150_c","org52151_c","org52152_c",
           "org52153_c","org52154_c","org52155_c","org52156_c",
           "org52157_c","org52158_c","org52159_c","org52160_c",
           "org52161_c","org52162_c","org52163_c","org52164_c",
           "org52165_c","org52166_c","org52167_c","org52168_c",
           "org52169_c","org52170_c","org52171_c","org52172_c",
           "org52173_c","org52174_c","org52175_c","org52176_c",
           "org52177_c","org52178_c","org52179_c","org52180_c",
           "org52181_c"),
    w3 = c("org52060_sc3g7_c","org52163_sc3g7_c","org52016_sc3g7_c",
           "org52133_sc3g7_c","org52107_sc3g7_c","org52078_sc3g7_c",
           "org52175_sc3g7_c","org52168_sc3g7_c","org52139_sc3g7_c",
           "org52169_sc3g7_c","org52138_sc3g7_c","org52068_sc3g7_c",
           "org52109_sc3g7_c","org52073_sc3g7_c","org52032_sc3g7_c",
           "org52096_sc3g7_c","org52074_sc3g7_c","org52082_sc3g7_c",
           "org52153_sc3g7_c","org52116_sc3g7_c","org52086_sc3g7_c",
           "org52129_sc3g7_c","org52128_sc3g7_c","org52048_sc3g7_c",
           "org52125_sc3g7_c","org52007_sc3g7_c","org52126_sc3g7_c",
           "org52043_sc3g7_c","org52002_sc3g7_c","org52119_sc3g7_c",
           "org52178_sc3g7_c","org52148_sc3g7_c","org52033_sc3g7_c",
           "org52147_sc3g7_c","org52098_sc3g7_c","org52112_sc3g7_c",
           "org52059_sc3g7_c","org52029_sc3g7_c","org52072_sc3g7_c",
           "org52144_sc3g7_c","org52030_sc3g7_c","org52143_sc3g7_c",
           "org52111_sc3g7_c","org52176_sc3g7_c","org52071_sc3g7_c",
           "org52075_sc3g7_c","org52146_sc3g7_c","org52097_sc3g7_c",
           "org52093_sc3g7_c","org52069_sc3g7_c","org52070_sc3g7_c",
           "org52142_sc3g7_c","org52094_sc3g7_c","org52154_sc3g7_c",
           "org52083_sc3g7_c","org52006_sc3g7_c","org52170_sc3g7_c",
           "org52140_sc3g7_c","org52092_sc3g7_c","org52084_sc3g7_c",
           "org52123_sc3g7_c","org52025_sc3g7_c","org52171_sc3g7_c",
           "org52141_sc3g7_c","org52173_sc3g7_c","org52027_sc3g7_c",
           "org52028_sc3g7_c","org52026_sc3g7_c","org52110_sc3g7_c",
           "org72001_c","org72002_c","org72003_c","org72004_c",
           "org72005_c","org72006_c","org72007_c","org72008_c",
           "org72009_c","org72010_c","org72011_c","org72012_c",
           "org72013_c","org72014_c","org72015_c","org72016_c",
           "org72017_c","org72018_c","org72019_c","org72020_c",
           "org72021_c","org72022_c","org72023_c","org72024_c",
           "org72025_c","org72026_c","org72027_c","org72028_c",
           "org72029_c","org72030_c","org72031_c","org72032_c",
           "org72033_c","org72034_c","org72035_c","org72036_c",
           "org72037_c","org72038_c","org72039_c","org72040_c",
           "org72041_c","org72042_c","org72043_c","org72044_c",
           "org72045_c","org72046_c","org72047_c","org72048_c",
           "org72049_c","org72050_c","org72051_c","org72052_c",
           "org72053_c","org72054_c","org72055_c","org72056_c",
           "org72057_c","org72058_c","org72059_c","org72060_c",
           "org72061_c","org72062_c","org72063_c","org72064_c",
           "org72065_c","org72066_c","org72067_c","org72068_c",
           "org72069_c","org72070_c","org72071_c","org72072_c",
           "org72073_c","org72074_c","org72075_c","org72076_c",
           "org72077_c","org72078_c","org72079_c","org72080_c",
           "org72081_c","org72082_c","org72083_c","org72084_c",
           "org72085_c","org72086_c","org72087_c","org72088_c",
           "org72089_c","org72090_c","org72091_c","org72092_c",
           "org72093_c","org72094_c","org72095_c","org72096_c",
           "org72097_c","org72098_c","org72099_c","org72100_c",
           "org72101_c","org72102_c","org72103_c","org72104_c",
           "org72105_c","org72106_c","org72107_c","org72108_c",
           "org72109_c","org72110_c","org72111_c","org72112_c",
           "org72113_c","org72114_c","org72115_c","org72116_c",
           "org72117_c","org72118_c","org72119_c","org72120_c",
           "org72121_c","org72122_c","org72123_c","org72124_c",
           "org72125_c","org72126_c","org72127_c","org72128_c",
           "org72129_c","org72130_c","org72131_c","org72132_c",
           "org72133_c","org72134_c","org72135_c","org72136_c",
           "org72137_c","org72138_c","org72139_c","org72140_c",
           "org72141_c","org72142_c","org72143_c","org72144_c",
           "org72145_c","org72146_c","org72147_c","org72148_c",
           "org72149_c","org72150_c","org72151_c","org72152_c",
           "org72153_c","org72154_c","org72155_c","org72156_c",
           "org72157_c","org72158_c","org72159_c","org72160_c",
           "org72161_c","org72162_c","org72163_c","org72164_c",
           "org72165_c","org72166_c","org72167_c","org72168_c",
           "org72169_c","org72170_c","org72171_c","org72172_c",
           "org72173_c","org72174_c","org72175_c","org72176_c",
           "org72177_c","org72178_c","org72179_c","org72180_c",
           "org72181_c","org72182_c","org72183_c","org72184_c",
           "org72185_c","org72186_c","org72187_c","org72188_c",
           "org72189_c","org72190_c","org72191_c","org72192_c",
           "org72193_c","org72194_c","org72195_c","org72196_c",
           "org72197_c","org72198_c","org72199_c","org72200_c",
           "org72201_c","org72202_c","org72203_c","org72204_c",
           "org72205_c","org72206_c","org72207_c","org72208_c",
           "org72209_c","org72210_c","org72211_c","org72212_c",
           "org72213_c","org72214_c","org72215_c","org72216_c",
           "org72217_c","org72218_c","org72219_c","org72220_c",
           "org72221_c","org72222_c","org72223_c","org72224_c",
           "org72225_c","org72226_c","org72227_c","org72228_c",
           "org72229_c","org72230_c","org72231_c","org72232_c"),
    w5 = c("org52078_sc3g9_c","org52138_sc3g9_c","org52068_sc3g9_c",
           "org52048_sc3g9_c","org52126_sc3g9_c","org52043_sc3g9_c",
           "org52178_sc3g9_c","org52148_sc3g9_c","org52098_sc3g9_c",
           "org52112_sc3g9_c","org52029_sc3g9_c","org52144_sc3g9_c",
           "org52111_sc3g9_c","org52071_sc3g9_c","org52075_sc3g9_c",
           "org52097_sc3g9_c","org52070_sc3g9_c","org52142_sc3g9_c",
           "org52094_sc3g9_c","org52083_sc3g9_c","org52170_sc3g9_c",
           "org52140_sc3g9_c","org52025_sc3g9_c","org52171_sc3g9_c",
           "org72001_sc3g9_c","org72002_sc3g9_c","org72003_sc3g9_c",
           "org72004_sc3g9_c","org72005_sc3g9_c","org72006_sc3g9_c",
           "org72007_sc3g9_c","org72008_sc3g9_c","org72009_sc3g9_c",
           "org72010_sc3g9_c","org72011_sc3g9_c","org72012_sc3g9_c",
           "org72013_sc3g9_c","org72014_sc3g9_c","org72015_sc3g9_c",
           "org72016_sc3g9_c","org72017_sc3g9_c","org72018_sc3g9_c",
           "org72019_sc3g9_c","org72020_sc3g9_c","org72021_sc3g9_c",
           "org72022_sc3g9_c","org72023_sc3g9_c","org72024_sc3g9_c",
           "org72025_sc3g9_c","org72026_sc3g9_c","org72027_sc3g9_c",
           "org72028_sc3g9_c","org72029_sc3g9_c","org72030_sc3g9_c",
           "org72031_sc3g9_c","org72032_sc3g9_c","org72033_sc3g9_c",
           "org72034_sc3g9_c","org72035_sc3g9_c","org72036_sc3g9_c",
           "org72037_sc3g9_c","org72038_sc3g9_c","org72039_sc3g9_c",
           "org72040_sc3g9_c","org72041_sc3g9_c","org72042_sc3g9_c",
           "org72043_sc3g9_c","org72044_sc3g9_c","org72045_sc3g9_c",
           "org72046_sc3g9_c","org72047_sc3g9_c","org72048_sc3g9_c",
           "org72049_sc3g9_c","org72050_sc3g9_c","org72051_sc3g9_c",
           "org72052_sc3g9_c","org72053_sc3g9_c","org72054_sc3g9_c",
           "org72055_sc3g9_c","org72056_sc3g9_c","org72057_sc3g9_c",
           "org72058_sc3g9_c","org72059_sc3g9_c","org72060_sc3g9_c",
           "org72061_sc3g9_c","org72062_sc3g9_c","org72063_sc3g9_c",
           "org72064_sc3g9_c","org72065_sc3g9_c","org72066_sc3g9_c",
           "org72067_sc3g9_c","org72068_sc3g9_c","org72069_sc3g9_c",
           "org72070_sc3g9_c","org72071_sc3g9_c","org72072_sc3g9_c",
           "org72073_sc3g9_c","org72074_sc3g9_c","org72075_sc3g9_c",
           "org72076_sc3g9_c","org72077_sc3g9_c","org72078_sc3g9_c",
           "org72079_sc3g9_c","org72080_sc3g9_c","org72081_sc3g9_c",
           "org72082_sc3g9_c","org72083_sc3g9_c","org72084_sc3g9_c",
           "org72085_sc3g9_c","org72086_sc3g9_c","org72087_sc3g9_c",
           "org72088_sc3g9_c","org72089_sc3g9_c","org72090_sc3g9_c",
           "org72091_sc3g9_c","org72092_sc3g9_c","org92001_c",
           "org92002_c","org92003_c","org92004_c","org92005_c",
           "org92006_c","org92007_c","org92008_c","org92009_c",
           "org92010_c","org92011_c","org92012_c","org92013_c",
           "org92014_c","org92015_c","org92016_c","org92017_c",
           "org92018_c","org92019_c","org92020_c","org92021_c",
           "org92022_c","org92023_c","org92024_c","org92025_c",
           "org92026_c","org92027_c","org92028_c","org92029_c",
           "org92030_c","org92031_c","org92032_c","org92033_c",
           "org92034_c","org92035_c","org92036_c","org92037_c",
           "org92038_c","org92039_c","org92040_c","org92041_c",
           "org92042_c","org92043_c","org92044_c","org92045_c",
           "org92046_c","org92047_c","org92048_c","org92049_c",
           "org92050_c","org92051_c","org92052_c","org92053_c",
           "org92054_c","org92055_c","org92056_c","org92057_c",
           "org92058_c","org92059_c","org92060_c","org92061_c",
           "org92062_c","org92063_c","org92064_c","org92065_c",
           "org92066_c","org92067_c","org92068_c","org92069_c",
           "org92070_c","org92071_c","org92072_c","org92073_c",
           "org92074_c","org92075_c","org92076_c","org92077_c",
           "org92078_c","org92079_c","org92080_c","org92081_c",
           "org92082_c","org92083_c","org92084_c","org92085_c",
           "org92086_c","org92087_c","org92088_c","org92089_c",
           "org92090_c","org92091_c","org92092_c","org92093_c",
           "org92094_c","org92095_c","org92096_c","org92097_c",
           "org92098_c","org92099_c","org92100_c","org92101_c",
           "org92102_c","org92103_c","org92104_c","org92105_c",
           "org92106_c","org92107_c")
  )
)
x3w1a <- cbind(item = 1:length(items$A$w1),
               xsi = c(-1.923, 0.662, 0.114, 2.565, 0.472, -0.267, -0.554,
                       -2.447, 1.043, 0.569, 1.283, 0.214, -0.961, -1.005,
                       0.803, -0.934, 0.212, 0.654, 2.634, -0.106, -1.076,
                       1.193, 1.605, 0.431, 1.584, -1.469, 1.727, -1.304,
                       -2.562, 0.878, 2.242, 1.371, -0.995, 0.612, -0.062,
                       -0.952, 0.564, 1.098, -0.017, -2.037, 2.583, 2.095,
                       0.010, 2.362, -3.512, -1.800, 0.401, -1.459, -0.186,
                       -2.772))
rownames(x3w1a) <- items$A$w1
colnames(x3w1a)[1] <- ""
x3w1b <- cbind(item = 1:length(items$B$w1),
               xsi = c(-2.486, 0.548, -4.288, -2.796, -3.027, -2.708, -3.370,
                       -2.192, -2.652, -2.204, 0.684, -3.305, -2.134, -0.469,
                       -3.512, -2.975, -3.700, -1.526, -3.836, -3.695, -0.961,
                       -2.463, -3.820, -3.084, -1.827, -2.092, -1.327, -3.192,
                       -2.044, -3.412, -2.325, -3.858, -2.650, -0.948, -3.759,
                       -3.525, -3.612, -1.978, -3.779, -3.042, -2.627, -2.895,
                       -0.847, -0.045, -1.088, -1.832, -2.749, -0.794, -0.376,
                       -1.595, -2.580, -2.080, -1.842, -0.915, -0.658, -1.050,
                       -4.721, -3.913, -2.835, 0.271, -1.408, -0.934, -1.972,
                       -1.672, -2.706, -2.251, 0.140, -1.122, -2.488, -2.128,
                       0.262, -3.188, -2.429, -3.896, -1.936, -3.249, -1.448,
                       -2.791, -1.922, -1.472, 0.097, -1.102, 0.649, -0.313,
                       -3.393, -0.327, -2.153, -0.362, -0.765, -2.786, -2.007,
                       0.697, -1.489, -0.197, -1.098, -2.682, -1.152, -0.028,
                       -3.330, 1.783, -3.480, -1.842, -1.162, -2.244, -3.131,
                       -3.603, -2.840, -2.827, -4.802, -1.981, -1.621, -0.966,
                       -1.643, -3.784, -2.057, -1.636, -3.030, -3.616, -2.266,
                       0.207, -1.408, -2.342, -2.421, -1.453, -0.578, -0.373,
                       -1.593, -0.749, -0.735, -0.516, -2.176, -2.638, -1.924,
                       -4.508, -0.808, -2.636, -2.023, 0.080, -4.236, -0.408,
                       -2.187, -1.647, -3.455, -1.257, -1.267, -2.425, -1.988,
                       -0.604, -1.581, -3.455, -2.830, -2.249, -3.594, -4.312,
                       -2.618, -3.607, -2.671, -2.757, -2.492, -3.239, -0.846,
                       -4.250, -1.908, -2.096, -0.370, -3.744, -2.936, -4.199,
                       -3.484, 0.802, 0.910, -1.956, -1.375, -3.287, -4.192,
                       -3.106, -1.225, -0.635, -3.662, -1.715, -2.333))
rownames(x3w1b) <- items$B$w1
colnames(x3w1b)[1] <- ""

x3w3a <- cbind(item = 1:length(items$A$w3),
               xsi = c(-0.422, -0.612, -3.035, -1.710, -1.245, -0.211, 1.477,
                       0.889, -0.367, -0.779, 0.548, -2.538, -0.396, 0.914,
                       -0.862, -0.011, -1.566, -0.775, -1.525, 0.454, 1.185,
                       -0.675, -1.161, 1.312, 1.077, 0.523, -2.194, 0.398,
                       -0.868, 0.847, -0.470, -1.326, 1.222, -0.203, -2.227,
                       -2.299, -0.237, 1.788, -1.893, -0.645, -0.174, -2.370,
                       0.580, -1.102, 0.075, -2.107, -0.577, 0.075, -1.001,
                       -2.596, -3.119, -1.886, -1.127, -1.149, -3.253, -2.564,
                       -2.946, -0.796, -4.019, 0.813, -0.749, -1.424, -1.919,
                       -3.540, 0.243, -2.352, -0.549, -0.560, -2.007, -3.462,
                       -0.845, -3.492, 0.413, -0.802, -0.357, -1.610, -0.811,
                       -2.965))
rownames(x3w3a) <- items$A$w3
colnames(x3w3a)[1] <- ""
x3w3b <- cbind(item = 1:length(items$B$w3),
               xsi = c(-1.512, -2.620, -4.174, -2.668, -3.321, -3.314, -5.389,
                       -5.445, -4.847, -4.255, -0.847, -2.259, -5.353, -3.222,
                       -4.031, -3.342, -3.974, -2.590, -4.546, -2.840, -2.445,
                       -2.245, -2.765, -1.974, -1.537, -4.214, -1.090, -1.362,
                       0.325, -2.945, -1.886, -1.746, -4.091, -3.946, -1.741,
                       -1.963, -3.898, -3.234, -4.291, 1.948, -4.687, -4.477,
                       -2.758, -4.419, -0.813, -3.564, -3.777, -2.006, -3.102,
                       -4.067, -3.174, -3.424, -2.283, -5.353, -0.500, -3.277,
                       0.275, -1.488, -0.533, -1.686, -3.556, -2.682, -0.013,
                       -3.564, -2.138, -2.945, -5.087, -4.037, -3.359, -1.732,
                       -0.928, -2.682, -2.698, -1.840, -0.139, -3.421, -3.147,
                       1.313, -1.054, -2.877, -1.270, -2.974, -0.270, 1.054,
                       -2.713, 0.967, -1.750, 0.439, -2.130, -2.186, -2.407,
                       -0.947, -1.710, -3.225, -1.630, -2.438, -3.374, -1.751,
                       -3.963, -3.447, -0.610, -3.569, -2.985, -1.466, -1.385,
                       -2.445, 1.159, -0.954, -2.813, -3.744, -3.569, -3.512,
                       -2.765, -3.428, -3.581, -2.926, -2.613, -2.255, -1.795,
                       -2.212, -2.739, -4.073, -2.290, -2.716, -2.451, -2.321,
                       -2.888, -1.808, -2.911, -0.962, -2.238, -0.868, -1.569,
                       -3.946, -0.030, -3.359, -3.187, -2.373, -0.469, -4.187,
                       -4.104, -2.913, -2.825, -1.157, -2.239, -0.207, 2.207,
                       2.167, -0.112, -0.428, -2.961, 2.399, -1.860, -2.980,
                       0.240, 0.688, -1.948, -1.076, -0.588, -3.630, -2.529,
                       -3.872, -3.338, -3.730, -3.772, -4.519, -4.738, -4.780,
                       -4.802, -3.215, -4.452, -4.129, -4.460, -2.059, -2.485,
                       -4.738, -4.419, -3.497, -3.763, -1.312, -2.827, -2.720,
                       -1.470, -4.020, -4.619, -4.954, -4.667, -4.276, -3.857,
                       -4.067, -4.214, -3.851, -4.967, -4.161, -2.584, -4.002,
                       -0.328, -4.248, -4.905, -2.411, -5.205, -4.546, -4.357,
                       -4.628, -4.759, -5.144, -4.954, -2.953, -4.494, -1.977,
                       -2.893, -1.229, -3.540, -4.436, -4.738, -4.141, -4.511,
                       -5.426, -4.194, -5.073, -3.963, -4.707, -4.148, -2.319,
                       -2.495, -5.407, -4.305, -3.974, -3.585, -3.753, -3.159,
                       -2.983, -3.969, -5.159, -4.207, -3.867, -4.180, -3.721,
                       -4.485, -4.717, -2.544, -3.096, -3.206, -1.067, -3.606,
                       -2.855, -4.600, -2.607, -2.929, -3.985, -3.763, -3.653,
                       -4.167,
                       -4.628, -3.985, -2.097, -2.159, -0.956, -4.061, -2.818,
                       -2.613, -4.085, -4.555, -4.020, -4.312, -2.994, -3.615,
                       -5.205, -5.144, -5.302, -2.779, 0.012, -1.239, -3.260,
                       -2.442, -1.489, -4.110, -4.234, -3.280, -3.811, -3.162,
                       -3.395, -3.707, -3.957, -4.411, -4.870, -3.311, -3.792,
                       -4.091, -2.948, -4.091, -2.662, -2.569, -2.620, -2.278,
                       -2.164, -3.658, -2.969, -3.739, -4.334, -2.937))
rownames(x3w3b) <- items$B$w3
colnames(x3w3b)[1] <- ""

x3w5a <- cbind(item = 1:length(items$A$w5),
               xsi = c(-1.479, -4.128, -1.518, 0.583, 0.066, -1.173, -1.941,
                       -1.551, 0.135, -0.651, -2.164, -2.528, -0.723, 0.470,
                       -1.951, -2.087, 0.293, 0.062, -0.670, -3.389, -0.456,
                       -0.976, -0.052, -1.264, -2.415, 0.052, -1.230, -2.830,
                       -3.122, -0.905, 0.970, -2.664, -1.598, -0.362, -3.206,
                       -0.416, -2.093, -0.441, -2.607, -1.484, -1.031, -1.982,
                       -1.167, -4.216, -4.059, -0.509, -0.679, -2.430, -3.139,
                       -0.332, -4.103, -1.475, 1.303, -1.838, -2.063, -1.061,
                       -0.751, -2.898, -3.630, -0.620, -0.746, -1.343, -0.904,
                       1.010, -0.874, -0.864, -3.764, 0.007, -1.184, -0.819,
                       -2.521, -1.650, -1.472, -2.031,
                       -0.680, -2.836, -4.182))
rownames(x3w5a) <- items$A$w5
colnames(x3w5a)[1] <- ""
x3w5b <- cbind(item = 1:length(items$B$w5),
               xsi = c(-4.166, -1.880, -3.366, -3.533, -2.421, -2.904, -2.495,
                       -2.540, -3.095, -2.557, -3.878, -2.781, -3.964, -2.163,
                       -4.294, -2.516, -3.946, -4.386, -3.305, -2.029, -0.602,
                       -2.886, -3.529, -0.768, -3.056, -2.615, -3.802, -4.484,
                       -2.784, -1.531, -4.207, -4.093, 0.486, -2.173, -3.878,
                       -2.797, -3.923, -0.307, 0.024, -3.797, -0.059, -3.153,
                       -0.866, -3.792, -3.578, -3.444, -2.240, -2.898, -4.410,
                       -1.967, -3.587, -4.173, -3.231, -4.418, -4.264, -1.585,
                       -4.426, -4.068, -2.772, -2.811, -3.787, -0.063, -2.034,
                       -3.379, -4.324, -4.055, -4.146, -3.725, -4.545, -4.370,
                       -3.895, -3.856, -3.301, -3.085, -3.264, -3.578, -4.394,
                       -4.173, -3.529, -3.049, -3.601, -3.994, -3.347, -3.889,
                       -1.835, -3.403, -1.783, -2.913, -4.370, -0.688, -4.074,
                       -3.917, -3.146, -1.548, -4.563, -4.545, -3.958, -3.761,
                       -2.308, -3.347, -1.847, 1.964, 1.864, -1.358, -1.701,
                       -3.170, 2.143, -2.412, -3.316, -1.104, -0.009, -2.767,
                       -2.040, -2.092, -3.312, -2.934, -4.386, -3.792, -4.006,
                       -4.012, -3.808, -2.483, -3.439, -4.402, -0.622, -4.207,
                       -2.814, -3.657, -2.385, -2.437, -3.132, -2.497, -3.477,
                       -4.493, -2.650, -4.484, -4.309, -4.264, 0.969, 2.052,
                       -4.294, -3.917, -2.133, -4.502, -1.761, -4.100, 1.157,
                       -1.494, -0.537, -4.207, -3.895, -2.663, -4.410, -3.324,
                       -2.103, -2.293, -4.362, -4.068, -4.055, -1.562, -4.324,
                       -0.954, -3.912, -3.964, -3.293, -1.280, -3.787, -3.062,
                       -0.629, -0.326, -3.427, -3.347, -3.095, -4.159, -2.740,
                       -3.546, 0.077, -3.958, -3.681, -2.098, -4.024, -1.001,
                       -3.776, -2.109, -0.966, -4.152, -3.751, -4.106, -0.719,
                       -1.294, -4.418, -2.860, -4.324, -0.401, -4.294, -0.032,
                       -3.797, -4.119, -4.236, -1.334, -3.099, -0.915, -4.068,
                       -3.681, -3.245, -2.632, -3.917, -4.264, -3.546, -3.473,
                       -4.214, -1.560, -4.294, -3.824, -3.332, -3.309, -4.126,
                       -3.363, -3.964, -4.173, 0.263, -4.331, -4.434))
rownames(x3w5b) <- items$B$w5
colnames(x3w5b)[1] <- ""


xsi.fixed$cross[["ORA"]] <- list(
  SC2 = list(
    w6 = x2w6a
  ),
  SC3 = list(
    w1 = x3w1a,
    w3 = x3w3a,
    w5 = x3w5a
  )
)
xsi.fixed$long[["ORA"]] <- list(
  SC2 = list(
    w6 = x2w6a
  ),
  SC3 = list(
    w1 = x3w1a,
    w3 = x3w3a,
    w5 = x3w5a
  )
)

xsi.fixed$cross[["ORB"]] <- list(
  SC2 = list(
    w6 = x2w6b
  ),
  SC3 = list(
    w1 = x3w1b,
    w3 = x3w3b,
    w5 = x3w5b
  )
)
xsi.fixed$long[["ORB"]] <- list(
  SC2 = list(
    w6 = x2w6b
  ),
  SC3 = list(
    w1 = x3w1b,
    w3 = x3w3b,
    w5 = x3w5b
  )
)

### ----------------------------------------------------------------------------
### Vocabulary
### ----------------------------------------------------------------------------

# # SC1
# items <- list(
#         w4 = c(
#          "von40001_c", "von40002_c", "von40003_c", "von40004_c", "von40005_c",
#          "von40006_c", "von40007_c",
#          "von40008_c", "von40009_c", "von40010_c", "von40011_c", "von40012_c",
#          "von40013_c", "von40014_c",
#          "von40015_c", "von40016_c", "von40017_c", "von40018_c", "von40019_c",
#          "von40020_c", "von40021_c",
#          "von40022_c", "von40023_c", "von40024_c", "von40025_c", "von40026_c",
#          "von40027_c", "von40028_c",
#          "von40029_c", "von40030_c", "von40031_c", "von40032_c", "von40033_c",
#          "von40034_c", "von40035_c",
#          "von40036_c", "von40037_c", "von40038_c", "von40039_c", "von40040_c",
#          "von40041_c", "von40042_c",
#          "von40043_c", "von40044_c", "von40045_c", "von40046_c", "von40047_c",
#          "von40048_c", "von40049_c",
#          "von40050_c", "von40051_c", "von40052_c", "von40053_c", "von40054_c",
#          "von40055_c", "von40056_c",
#          "von40057_c", "von40058_c", "von40059_c", "von40060_c", "von40061_c",
#          "von40062_c", "von40063_c",
#          "von40064_c", "von40065_c", "von40066_c", "von40067_c", "von40068_c",
#          "von40069_c", "von40070_c",
#          "von40071_c", "von40072_c", "von40073_c", "von40074_c", "von40075_c",
#          "von40076_c", "von40077_c",
#          "von40078_c", "von40079_c", "von40080_c", "von40081_c", "von40082_c",
#          "von40083_c", "von40084_c",
#          "von40085_c", "von40086_c", "von40087_c", "von40088_c", "von40089_c",
#          "von40090_c", "von40091_c",
#          "von40092_c", "von40093_c", "von40094_c", "von40095_c", "von40096_c",
#          "von40097_c", "von40098_c",
#          "von40099_c", "von40100_c", "von40101_c", "von40102_c", "von40103_c",
#          "von40104_c", "von40105_c",
#          "von40106_c", "von40107_c", "von40108_c", "von40109_c", "von40110_c",
#          "von40111_c", "von40112_c",
#          "von40113_c", "von40114_c", "von40115_c", "von40116_c", "von40117_c",
#          "von40118_c", "von40119_c",
#          "von40120_c", "von40121_c", "von40122_c", "von40123_c", "von40124_c",
#          "von40125_c", "von40126_c",
#          "von40127_c", "von40128_c", "von40129_c", "von40130_c", "von40131_c",
#          "von40132_c", "von40133_c",
#          "von40134_c", "von40135_c", "von40136_c", "von40137_c", "von40138_c",
#          "von40139_c", "von40140_c",
#          "von40141_c", "von40142_c", "von40143_c", "von40144_c", "von40145_c",
#          "von40146_c", "von40147_c",
#          "von40148_c", "von40149_c", "von40150_c", "von40151_c", "von40152_c",
#          "von40153_c", "von40154_c",
#          "von40155_c", "von40156_c", "von40157_c", "von40158_c", "von40159_c",
#          "von40160_c", "von40161_c",
#          "von40162_c", "von40163_c", "von40164_c", "von40165_c", "von40166_c",
#          "von40167_c", "von40168_c",
#          "von40169_c", "von40170_c", "von40171_c", "von40172_c", "von40173_c",
#          "von40174_c", "von40175_c",
#          "von40176_c", "von40177_c", "von40178_c", "von40179_c", "von40180_c",
#          "von40181_c", "von40182_c",
#          "von40183_c", "von40184_c", "von40185_c", "von40186_c", "von40187_c",
#          "von40188_c", "von40189_c",
#          "von40190_c", "von40191_c", "von40192_c", "von40193_c", "von40194_c",
#          "von40195_c", "von40196_c",
#          "von40197_c", "von40198_c", "von40199_c", "von40200_c", "von40201_c",
#          "von40202_c", "von40203_c",
#          "von40204_c", "von40205_c", "von40206_c", "von40207_c", "von40208_c",
#          "von40209_c", "von40210_c",
#          "von40211_c", "von40212_c", "von40213_c", "von40214_c", "von40215_c",
#          "von40216_c", "von40217_c",
#          "von40218_c", "von40219_c", "von40220_c", "von40221_c", "von40222_c",
#          "von40223_c", "von40224_c",
#         "von40225_c", "von40226_c", "von40227_c", "von40228_c"),
#         w6 = c(
#         "von40001_sc1n6_c", "von40002_sc1n6_c", "von40003_sc1n6_c",
#         "von40004_sc1n6_c", "von40005_sc1n6_c", "von40006_sc1n6_c",
#         "von40007_sc1n6_c", "von40008_sc1n6_c", "von40009_sc1n6_c",
#         "von40010_sc1n6_c", "von40011_sc1n6_c", "von40012_sc1n6_c",
#         "von40013_sc1n6_c", "von40014_sc1n6_c", "von40015_sc1n6_c",
#         "von40016_sc1n6_c", "von40017_sc1n6_c", "von40018_sc1n6_c",
#         "von40019_sc1n6_c", "von40020_sc1n6_c", "von40021_sc1n6_c",
#         "von40022_sc1n6_c", "von40023_sc1n6_c", "von40024_sc1n6_c",
#         "von40025_sc1n6_c", "von40026_sc1n6_c", "von40027_sc1n6_c",
#         "von40028_sc1n6_c", "von40029_sc1n6_c", "von40030_sc1n6_c",
#         "von40031_sc1n6_c", "von40032_sc1n6_c", "von40033_sc1n6_c",
#         "von40034_sc1n6_c", "von40035_sc1n6_c", "von40036_sc1n6_c",
#         "von40037_sc1n6_c", "von40038_sc1n6_c", "von40039_sc1n6_c",
#         "von40040_sc1n6_c", "von40041_sc1n6_c", "von40042_sc1n6_c",
#         "von40043_sc1n6_c", "von40044_sc1n6_c", "von40045_sc1n6_c",
#         "von40046_sc1n6_c", "von40047_sc1n6_c", "von40048_sc1n6_c",
#         "von40049_sc1n6_c", "von40050_sc1n6_c", "von40051_sc1n6_c",
#         "von40052_sc1n6_c", "von40053_sc1n6_c", "von40054_sc1n6_c",
#         "von40055_sc1n6_c", "von40056_sc1n6_c", "von40057_sc1n6_c",
#         "von40058_sc1n6_c", "von40059_sc1n6_c", "von40060_sc1n6_c",
#         "von40061_sc1n6_c", "von40062_sc1n6_c", "von40063_sc1n6_c",
#         "von40064_sc1n6_c", "von40065_sc1n6_c", "von40066_sc1n6_c",
#         "von40067_sc1n6_c", "von40068_sc1n6_c", "von40069_sc1n6_c",
#         "von40070_sc1n6_c", "von40071_sc1n6_c", "von40072_sc1n6_c",
#         "von40073_sc1n6_c", "von40074_sc1n6_c", "von40075_sc1n6_c",
#         "von40076_sc1n6_c", "von40077_sc1n6_c", "von40078_sc1n6_c",
#         "von40079_sc1n6_c", "von40080_sc1n6_c", "von40081_sc1n6_c",
#         "von40082_sc1n6_c", "von40083_sc1n6_c", "von40084_sc1n6_c",
#         "von40085_sc1n6_c", "von40086_sc1n6_c", "von40087_sc1n6_c",
#         "von40088_sc1n6_c", "von40089_sc1n6_c", "von40090_sc1n6_c",
#         "von40091_sc1n6_c", "von40092_sc1n6_c", "von40093_sc1n6_c",
#         "von40094_sc1n6_c", "von40095_sc1n6_c", "von40096_sc1n6_c",
#         "von40097_sc1n6_c", "von40098_sc1n6_c", "von40099_sc1n6_c",
#         "von40100_sc1n6_c", "von40101_sc1n6_c", "von40102_sc1n6_c",
#         "von40103_sc1n6_c", "von40104_sc1n6_c", "von40105_sc1n6_c",
#         "von40106_sc1n6_c", "von40107_sc1n6_c", "von40108_sc1n6_c",
#         "von40109_sc1n6_c", "von40110_sc1n6_c", "von40111_sc1n6_c",
#         "von40112_sc1n6_c", "von40113_sc1n6_c", "von40114_sc1n6_c",
#         "von40115_sc1n6_c", "von40116_sc1n6_c", "von40117_sc1n6_c",
#         "von40118_sc1n6_c", "von40119_sc1n6_c", "von40120_sc1n6_c",
#         "von40121_sc1n6_c", "von40122_sc1n6_c", "von40123_sc1n6_c",
#         "von40124_sc1n6_c", "von40125_sc1n6_c", "von40126_sc1n6_c",
#         "von40127_sc1n6_c", "von40128_sc1n6_c", "von40129_sc1n6_c",
#         "von40130_sc1n6_c", "von40131_sc1n6_c", "von40132_sc1n6_c",
#         "von40133_sc1n6_c", "von40134_sc1n6_c", "von40135_sc1n6_c",
#         "von40136_sc1n6_c", "von40137_sc1n6_c", "von40138_sc1n6_c",
#         "von40139_sc1n6_c", "von40140_sc1n6_c", "von40141_sc1n6_c",
#         "von40142_sc1n6_c", "von40143_sc1n6_c", "von40144_sc1n6_c",
#         "von40145_sc1n6_c", "von40146_sc1n6_c", "von40147_sc1n6_c",
#         "von40148_sc1n6_c", "von40149_sc1n6_c", "von40150_sc1n6_c",
#         "von40151_sc1n6_c", "von40152_sc1n6_c", "von40153_sc1n6_c",
#         "von40154_sc1n6_c", "von40155_sc1n6_c", "von40156_sc1n6_c",
#         "von40157_sc1n6_c", "von40158_sc1n6_c", "von40159_sc1n6_c",
#         "von40160_sc1n6_c", "von40161_sc1n6_c", "von40162_sc1n6_c",
#         "von40163_sc1n6_c", "von40164_sc1n6_c", "von40165_sc1n6_c",
#         "von40166_sc1n6_c", "von40167_sc1n6_c", "von40168_sc1n6_c",
#         "von40169_sc1n6_c", "von40170_sc1n6_c", "von40171_sc1n6_c",
#         "von40172_sc1n6_c", "von40173_sc1n6_c", "von40174_sc1n6_c",
#         "von40175_sc1n6_c", "von40176_sc1n6_c", "von40177_sc1n6_c",
#         "von40178_sc1n6_c", "von40179_sc1n6_c", "von40180_sc1n6_c",
#         "von40181_sc1n6_c", "von40182_sc1n6_c", "von40183_sc1n6_c",
#         "von40184_sc1n6_c", "von40185_sc1n6_c", "von40186_sc1n6_c",
#         "von40187_sc1n6_c", "von40188_sc1n6_c", "von40189_sc1n6_c",
#         "von40190_sc1n6_c", "von40191_sc1n6_c", "von40192_sc1n6_c",
#         "von40193_sc1n6_c", "von40194_sc1n6_c", "von40195_sc1n6_c",
#         "von40196_sc1n6_c", "von40197_sc1n6_c", "von40198_sc1n6_c",
#         "von40199_sc1n6_c", "von40200_sc1n6_c", "von40201_sc1n6_c",
#         "von40202_sc1n6_c", "von40203_sc1n6_c", "von40204_sc1n6_c",
#         "von40205_sc1n6_c", "von40206_sc1n6_c", "von40207_sc1n6_c",
#         "von40208_sc1n6_c", "von40209_sc1n6_c", "von40210_sc1n6_c",
#         "von40211_sc1n6_c", "von40212_sc1n6_c", "von40213_sc1n6_c",
#         "von40214_sc1n6_c", "von40215_sc1n6_c", "von40216_sc1n6_c",
#         "von40217_sc1n6_c", "von40218_sc1n6_c", "von40219_sc1n6_c",
#         "von40220_sc1n6_c", "von40221_sc1n6_c", "von40222_sc1n6_c",
#         "von40223_sc1n6_c", "von40224_sc1n6_c", "von40225_sc1n6_c",
#         "von40226_sc1n6_c", "von40227_sc1n6_c", "von40228_sc1n6_c"),
#         w8 = c()
#       )
# # load("data-raw/item_difficulty_SC1_VO_w4.RData")
# # x1w4 <- item_difficulty_SC1_VO_w4
# # rm(item_difficulty_SC1_VO_w4)
# # load("data-raw/item_difficulty_SC1_VO_w6.RData")
# # x1w6 <- item_difficulty_SC1_VO_w6
# # rm(item_difficulty_SC1_VO_w6)

# SC2
items <- list(
        w1 = c("vok10002_c", "vok10007_c", "vok10008_c", "vok10009_c", "vok10010_c",
        "vok10011_c", "vok10012_c", "vok10013_c", "vok10014_c", "vok10015_c",
        "vok10016_c", "vok10017_c", "vok10018_c", "vok10019_c", "vok10020_c",
        "vok10021_c", "vok10022_c", "vok10023_c", "vok10024_c", "vok10025_c",
        "vok10026_c", "vok10027_c", "vok10028_c",
        "vok10031_c", "vok10032_c", "vok10033_c", "vok10034_c", "vok10035_c",
        "vok10036_c", "vok10037_c", "vok10038_c", "vok10039_c", "vok10040_c",
        "vok10041_c", "vok10042_c", "vok10043_c", "vok10045_c",
        "vok10046_c", "vok10047_c", "vok10048_c", "vok10049_c", "vok10050_c",
        "vok10051_c", "vok10052_c", "vok10053_c", "vok10054_c", "vok10055_c",
        "vok10056_c", "vok10057_c", "vok10058_c", "vok10060_c",
        "vok10061_c", "vok10062_c", "vok10063_c", "vok10064_c", "vok10065_c",
        "vok10066_c"),
        w3 = c("vok10067_sc2g1_c", "vok10043_sc2g1_c", "vok10053_sc2g1_c",
        "vok10049_sc2g1_c", "vog60001_sc2g1_c", "vok10025_sc2g1_c",
        "vok10076_sc2g1_c", "vok10050_sc2g1_c", "vog10009_c", "vog60009_sc2g1_c",
        "vok10060_sc2g1_c", "vok10066_sc2g1_c", "vok10063_sc2g1_c",
        "vok10040_sc2g1_c", "vok10074_sc2g1_c", "vok10033_sc2g1_c",
        "vog90015_sc2g1_c", "vok10051_sc2g1_c", "vok10061_sc2g1_c",
        "vog60051_sc2g1_c", "vog90007_sc2g1_c", "vog60015_sc2g1_c",
        "vok10057_sc2g1_c", "vok10072_sc2g1_c", "vog90016_sc2g1_c",
        "vog90032_sc2g1_c", "vog60010_sc2g1_c", "vok10041_sc2g1_c",
        "vok10052_sc2g1_c", "vog60032_sc2g1_c", "vok10031_sc2g1_c",
        "vok10045_sc2g1_c", "vok10039_sc2g1_c", "vog10034_c", "vok10034_sc2g1_c",
        "vok10058_sc2g1_c", "vog90031_sc2g1_c", "vog60049_sc2g1_c",
        "vok10065_sc2g1_c", "vog10040_c", "vok10071_sc2g1_c", "vok10069_sc2g1_c",
        "vog60025_sc2g1_c", "vog10044_c", "vok10028_sc2g1_c", "vog10046_c",
        "vog60027_sc2g1_c", "vog60047_sc2g1_c", "vok10022_sc2g1_c",
        "vok10038_sc2g1_c", "vog90028_sc2g1_c", "vok10047_sc2g1_c",
        "vok10046_sc2g1_c", "vog60019_sc2g1_c", "vok10048_sc2g1_c", "vog10056_c",
        "vog90020_sc2g1_c", "vok10037_sc2g1_c", "vog60030_sc2g1_c", "vog10060_c",
        "vok10077_sc2g1_c", "vog10062_c", "vog10063_c", "vok10042_sc2g1_c",
        "vok10064_sc2g1_c", "vok10026_sc2g1_c"),
        w5 = c("vog10034_sc2g3_c",
        "vok10043_sc2g3_c", "vog90031_sc2g3_c",
        "vog10060_sc2g3_c", "vog10009_sc2g3_c",
        "vog60041_sc2g3_c", "vog60025_sc2g3_c",
        "vok10075_sc2g3_c", "vok10033_sc2g3_c", "vog90015_sc2g3_c",
        "vok10061_sc2g3_c", "vok10065_sc2g3_c", #"vog60015_sc2g3_c", # not in SUF, but in TR
        "vok10072_sc2g3_c",
        "vog60030_sc2g3_c", "vog60029_sc2g3_c", "vog90003_sc2g3_c",
        "vog10062_sc2g3_c",
        "vok10026_sc2g3_c", "vog60037_sc2g3_c", "vok10058_sc2g3_c",
        "vog60049_sc2g3_c",
        "vok10076_sc2g3_c", "vok10040_sc2g3_c", "vog10040_sc2g3_c",
        "vok10071_sc2g3_c", "vok10060_sc2g3_c",
        "vog10044_sc2g3_c", "vog60045_sc2g3_c",
        "vog90035_sc2g3_c", "vok10074_sc2g3_c",
        "vog60027_sc2g3_c",
        "vok10051_sc2g3_c", "vog60047_sc2g3_c", "vok10073_sc2g3_c",
        "vog90037_sc2g3_c",
        "vok10038_sc2g3_c", "vok10047_sc2g3_c",
        "vok10057_sc2g3_c", "vok10046_sc2g3_c", "vog60019_sc2g3_c",
        "vok10048_sc2g3_c", "vog90016_sc2g3_c",
        "vog90032_sc2g3_c", "vog60010_sc2g3_c", "vog60032_sc2g3_c",
        "vog60054_sc2g3_c", "vok10064_sc2g3_c", "vog90028_sc2g3_c")
      )
x2w1 <- cbind(item = 1:length(items$w1),
            xsi = c(-3.15, -2.06, -2.29, -2.75, -2.87, -2.40, -2.29, -1.97,
                    -2.02, -2.46, -2.26, -2.23, -2.22, -2.70, -1.87, -1.90,
                    -1.19, -1.22, -1.83, -1.67, 0.19, -1.04, -0.72, -0.08,
                    -1.99, 0.83, -0.59, -0.63, -1.40, -0.95, -0.08, -1.40,
                    -0.07, -0.21, -0.84, 1.46, 0.67, 0.85, 1.27, 0.50, -0.39,
                    -0.97, -0.02, 0.82, 0.07, -2.23, -1.66, -0.22, 0.54,
                    0.28, 0.88, 0.31, -1.82, -0.39, 0.38, 1.89, -0.54))
rownames(x2w1) <- items$w1
colnames(x2w1)[1] <- ""
x2w3 <- cbind(item = 1:length(items$w3),
            xsi = c(-1.54, -0.21, -1.75, -2.14, 0.66, -2.31, 0.65, -1.60,
                    1.30, 0.86, -0.30, -2.18, -1.83, -2.15, 0.28, -0.79,
                    0.08, -1.18, -0.70, 0.58, -0.62, 0.01, -0.66, -0.29,
                    0.27, 1.75, -1.58, -1.55, -0.60, 0.43, -2.08, -1.79,
                    -2.95, -0.66, -1.94, -0.77, 0.53, 0.72, 0.33, 0.99,
                    -0.17, 0.56, 0.24, -0.85, -2.48, -1.01, 1.09, 0.07,
                    -1.54, -1.66, -1.58, -0.23, -1.03, -0.44,
                    -1.31, 0.62, 0.15, -2.58, 0.74, 0.81, -0.79, -1.64,
                    -0.23, -2.49, -1.23, -0.99))
rownames(x2w3) <- items$w3
colnames(x2w3)[1] <- ""
x2w5 <- cbind(item = 1:length(items$w5),
            xsi = c(-2.19, -1.57, 0.09, -0.96, -0.29, 0.62, -0.65, 0.02,
                    -2.69, -0.01, -1.62, -0.57, #-0.67,
                    -1.55, -0.35, -0.48,
                    -0.40, -3.10, -2.57, -0.03, -1.73, -0.02, -0.39, -3.29,
                    0.18, -1.41, -1.33, -2.11, 1.43, -0.82, -0.57, -0.36,
                    -2.15, -0.28, -0.05, 0.72, -2.83, -1.54, -0.93, -1.92,
                    -1.11, -1.82, -0.4, 0.56, -2.56, -0.82, 0.41, -2.41,
                    -2.73))
rownames(x2w5) <- items$w5
colnames(x2w5)[1] <- ""

xsi.fixed$cross[["VO"]] <- list(
  # SC1 = list(
  #   w4 = x1w4,
  #   w6 = x1w6
  # ),
  SC2 = list(
    w1 = x2w1,
    w3 = x2w3,
    w5 = x2w5
  )
)
# necessary in the way of linking now?
x2w3[, 2] <- x2w3[, 2] + 0.22 # dropout correction: only for longitudinal?
x2w5[, 2] <- x2w5[, 2] + 0.22 + 0.01 # dropout correction: only for longitudinal?
xsi.fixed$long[["VO"]] <- list(
  # SC1 = list(
  #   w4 = x1w4,
  #   w6 = x1w6
  # ),
  SC2 = list(
    w1 = x2w1,
    w3 = x2w3,
    w5 = x2w5
  )
)



### ----------------------------------------------------------------------------
### Grammar
### ----------------------------------------------------------------------------

# SC2
load("data-raw/item_difficulty_SC2_GR_w1.RData")
x2w1 <- item_difficulty_SC2_GR_w1
rm(item_difficulty_SC2_GR_w1)
load("data-raw/item_difficulty_SC2_GR_w3.RData")
x2w3 <- item_difficulty_SC2_GR_w3
rm(item_difficulty_SC2_GR_w3)

xsi.fixed$cross[["GR"]] <- list(
  SC2 = list(
    w1 = x2w1,
    w3 = x2w3
  )
)
xsi.fixed$long[["GR"]] <- list(
  SC2 = list(
    w1 = x2w1,
    w3 = x2w3
  )
)



### ----------------------------------------------------------------------------
### Cognitive Development
### ----------------------------------------------------------------------------

# SC1
# items <- list(
#     w1 = c("cdn1c001_c", "cdn1c002_c", "cdn1c003_c", "cdn1c004_c",
#            "cdn1c005_c", "cdn1c006_c", "cdn1c007_c", "cdn1c008_c",
#            "cdn1c009_c", "cdn1c010_c", "cdn1c011_c", "cdn1c012_c",
#            "cdn1c013_c", "cdn1c014_c")
# )
# x1w1 <- cbind(item = 1:length(items$w1),
#               xsi = c(-3.478, 2.564, -1.486, 2.466, -2.865, 0.735, -1.113,
#                       2.030, 0.402, -1.860, 1.241, -4.569, 1.923, 4.009))
# rownames(x1w1) <- items$w1
# colnames(x1w1)[1] <- ""
load("data-raw/item_difficulty_SC1_CD_w1.RData")
x1w1 <- item_difficulty_SC1_CD_w1
rm(item_difficulty_SC1_CD_w1)

xsi.fixed$cross[["CD"]] <- list(
  SC1 = list(
    w1 = x1w1
  )
)


# order item difficulties alphabetically
# SC2
# MA
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["MA"]][["SC2"]][["w2"]] <-
        xsi.fixed[[type]][["MA"]][["SC2"]][["w2"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC2"]][["w2"]])), ]
    xsi.fixed[[type]][["MA"]][["SC2"]][["w2"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC2"]][["w2"]])
    xsi.fixed[[type]][["MA"]][["SC2"]][["w3"]] <-
        xsi.fixed[[type]][["MA"]][["SC2"]][["w3"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC2"]][["w3"]])), ]
    xsi.fixed[[type]][["MA"]][["SC2"]][["w3"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC2"]][["w3"]])
    xsi.fixed[[type]][["MA"]][["SC2"]][["w4"]] <-
        xsi.fixed[[type]][["MA"]][["SC2"]][["w4"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC2"]][["w4"]])), ]
    xsi.fixed[[type]][["MA"]][["SC2"]][["w4"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC2"]][["w4"]])
    xsi.fixed[[type]][["MA"]][["SC2"]][["w6"]] <-
        xsi.fixed[[type]][["MA"]][["SC2"]][["w6"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC2"]][["w6"]])), ]
    xsi.fixed[[type]][["MA"]][["SC2"]][["w6"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC2"]][["w6"]])
}

# SC
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["SC"]][["SC2"]][["w1"]] <-
        xsi.fixed[[type]][["SC"]][["SC2"]][["w1"]][
            order(rownames(xsi.fixed[[type]][["SC"]][["SC2"]][["w1"]])), ]
    xsi.fixed[[type]][["SC"]][["SC2"]][["w1"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["SC"]][["SC2"]][["w1"]])
    xsi.fixed[[type]][["SC"]][["SC2"]][["w3"]] <-
        xsi.fixed[[type]][["SC"]][["SC2"]][["w3"]][
            order(rownames(xsi.fixed[[type]][["SC"]][["SC2"]][["w3"]])), ]
    xsi.fixed[[type]][["SC"]][["SC2"]][["w3"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["SC"]][["SC2"]][["w3"]])
    # if (type == "long") next
    # xsi.fixed[[type]][["SC"]][["SC2"]][["w5"]] <-
    #     xsi.fixed[[type]][["SC"]][["SC2"]][["w5"]][
    #         order(rownames(xsi.fixed[[type]][["SC"]][["SC2"]][["w5"]])), ]
    # xsi.fixed[[type]][["SC"]][["SC2"]][["w5"]][, 1] <-
    #     1:nrow(xsi.fixed[[type]][["SC"]][["SC2"]][["w5"]])
}

# SC3
# MA
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["MA"]][["SC3"]][["w1"]] <-
        xsi.fixed[[type]][["MA"]][["SC3"]][["w1"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC3"]][["w1"]])), ]
    xsi.fixed[[type]][["MA"]][["SC3"]][["w1"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC3"]][["w1"]])
    xsi.fixed[[type]][["MA"]][["SC3"]][["w3"]] <-
        xsi.fixed[[type]][["MA"]][["SC3"]][["w3"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC3"]][["w3"]])), ]
    xsi.fixed[[type]][["MA"]][["SC3"]][["w3"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC3"]][["w3"]])
    xsi.fixed[[type]][["MA"]][["SC3"]][["w5"]] <-
        xsi.fixed[[type]][["MA"]][["SC3"]][["w5"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC3"]][["w5"]])), ]
    xsi.fixed[[type]][["MA"]][["SC3"]][["w5"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC3"]][["w5"]])
    if (type == "long") next
    xsi.fixed[[type]][["MA"]][["SC3"]][["w9"]] <-
        xsi.fixed[[type]][["MA"]][["SC3"]][["w9"]][
            order(rownames(xsi.fixed[[type]][["MA"]][["SC3"]][["w9"]])), ]
    xsi.fixed[[type]][["MA"]][["SC3"]][["w9"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["MA"]][["SC3"]][["w9"]])
}

# RE
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["RE"]][["SC3"]][["w1"]] <-
        xsi.fixed[[type]][["RE"]][["SC3"]][["w1"]][
            order(rownames(xsi.fixed[[type]][["RE"]][["SC3"]][["w1"]])), ]
    xsi.fixed[[type]][["RE"]][["SC3"]][["w1"]][, 1] <-
      1:nrow(xsi.fixed[[type]][["RE"]][["SC3"]][["w1"]])
    xsi.fixed[[type]][["RE"]][["SC3"]][["w3"]] <-
      xsi.fixed[[type]][["RE"]][["SC3"]][["w3"]][
        order(rownames(xsi.fixed[[type]][["RE"]][["SC3"]][["w3"]])), ]
    xsi.fixed[[type]][["RE"]][["SC3"]][["w3"]][, 1] <-
      1:nrow(xsi.fixed[[type]][["RE"]][["SC3"]][["w3"]])
    xsi.fixed[[type]][["RE"]][["SC3"]][["w6"]] <-
      xsi.fixed[[type]][["RE"]][["SC3"]][["w6"]][
        order(rownames(xsi.fixed[[type]][["RE"]][["SC3"]][["w6"]])), ]
    xsi.fixed[[type]][["RE"]][["SC3"]][["w6"]][, 1] <-
      1:nrow(xsi.fixed[[type]][["RE"]][["SC3"]][["w6"]])
    if (type == "long") {
      xsi.fixed[[type]][["RE"]][["SC3"]][["w9"]] <-
        xsi.fixed[[type]][["RE"]][["SC3"]][["w9"]][
          order(rownames(xsi.fixed[[type]][["RE"]][["SC3"]][["w9"]])), ]
      xsi.fixed[[type]][["RE"]][["SC3"]][["w9"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["RE"]][["SC3"]][["w9"]])
    }
}

# EF
xsi.fixed[["cross"]][["EF"]][["SC3"]][["w9"]] <-
    xsi.fixed[["cross"]][["EF"]][["SC3"]][["w9"]][
        order(rownames(xsi.fixed[["cross"]][["EF"]][["SC3"]][["w9"]])), ]
xsi.fixed[["cross"]][["EF"]][["SC3"]][["w9"]][, 1] <-
    1:nrow(xsi.fixed[["cross"]][["EF"]][["SC3"]][["w9"]])


# SC4
# MA
xsi.fixed[["cross"]][["MA"]][["SC4"]][["w7"]] <-
    xsi.fixed[["cross"]][["MA"]][["SC4"]][["w7"]][
        order(rownames(xsi.fixed[["cross"]][["MA"]][["SC4"]][["w7"]])), ]
xsi.fixed[["cross"]][["MA"]][["SC4"]][["w7"]][, 1] <-
    1:nrow(xsi.fixed[["cross"]][["MA"]][["SC4"]][["w7"]])

# RE
xsi.fixed[["long"]][["RE"]][["SC4"]][["w10"]] <-
    xsi.fixed[["long"]][["RE"]][["SC4"]][["w10"]][
        order(rownames(xsi.fixed[["long"]][["RE"]][["SC4"]][["w10"]])), ]
xsi.fixed[["long"]][["RE"]][["SC4"]][["w10"]][, 1] <-
    1:nrow(xsi.fixed[["long"]][["RE"]][["SC4"]][["w10"]])

# SC
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["SC"]][["SC4"]][["w1"]] <-
        xsi.fixed[[type]][["SC"]][["SC4"]][["w1"]][
            order(rownames(xsi.fixed[[type]][["SC"]][["SC4"]][["w1"]])), ]
    xsi.fixed[[type]][["SC"]][["SC4"]][["w1"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["SC"]][["SC4"]][["w1"]])
    xsi.fixed[[type]][["SC"]][["SC4"]][["w5"]] <-
        xsi.fixed[[type]][["SC"]][["SC4"]][["w5"]][
            order(rownames(xsi.fixed[[type]][["SC"]][["SC4"]][["w5"]])), ]
    xsi.fixed[[type]][["SC"]][["SC4"]][["w5"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["SC"]][["SC4"]][["w5"]])
}

# IC
for (type in c("cross", "long")) {
    xsi.fixed[[type]][["IC"]][["SC4"]][["w1"]] <-
        xsi.fixed[[type]][["IC"]][["SC4"]][["w1"]][
            order(rownames(xsi.fixed[[type]][["IC"]][["SC4"]][["w1"]])), ]
    xsi.fixed[[type]][["IC"]][["SC4"]][["w1"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["IC"]][["SC4"]][["w1"]])
    xsi.fixed[[type]][["IC"]][["SC4"]][["w7"]] <-
        xsi.fixed[[type]][["IC"]][["SC4"]][["w7"]][
            order(rownames(xsi.fixed[[type]][["IC"]][["SC4"]][["w7"]])), ]
    xsi.fixed[[type]][["IC"]][["SC4"]][["w7"]][, 1] <-
        1:nrow(xsi.fixed[[type]][["IC"]][["SC4"]][["w7"]])
}

# EF
xsi.fixed[["cross"]][["EF"]][["SC4"]][["w7"]] <-
    xsi.fixed[["cross"]][["EF"]][["SC4"]][["w7"]][
        order(rownames(xsi.fixed[["cross"]][["EF"]][["SC4"]][["w7"]])), ]
xsi.fixed[["cross"]][["EF"]][["SC4"]][["w7"]][, 1] <-
    1:nrow(xsi.fixed[["cross"]][["EF"]][["SC4"]][["w7"]])

# for (a in names(xsi.fixed)) { # type
#     for (b in names(xsi.fixed[[a]])) { # domain
#         for (c in names(xsi.fixed[[a]][[b]])) { # SC
#             for (d in names(xsi.fixed[[a]][[b]][[c]])) { # wave / matrix
#                 if (is.null(rownames(xsi.fixed[[a]][[b]][[c]][[d]]))) {
#                     next
#                 }
#                 xsi.fixed[[a]][[b]][[c]][[d]] <-
#                     xsi.fixed[[a]][[b]][[c]][[d]][
#                         order(rownames(xsi.fixed[[a]][[b]][[c]][[d]])), ]
#                 xsi.fixed[[a]][[b]][[c]][[d]][, 1] <- 1:nrow(xsi.fixed[[a]][[b]][[c]][[d]])
#             }
#         }
#     }
# }


# save fixed item parameters
save(xsi.fixed, file = "data-raw/xsi_fixed.RData")

rm(list = ls())
