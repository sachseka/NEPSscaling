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
rm(list = ls())

# fixed item parameters
xsi.fixed <- list(cross = list(), long = list())

### ----------------------------------------------------------------------------
### mathematical competence
### ----------------------------------------------------------------------------

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
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-0.510, -0.375, 0.747, 0.070, -0.099, 1.328, 0.299, 0.894,
                      0.512, -0.891, 0.064, -0.984, 1.545, -0.342, 1.143,
                      -1.295, -0.217, 0.090, -1.377, -0.270, -1.247, -0.934,
                      1.109, -0.033, 0.188, -1.354, -0.331, -0.514, -0.151,
                      -1.337, -0.340))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
x4 <- xsi
rownames(x4) <- NEPScaling:::item_labels$SC4$MA$w10

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
x <- c(
  -1.206, 1.748, -0.822, 0.450, -2.203, -0.646, 0.461, 0.344, -0.863,
  -0.472, -0.768, 0.710, -2.458, -1.279, 0.147, -1.212, -1.754, -0.522,
  -0.147, 1.171
)
x51 <- cbind(item = 1:length(x), xsi = x)
rownames(x51) <- items
colnames(x51)[1] <- ""
x52 <- xsi
rownames(x52) <- NEPScaling:::item_labels$SC5$MA$w12

# SC6

items <- c(
  "maa3q071_c", "mag9v131_sc6a3_c", "mag9r261_sc6a3_c", "mag9r111_sc6a3_c",
  "maa3d131_c", "maa3d132_c", "mag9r051_sc6a3_c", "maa3d041_c", "maa3r081_c",
  "maa3v082_c", "mag9d201_sc6a3_c", "maa3r091_c", "mag9v121_sc6a3_c",
  "maa3r121_c", "maa3d112_c", "maa3r011_c", "maa3q101_c", "mag5v321_sc6a3_c",
  "mag9q021_sc6a3_c", "maa3v061_c", "maa3q021_c"
)
x <- c(
  -0.527, -1.803, 1.550, -2.248, -0.813, 0.702, -0.229, -2.019, -0.215,
  0.691, -0.349, 1.481, -0.086, -1.195, 1.579, -0.124, -0.075, -1.120,
  -0.824, -3.183, 1.007
)
x61 <- cbind(item = 1:length(x), xsi = x)
rownames(x61) <- items
colnames(x61)[1] <- ""
x62 <- xsi
rownames(x62) <- NEPScaling:::item_labels$SC6$MA$w9

# store pre-scaled item parameters in list
xsi.fixed$cross[["MA"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w10 = x4
    ),
    SC5 = list(
      w1 = x51,
      w12 = x52
    ),
    SC6 = list(
      w3 = x61,
      w9 = x62
    )
  )
xsi.fixed$long[["MA"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w10 = x4
    ),
    SC5 = list(
      w1 = x51,
      w12 = x52
    ),
    SC6 = list(
      w3 = x61,
      w9 = x62
    )
  )
rm(xsi, x4, x51, x61, x52, x62, items, dat, pcm, x)

### ----------------------------------------------------------------------------
### reading competence
### ----------------------------------------------------------------------------

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
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-1.289, -0.821, -2.313, -2.679, -0.907, -1.141, -2.146,
                      -0.807, -0.488, -1.691, -1.142, -1.831, -0.223, -0.145,
                      -1.627, -0.940, 0.412, -0.397, -0.809, 0.167, -0.031,
                      -1.285, -0.710, -2.223, -1.178, -2.016, -1.499, -0.423,
                      -1.176, -0.422, -0.881, -1.517, -0.475, -1.460, 1.132,
                      -1.500, -0.942, -1.672, -0.073, 0.066, -1.288))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
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
x <- c(
  -1.154, -4.048, -3.441, -0.793, 0.124, -1.172, -2.441, -0.162, -2.538,
  -3.151, -0.768, -1.384, -0.639, -1.892, -1.132, -1.909, -0.533,
  -1.384, 0.259, -0.274, -0.822, -0.504, -1.949, -1.896, -2.603,
  -1.197, -0.838, -1.344
)
x51 <- cbind(item = 1:length(x), xsi = x)
rownames(x51) <- items
colnames(x51)[1] <- ""


# B110, B116, B114
xsi <- readODS::read_ods(
  path = "data-raw/neps_raw/b110_114_116_alte_schwierigkeiten.ods",
  col_names = FALSE
)

# match correct difficulties to cohort
items4 <- c(1:10, 12:17, 20, 22:26, 28:31, 33:42)
items5 <- c(6:10, 12, 13, 15, 16, 19, 20, 22:24, 27:29, 32, 39, 40, 42)
items6 <- c(1:9, 11:16, 18, 21:26, 28:31, 33:42)
xsi.sc4 <- xsi[items4, ]
rownames(xsi.sc4) <- NEPScaling:::item_labels$SC4$RE$w10
colnames(xsi.sc4)[1] <- ""
xsi.sc4[, 1] <- 1:nrow(xsi.sc4)
xsi.sc4 <- as.matrix(xsi.sc4)
xsi.sc5 <- xsi[items5, ]
rownames(xsi.sc5) <- NEPScaling:::item_labels$SC5$RE$w12
colnames(xsi.sc5)[1] <- ""
xsi.sc5[, 1] <- 1:nrow(xsi.sc5)
xsi.sc5 <- as.matrix(xsi.sc5)
xsi.sc6 <- xsi[items6, ]
rownames(xsi.sc6) <- NEPScaling:::item_labels$SC6$RE$w9
colnames(xsi.sc6)[1] <- ""
xsi.sc6[, 1] <- 1:nrow(xsi.sc6)
xsi.sc6 <- as.matrix(xsi.sc6)

# wave 5 sample refreshment
load("data-raw/item_diff_SC6_RE_w3.RData")
item_diff_SC6_RE_w3 <- item_diff_SC6_RE_w3[1:30, ]

# store pre-scaled item parameters in list
xsi.fixed$cross[["RE"]] <-
  list(
    SC4 = list(
      w2 = x4w2,
      w7 = x4w7,
      w10 = xsi.sc4
    ),
    SC5 = list(
      w1 = x51,
      w12 = xsi.sc5
    ),
    SC6 = list(
      w3 = item_diff_SC6_RE_w3,
      w5 = item_diff_SC6_RE_w3,
      w9 = xsi.sc6
    )
  )
xsi.fixed$long[["RE"]] <-
  list(
    SC4 = list(
      w2 = x4w2,
      w7 = x4w7,
      w10 = xsi.sc4
    ),
    SC5 = list(
      w1 = x51,
      w12 = xsi.sc5
    ),
    SC6 = list(
      w3 = item_diff_SC6_RE_w3,
      w5 = item_diff_SC6_RE_w3,
      w9 = xsi.sc6
    )
  )


### ----------------------------------------------------------------------------
### ICT literacy
### ----------------------------------------------------------------------------

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
    "icg12018s_c", "ica5003x_sc4g12_c", "icg12107s_c", "icg12004s_c",
    "icg12010x_c", "icg12011x_c", "ica5008x_sc4g12_c", "icg12060s_c",
    "icg12013s_c", "icg12016s_c", "ica5019x_sc4g12_c",
    "icg12121x_c", "icg12028s_c", "ica5023x_sc4g12_c", "ica5027x_sc4g12_c",
    "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
    "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
    "icg12046s_c", "ica5021s_sc4g12_c", "ica5052s_sc4g12_c", "icg12048s_c",
    "icg12050s_c", "icg12054s_c", "icg12109s_c", "icg12119s_c"
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
x4w7 <- cbind(item = 1:length(items$w7),
              xsi = c(-1.164, 1.320, -0.587, -0.124, 0.041, 1.323, -0.850,
                      -1.512, -1.308, -0.365, 1.124, 0.430, -1.359, 0.109,
                      0.100, -0.443, -0.889, -0.082, 0.258, -0.342, -1.584,
                      -0.287, -0.719, -0.533, -2.191, -0.285, -1.010, -0.928,
                      -0.713, -0.826, -0.840))
rownames(x4w7) <- items$w7
colnames(x4w7)[1] <- ""
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
x <- c(
  1.40366579, -0.86991232, -1.20855800, -0.83436944, -0.90459723,
  -0.37176353, -0.32118536, -0.62767290, -0.56493133, -1.70029390,
  1.98597554, -0.79915894, -1.62065130, 0.41288635, -0.41379726,
  -0.06746113, -0.05061254, -1.32526649, -2.80908060, -0.35641518,
  1.30212156, 0.44628818, -0.54315593, 0.49712400, -0.51068062,
  -0.29597091, 0.86393283, -1.07652203, 0.35764623, -0.85212525
) # NEPScaling SC5 IC without additional info. or rotation
x5 <- cbind(item = 1:length(x), xsi = x)
rownames(x5) <- items
colnames(x5)[1] <- ""

# SC6
items <- c(
  "ica5001x_c", "ica5003x_c", "ica5005x_c", "ica5004s_c", "ica5006x_c",
  "ica5007x_c", "ica5008x_c", "ica5010x_c", "ica5017s_c", "ica5018s_c",
  "ica5015s_c", "ica5019x_c", "ica5016s_c", "ica5020s_c", "ica5023x_c",
  "ica5027x_c", "ica5026x_c", "ica5029x_c", "ica5028x_c", "ica5030x_c",
  "icg9119x_sc6a5_c", "ica5050s_c", "icg9122x_sc6a5_c", "ica5047s_c",
  "ica5046x_c", "ica5021s_c", "ica5052s_c", "ica5054x_c", "ica5057x_c"
)
xsi <- c(
  -1.78, 1.04, 1.47, -1.82, -0.93, -0.68, -1.32, -0.01, -1.74, -0.64,
  -2.23, 0.31, -2.16, -2.22, 0.39, 0.35, 0.58, -1.72, -1.25, -0.96,
  -1.32, -2.47, -0.21, -2.10, -0.96, -1.80, -0.99, -1.00, -1.45
)
x6 <- cbind(item = 1:length(xsi), xsi)
rownames(x6) <- items
colnames(x6)[1] <- ""

# store pre-scaled item parameters in list
xsi.fixed$cross[["IC"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )
xsi.fixed$long[["IC"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w7 = x4w7,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )


### ----------------------------------------------------------------------------
### Science competence
### ----------------------------------------------------------------------------


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
  w13 = c()
)
x4w1 <- cbind(item = 1:length(items$w1),
              xsi = c(-0.608, -2.103, -0.641, -0.743, -0.099, -1.027, -1.673,
                      -0.912, -1.752, -0.236, 0.346, -0.789, -2.199, -1.558,
                      0.030, -1.369, -0.913, -2.478, -1.264, -1.150, -0.319,
                      -0.553, 0.401, -1.215, -0.320, 0.149, 1.160, 0.835))
rownames(x4w1) <- items$w1
colnames(x4w1)[1] <- ""
x4w5 <- cbind(item = 1:length(items$w5),
              xsi = c(-0.391, 0.087, 0.171, 0.390, -0.879, -0.528, -1.170,
                      -0.273, -0.608, -0.147, -0.661, -0.407, -0.654, 1.516,
                      -0.406, 1.403, 1.329, 1.895, -0.678, 0.024, 1.458, 0.619,
                      -0.618, 0.031))
rownames(x4w5) <- items$w5
colnames(x4w5)[1] <- ""
x4w13 <- cbind(item = 1:length(items$w13),
              xsi = c())
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
x <- c(
  0.92237716, 1.46643444, -0.75800539, -0.13650864, -1.07382160,
  -0.46738747, -0.80254731, -0.18597379, -0.57964106, 0.11567871,
  -0.13176793, 0.25520290, -0.57676871, -0.22201618, 0.10421985,
  -0.24081756, -0.61244643, -0.79890169, 0.02618897, -0.42064414,
  -0.39688836, -0.45444353, -0.64151218, -0.62774150, 0.54444123,
  -0.75949384, -0.20641310, 0.12653751, -0.02551815
) # NEPScaling SC5 SC without additional info. or rotation
x5 <- cbind(item = 1:length(x), xsi = x)
rownames(x5) <- items
colnames(x5)[1] <- ""

# SC6
items <- c(
  "sca56120_c", "sca56130_c", "sca51110_c", "sca51140_c", "sca50410_c",
  "sca5652s_c", "sca56540_c", "sca51430_c", "sca51440_c", "sca50210_c",
  "sca50220_c", "sca50710_c", "sca50720_c", "sca56310_c", "sca56320_c",
  "sca5091s_c", "sca56020_c", "sca56030_c", "sca50520_c", "sca50530_c",
  "sca51020_c", "sca51030_c"
)
x <- c(
  0.633, -1.601, -1.774, -1.407, -1.462, -1.634, -0.100, -1.758, -0.628,
  -0.585, -1.073, -1.377, 0.757, -1.306, 0.769, -1.697, -0.984, -2.008,
  -0.501, 0.416, 1.276, -1.721
)
x6 <- cbind(item = 1:length(x), xsi = x)
rownames(x6) <- items
colnames(x6)[1] <- ""


# store pre-scaled item parameters in list
xsi.fixed$cross[["SC"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w5 = x4w5,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )
xsi.fixed$long[["SC"]] <-
  list(
    SC4 = list(
      w1 = x4w1,
      w5 = x4w5,
      w13 = x4w13
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )

### ----------------------------------------------------------------------------
### English as a foreign language
### ----------------------------------------------------------------------------

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
              xsi = c(-0.25, -0.77, -1.12, -0.40, 0.37, 2.53, -0.66, 0.43))
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
x <- c(
  -1.18, -0.25, -2.51, -2.02, -0.88, -1.63, -2.26, -2.35, -2.24, -0.21,
  -2.37, -1.33, -1.28, -0.32, -0.84, -1.37, 0.25, -1.12, -0.24, -0.67,
  0.27, -1.06, -0.32
)
x5 <- cbind(item = 1:length(x), xsi = x)
rownames(x5) <- items
colnames(x5)[1] <- ""

# store pre-scaled item parameters in list
xsi.fixed$cross[["EF"]] <-
  list(
    SC4 = list(
      w3 = x4w3,
      w7 = x4w7
    ),
    SC5 = list(w12 = x5)
  )
xsi.fixed$long[["EF"]] <-
  list(
    SC4 = list(
      w3 = x4w3,
      w7 = x4w7long
    ),
    SC5 = list(w12 = x5)
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
x <- c(
  -1.74, -.21, -1.10, -.87, -.09, -.79, -1.33, -.32, -.74, -.95, -1.36,
  -.23, -1.45, .11, -.14, -1.84, 1.07, -.36, -1.23, .08, -1.72, -.88,
  .30, -.52, -.82, -.04, .38, .56, .45, # .69#, .62,
  .63, -1.05, -.17, .52, -.27
)
x5 <- cbind(item = 1:length(x), xsi = x)
rownames(x5) <- items
colnames(x5)[1] <- ""

xsi.fixed$cross[["BA"]][["SC5"]][["w7"]] <- x5

### ----------------------------------------------------------------------------
### Listening comprehension Russian
### ----------------------------------------------------------------------------

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
x4 <- cbind(item = 1:length(items),
            xsi = c(-1.48, 0.86, -0.29, -1.23, -0.51, -0.37, 0.01, -0.66,
                    0.89, -0.13, -0.89, -0.02, 0.76, 0.55, -0.10, -0.28,
                    -0.73, -0.16, 0.79, 0.16, -0.34, -0.18, -0.36, -0.44,
                    0.75, -0.56, 0.14, -0.55, -1.17, 1.41, 0.26))
rownames(x4) <- items
colnames(x4)[1] <- ""

xsi.fixed$cross[["NR"]][["SC4"]][["w2"]] <- x4

### ----------------------------------------------------------------------------
### Listening comprehension Turkish
### ----------------------------------------------------------------------------

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
x4 <- cbind(item = 1:length(items),
            xsi = c(-1.46, -0.90, -0.53, -1.78, -0.28, -0.82, -0.41, -0.01,
                    0.06, 0.39, -0.96, 0.01, 0.71, 0.86, -0.15, -0.37, -0.13,
                    -1.33, 0.95, -0.31, -1.02, 0.02, -0.15, -0.34, 1.30,
                    -0.05, 0.02, -0.44, -0.22, 0.29, -0.03))
rownames(x4) <- items
colnames(x4)[1] <- ""

xsi.fixed$cross[["NT"]][["SC4"]][["w2"]] <- x4

### ----------------------------------------------------------------------------
### Scientific thinking
### ----------------------------------------------------------------------------

# SC3

items <- c(
  "stg12nhs_sc3g12_c", "stg12egs_sc3g12_c", "stg12mts_sc3g12_c",
  "stg12cws_sc3g12_c", "stg12pds_sc3g12_c"
)
x3 <- cbind(item = 1:length(items),
            xsi = c(-0.24, 0.12, -0.19, -0.34, -0.30))
rownames(x3) <- items
colnames(x3)[1] <- ""

xsi.fixed$cross[["ST"]][["SC3"]][["w9"]] <- x3

# SC4

items <- c(
  "stg12nhs_c", "stg12egs_c", "stg12mts_c", "stg12cws_c", "stg12pds_c"
)
x4 <- cbind(item = 1:length(items),
            xsi = c(-0.24, 0.12, -0.19, -0.34, -0.30))
rownames(x4) <- items
colnames(x4)[1] <- ""

xsi.fixed$cross[["ST"]][["SC4"]][["w7"]] <- x4



# save fixed item parameters
save(xsi.fixed, file = "data-raw/xsi_fixed.RData")
