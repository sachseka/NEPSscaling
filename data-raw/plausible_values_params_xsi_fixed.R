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
# load("data-raw/item_difficulties.RData")

xsi <- pcm$capi$mod$xsi.fixed.estimated
xsi <- xsi[1:length(items$poly$suf4), ]

## check whether item sequence corresponds to internally stored sequence
# change rownames to correct item names
# rn <- rownames(xsi)
# rn <- gsub("a8", "a9", x = rn)
# rn <- gsub("a4", "a9", x = rn)
# rn <- gsub("a2", "a3", x = rn)
# rn <- gsub("gc", "g12", x = rn)
# rn[rn == "mag2g12s_sc6a9_c"] <- "mag12q111_sc6a9_c"
# rn[rn == "maa3d111_sc6a9_c"] <- "mas1d081_sc6a9_c"
# rn[grep("mag12", rn)] <- grep("mag12", items$poly$suf6, value = T) # item positions match!
#
# View(cbind(rownames(xsi), rn, NEPScaling:::item_labels$SC6$MA$w9,
#            items$poly$suf6, items$poly$suf5, items$poly$suf4))
# rm(rn)

x4 <- xsi
rownames(x4) <- NEPScaling:::item_labels$SC4$MA$w10
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
      w1 = NULL,
      w7 = NULL,
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
      w1 = NULL,
      w7 = NULL,
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
# load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/RE/Version 2/02. Output/data/dat.Rdata")
# load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/RE/Version 2/02. Output/data/pcm.Rdata")
#
# xsi <- pcm$all$mod$xsi.fixed.estimated
# xsi <- xsi[!grepl("step", rownames(xsi)), ]
xsi <- readODS::read_ods(
  path = "../b110_114_116_alte_schwierigkeiten.ods",
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


# ## check whether item sequence corresponds to internally stored sequence
# # change rownames to correct item names
# # SC4
# rn <- rownames(xsi.sc4)
# rn <- gsub("A8", "a9", x = rn)
# rn <- paste0(substr(rn, 1, 8), substr(rn, 10, 10), "_sc4a10_c")
# rownames(xsi.sc4) <- rn
# xsi.sc4 <- xsi.sc4[rownames(xsi.sc4) %in% item_labels$SC4$RE$w10, ]
# View(cbind(rownames(xsi.sc4), item_labels$SC4$RE$w10))
#
#
#
# # SC5
# rn <- rownames(xsi.sc5)
# rn[rn == "reA8030800_c"] <- "res1203080_c"
# rn[rn == "reA8040500_c"] <- "res1204050_c"
# rn <- gsub("A8", "a9", x = rn)
# rn[grepl("rea9", rn)] <- paste0(substr(rn[grepl("rea9", rn)], 1, 8),
#                                 substr(rn[grepl("rea9", rn)], 10, 10),
#                                 "_sc5s12_c")
# rownames(xsi.sc5) <- rn
# xsi.sc5 <- xsi.sc5[rownames(xsi.sc5) %in% item_labels$SC5$RE$w12, ]
# View(cbind(rownames(xsi.sc5), item_labels$SC5$RE$w12))
#
#
#
# # SC6
# rn <- rownames(xsi.sc6)
# rn <- gsub("A8", "a9", x = rn)
# rn <- paste0(substr(rn, 1, 8), substr(rn, 10, 12))
# rownames(xsi.sc6) <- rn
# xsi.sc6 <- xsi.sc6[rownames(xsi.sc6) %in% item_labels$SC6$RE$w9, ]
# View(cbind(rownames(xsi.sc6), item_labels$SC6$RE$w9))
# wave 5 sample refreshment
load("data-raw/item_diff_SC6_RE_w3.RData")
item_diff_SC6_RE_w3 <- item_diff_SC6_RE_w3[1:30, ]

# store pre-scaled item parameters in list
xsi.fixed$cross[["RE"]] <-
  list(
    SC4 = list(
      w2 = NULL,
      w7 = NULL,
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
# l4 <- length(item_labels$SC4$RE$w2) + length(item_labels$SC4$RE$w7)
# l5 <- length(item_labels$SC5$RE$w1)
# l6 <- length(item_labels$SC6$RE$w3)
xsi.fixed$long[["RE"]] <-
  list(
    SC4 = list(
      w2 = NULL,
      w7 = NULL,
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
      w1 = NULL,
      w7 = NULL,
      w13 = NULL
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )
xsi.fixed$long[["IC"]] <-
  list(
    SC4 = list(
      w1 = NULL,
      w7 = NULL,
      w13 = NULL
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )


### ----------------------------------------------------------------------------
### Science competence
### ----------------------------------------------------------------------------


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
      w5 = NULL,
      w13 = NULL
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )
xsi.fixed$long[["SC"]] <-
  list(
    SC4 = list(
      w5 = NULL,
      w13 = NULL
    ),
    SC5 = list(w5 = x5),
    SC6 = list(w5 = x6)
  )

### ----------------------------------------------------------------------------
### English as a foreign language
### ----------------------------------------------------------------------------

# SC5

## additionally SC5 EF w12 is fixed because the scalings are based on the CBT
## sample alone, but the SUF contains WBT also (not discriminable)
# load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B114_SC5/EF/Version 1/output/data/pcm.Rdata")
#
# xsi <- pcm$all$mod$xsi.fixed.estimated
# xsi <- xsi[!grepl("step", rownames(xsi)), ]
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

xsi.fixed$cross[["EF"]][["SC5"]][["w12"]] <- x5

### ----------------------------------------------------------------------------
### Business Administration competence
### ----------------------------------------------------------------------------

# SC5

## additionally SC5 EF w12 is fixed because the scalings are based on the CBT
## sample alone, but the SUF contains WBT also (not discriminable)
# load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B114_SC5/EF/Version 1/output/data/pcm.Rdata")
#
# xsi <- pcm$all$mod$xsi.fixed.estimated
# xsi <- xsi[!grepl("step", rownames(xsi)), ]
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



# save fixed item parameters
save(xsi.fixed, file = "data-raw/xsi_fixed.RData")
