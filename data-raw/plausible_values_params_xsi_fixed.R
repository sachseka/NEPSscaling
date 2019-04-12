## Fixed item parameters for:
## - SC4 math/reading wave 10
## - SC5 math/reading wave 12
## - SC6 math/reading wave 9
## The same test forms were administered to the three starting cohorts and
## scaled together.
## To ensure comparability the item parameters of main scalings are used.
rm(list = ls())

# fixed item parameters
xsi.fixed <- list(cross = list(), long = list())

### mathematical competence

load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/MA/Version 3/output/data/data.Rdata")
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/MA/Version 3/output/data/pcm.Rdata")

xsi <- pcm$capi$mod$xsi.fixed.estimated
xsi <- xsi[1:length(items$poly$suf4), ]

## check whether item sequence corresponds to internally stored sequence
# change rownames to correct item names
rn <- rownames(xsi)
rn <- gsub("a8", "a9", x = rn)
rn <- gsub("a4", "a9", x = rn)
rn <- gsub("a2", "a3", x = rn)
rn <- gsub("gc", "g12", x = rn)
rn[rn == "mag2g12s_sc6a9_c"] <- "mag12q111_sc6a9_c"
rn[rn == "maa3d111_sc6a9_c"] <- "mas1d081_sc6a9_c"
rn[grep("mag12", rn)] <- grep("mag12", items$poly$suf6, value = T) # item positions match!

View(cbind(rownames(xsi), rn, NEPStools:::item_labels$SC6$MA$w9,
           items$poly$suf6, items$poly$suf5, items$poly$suf4))

# store pre-scaled item parameters in list
l4 <- length(NEPStools:::item_labels$SC4$MA$w1) + length(NEPStools:::item_labels$SC4$MA$w7)
l5 <- length(NEPStools:::item_labels$SC5$MA$w1)
l6 <- length(NEPStools:::item_labels$SC6$MA$w3)
xsi.fixed$cross[["MA"]] <- cbind(1:nrow(xsi), unname(xsi[, 2]))
xsi.fixed$long[["MA"]] <- list(SC4 = cbind((l4+1):(l4+nrow(xsi)), unname(xsi[, 2])),
                               SC5 = cbind((l5+1):(l5+nrow(xsi)), unname(xsi[, 2])),
                               SC6 = cbind((l6+1):(l6+nrow(xsi)), unname(xsi[, 2])))

### reading competence

load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/RE/Version 2/02. Output/data/dat.Rdata")
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B116_SC6/RE/Version 2/02. Output/data/pcm.Rdata")

xsi <- pcm$all$mod$xsi.fixed.estimated
xsi <- xsi[!grepl("step", rownames(xsi)), ]

# match correct difficulties to cohort
xsi.sc4 <- xsi[c(1:6, 8:13, 16, 18:42),]
xsi.sc5 <- xsi[c(1:6, 8:12, 15:16, 18:42),]
xsi.sc6 <- xsi[c(1:5, 7:12, 14, 17:42),]

## check whether item sequence corresponds to internally stored sequence
# change rownames to correct item names
# SC4
rn <- rownames(xsi.sc4)
rn <- gsub("A8", "a9", x = rn)
rn <- paste0(substr(rn, 1, 8), substr(rn, 10, 10), "_sc4a10_c")
rownames(xsi.sc4) <- rn
xsi.sc4 <- xsi.sc4[rownames(xsi.sc4) %in% NEPStools:::item_labels$SC4$RE$w10, ]
View(cbind(rownames(xsi.sc4), NEPStools:::item_labels$SC4$RE$w10))



# SC5
rn <- rownames(xsi.sc5)
rn[rn == "reA8030800_c"] <- "res1203080_c"
rn[rn == "reA8040500_c"] <- "res1204050_c"
rn <- gsub("A8", "a9", x = rn)
rn[grepl("rea9", rn)] <- paste0(substr(rn[grepl("rea9", rn)], 1, 8),
                                substr(rn[grepl("rea9", rn)], 10, 10),
                                "_sc5s12_c")
rownames(xsi.sc5) <- rn
xsi.sc5 <- xsi.sc5[rownames(xsi.sc5) %in% NEPStools:::item_labels$SC5$RE$w12, ]
View(cbind(rownames(xsi.sc5), NEPStools:::item_labels$SC5$RE$w12))



# SC6
rn <- rownames(xsi.sc6)
rn <- gsub("A8", "a9", x = rn)
rn <- paste0(substr(rn, 1, 8), substr(rn, 10, 12))
rownames(xsi.sc6) <- rn
xsi.sc6 <- xsi.sc6[rownames(xsi.sc6) %in% NEPStools:::item_labels$SC6$RE$w9, ]
View(cbind(rownames(xsi.sc6), NEPStools:::item_labels$SC6$RE$w9))


# store pre-scaled item parameters in list
xsi.fixed$cross[["RE"]] <- list(SC4 = cbind(1:nrow(xsi.sc4), unname(xsi.sc4[,2])),
                                SC5 = cbind(1:nrow(xsi.sc5), unname(xsi.sc5[,2])),
                                SC6 = cbind(1:nrow(xsi.sc6), unname(xsi.sc6[,2])))
l4 <- length(NEPStools:::item_labels$SC4$RE$w2) + length(NEPStools:::item_labels$SC4$RE$w7)
l5 <- length(NEPStools:::item_labels$SC5$RE$w1)
l6 <- length(NEPStools:::item_labels$SC6$RE$w3)
xsi.fixed$long[["RE"]] <- list(SC4 = cbind((l4+1):(l4+nrow(xsi.sc4)), unname(xsi.sc4[, 2])),
                               SC5 = cbind((l5+1):(l5+nrow(xsi.sc5)), unname(xsi.sc5[, 2])),
                               SC6 = cbind((l6+1):(l6+nrow(xsi.sc6)), unname(xsi.sc6[, 2])))


## additionally SC5 EF w12 is fixed because the scalings are based on the CBT
## sample alone, but the SUF contains WBT also (not discriminable)
load("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Skalierung/HE/B114_SC5/EF/Version 1/output/data/pcm.Rdata")

xsi <- pcm$all$mod$xsi.fixed.estimated#all$mod$xsi.fixed.estimated
xsi <- xsi[!grepl("step", rownames(xsi)), ]

xsi.fixed$cross[["EF"]] <- xsi



# save fixed item parameters
save(xsi.fixed, file = "data-raw/xsi_fixed.RData")
