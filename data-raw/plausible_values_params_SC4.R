## Link constants and correction terms for starting cohort 4

rm(list = ls())

# output object
meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC4'

# Reading
meanvar[[SC]][["RE"]][["w7"]] <- c(0.045, 0.829)
meanvar[[SC]][["RE"]][["w10"]] <- c(0.049406366423006, 0.8320544)#+0.045

# Mathematics
meanvar[[SC]][["MA"]][["w7"]] <- c(0.496, 0.989) # c=0.5162951 über R-Code
meanvar[[SC]][["MA"]][["w10"]] <- c(0.3194437, 1.16)#+0.496

# Information and Communication Technology literacy
meanvar[[SC]][["IC"]][["w7"]] <- c(0.687, 0.46)

# Science
meanvar[[SC]][["SC"]][["w5"]] <- c(0, 0.475) #!

# Native Russian
meanvar[[SC]][["NR"]][["w2"]] <- c(0,1) #!

# Native Turkish
meanvar[[SC]][["NT"]][["w2"]] <- c(0,1) #!

# English as a foreign language
meanvar[[SC]][["EF"]][["w3"]] <- c(0, 1.88) #!
meanvar[[SC]][["EF"]][["w7"]] <- c(0, 1.6957) #!

# Scientific thinking
meanvar[[SC]][["ST"]][["w7"]] <- c(0,1) #!

save(meanvar, file = "data-raw/meanvar.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]][["w7"]] <- 0.487882
correction[[SC]][["RE"]][["w10"]] <- c(0,0) #!

save(correction, file = "data-raw/correction.RData")
