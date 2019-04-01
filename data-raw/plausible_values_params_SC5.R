## Link constants and correction terms for starting cohort 4

rm(list = ls())

# output object
meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC5'

# Reading
meanvar[[SC]][["RE"]][["w12"]] <- c(0.260422881116179, 0.8320544)

# Mathematics
meanvar[[SC]][["MA"]][["w12"]] <- c(-0.009668033, 1.16)

# Information and Communication Technology literacy
meanvar[[SC]][["IC"]][["w5"]] <- c(0, 1) #!

# Science
meanvar[[SC]][["SC"]][["w5"]] <- c(0, 1) #!

# English as a foreign language
meanvar[[SC]][["EF"]][["w12"]] <- c(0, 1) #!

# Business Administration and Economics
meanvar[[SC]][["BA"]][["w7"]] <- c(0,1) #!

save(meanvar, file = "data-raw/meanvar.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w12"]] <- 0 #!
correction[[SC]][["MA"]] <- list()
correction[[SC]][["MA"]][["w12"]] <- 0 #!

save(correction, file = "data-raw/correction.RData")
