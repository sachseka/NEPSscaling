## Link constants and correction terms for starting cohort 4

rm(list = ls())

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- 'SC5'

# Reading
link_constant[[SC]][["RE"]][["w12"]] <- 0.260422881116179

# Mathematics
link_constant[[SC]][["MA"]][["w12"]] <- -0.009668033

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w5"]] <- 0 #!

# Science
link_constant[[SC]][["SC"]][["w5"]] <- 0 #!

# English as a foreign language
link_constant[[SC]][["EF"]][["w12"]] <- 0 #!

# Business Administration and Economics
link_constant[[SC]][["BA"]][["w7"]] <- 0 #!

save(link_constant, file = "data-raw/link_constant.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w12"]] <- 0 #!
correction[[SC]][["MA"]] <- list()
correction[[SC]][["MA"]][["w12"]] <- 0 #!

save(correction, file = "data-raw/correction.RData")
