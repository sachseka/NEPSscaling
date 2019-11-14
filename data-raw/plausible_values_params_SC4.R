## Link constants and correction terms for starting cohort 4

rm(list = ls())

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- 'SC4'

# Reading
link_constant[[SC]][["RE"]][["w7"]] <- 0.045
link_constant[[SC]][["RE"]][["w10"]] <- 0.049406366423006

# Mathematics
link_constant[[SC]][["MA"]][["w7"]] <- 0.496
link_constant[[SC]][["MA"]][["w10"]] <- 0.3194437

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w7"]] <- 0.687

# Science
link_constant[[SC]][["SC"]][["w5"]] <- 0 #!

# Native Russian
link_constant[[SC]][["NR"]][["w2"]] <- 0 #!

# Native Turkish
link_constant[[SC]][["NT"]][["w2"]] <- 0 #!

# English as a foreign language
link_constant[[SC]][["EF"]][["w3"]] <- 0 #!
link_constant[[SC]][["EF"]][["w7"]] <- 0 #!

# Scientific thinking
link_constant[[SC]][["ST"]][["w7"]] <- 0 #!

save(link_constant, file = "data-raw/link_constant.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w2"]] <- 0
correction[[SC]][["RE"]][["w7"]] <- 0.487882
correction[[SC]][["RE"]][["w10"]] <- 0 #!

save(correction, file = "data-raw/correction.RData")
