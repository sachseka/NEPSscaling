## Link constants and correction terms for starting cohort 2

# output object
meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC2'

# Reading
meanvar[[SC]][["RE"]][["w6"]] <- c(-0.567, 1.536)

# Mathematics
meanvar[[SC]][["MA"]][["w2"]] <- c(0, 1.087) #!
meanvar[[SC]][["MA"]][["w3"]] <- c(1.3522, 0.967)
meanvar[[SC]][["MA"]][["w4"]] <- c(-0.003803296, 1) #!
meanvar[[SC]][["MA"]][["w6"]] <- c(0, 1) #!

# Information and Communication Technology literacy
meanvar[[SC]][["IC"]][["w5"]] <- c(0, 0.24) #!

# Science
meanvar[[SC]][["SC"]][["w3"]] <- c(0.2875, 0.635) #!
meanvar[[SC]][["SC"]][["w5"]] <- c(0.0132, 1) #!

# Native Russian
meanvar[[SC]][["NR"]][["w4"]] <- c(0,1) #!

# Native Turkish
meanvar[[SC]][["NT"]][["w4"]] <- c(0,1) #!

# Vocabulary
meanvar[[SC]][["VO"]][["w3"]] <- c(0,1) #!
meanvar[[SC]][["VO"]][["w5"]] <- c(0,1) #!

# Grammar
meanvar[[SC]][["GR"]][["w3"]] <- c(0,1) #!

save(meanvar, file = "data-raw/meanvar.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w7"]] <- 0 #!
correction[[SC]][["RE"]][["w10"]] <- 0 #!

save(correction, file = "data-raw/correction.RData")
