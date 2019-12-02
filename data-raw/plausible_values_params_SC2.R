## Link constants and correction terms for starting cohort 2

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- "SC2"

# Reading
link_constant[[SC]][["RE"]][["w6"]] <- -0.567

# Mathematics
link_constant[[SC]][["MA"]][["w2"]] <- 0 # !
link_constant[[SC]][["MA"]][["w3"]] <- 1.3522
link_constant[[SC]][["MA"]][["w4"]] <- -0.003803296
link_constant[[SC]][["MA"]][["w6"]] <- 0 # !

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w5"]] <- 0 # !

# Science
link_constant[[SC]][["SC"]][["w3"]] <- 0.2875 # !
link_constant[[SC]][["SC"]][["w5"]] <- 0.0132

# Native Russian
link_constant[[SC]][["NR"]][["w4"]] <- 0 # !

# Native Turkish
link_constant[[SC]][["NT"]][["w4"]] <- 0 # !

# Vocabulary
link_constant[[SC]][["VO"]][["w3"]] <- 0 # !
link_constant[[SC]][["VO"]][["w5"]] <- 0 # !

# Grammar
link_constant[[SC]][["GR"]][["w3"]] <- 0 # !

save(link_constant, file = "data-raw/link_constant.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w7"]] <- 0 # !
correction[[SC]][["RE"]][["w10"]] <- 0 # !

save(correction, file = "data-raw/correction.RData")
