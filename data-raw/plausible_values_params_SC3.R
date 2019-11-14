## Link constants and correction terms for starting cohort 3

link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- 'SC3'

# Reading
link_constant[[SC]][["RE"]][["w3"]] <- 0.1371353
link_constant[[SC]][["RE"]][["w6"]] <- 0.579

# Mathematics
link_constant[[SC]][["MA"]][["w3"]] <- 0.771
link_constant[[SC]][["MA"]][["w5"]] <- 0.794

# Information and Communication Technology literacy
link_constant[[SC]][["IC"]][["w2"]] <- 0 #!
link_constant[[SC]][["IC"]][["w5"]] <- 1.042
link_constant[[SC]][["IC"]][["w8"]] <- 0 #!

# Science
link_constant[[SC]][["SC"]][["w2"]] <- 0 #!
link_constant[[SC]][["SC"]][["w5"]] <- 0
link_constant[[SC]][["SC"]][["w8"]] <- 0 #!

# Native Russian
link_constant[[SC]][["NR"]][["w3"]] <- 0 #!
link_constant[[SC]][["NR"]][["w6"]] <- 0 #!

# Native Turkish
link_constant[[SC]][["NT"]][["w3"]] <- 0 #!
link_constant[[SC]][["NT"]][["w6"]] <- 0 #!

# English as a foreign language
link_constant[[SC]][["EF"]][["w7"]] <- -0.201 #!

# Scientific thinking
link_constant[[SC]][["ST"]][["w"]] <- 0 #!

# Vocabulary
link_constant[[SC]][["VO"]][["w2"]] <- 0 #!

# Listening
link_constant[[SC]][["LI"]][["w6"]] <- 0 #!

save(link_constant, file = "data-raw/link_constant.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]] <- list()
correction[[SC]][["RE"]][["w7"]] <- 0 #!
correction[[SC]][["RE"]][["w10"]] <- 0 #!

save(correction, file = "data-raw/correction.RData")
