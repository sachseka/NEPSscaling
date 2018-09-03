## Link constants and correction terms for starting cohort 3

meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC3'

# Reading
meanvar[[SC]][["RE"]][["w3"]] <- c(0.1371353,1) #! # 1.124 (easy), 1.462 (difficult); muss man hier den Mittelwert nehmen für die Varianz?
meanvar[[SC]][["RE"]][["w6"]] <- c(0.579, 1.02)

# Mathematics
meanvar[[SC]][["MA"]][["w3"]] <- c(0.771, 1.156)
meanvar[[SC]][["MA"]][["w5"]] <- c(0.794, 1.197)

# Information and Communication Technology literacy
meanvar[[SC]][["IC"]][["w2"]] <- c(0, 0.40) #!
meanvar[[SC]][["IC"]][["w5"]] <- c(1.042, 0.64)
meanvar[[SC]][["IC"]][["w8"]] <- c(0, 1) #!

# Science
meanvar[[SC]][["SC"]][["w2"]] <- c(0, 1.082) #!

# Native Russian
meanvar[[SC]][["NR"]][["w"]] <- c(0,1) #!

# Native Turkish
meanvar[[SC]][["NT"]][["w"]] <- c(0,1) #!

# English as a foreign language
meanvar[[SC]][["EF"]][["w7"]] <- c(-0.201, 2.09) #!

# Scientific thinking
meanvar[[SC]][["ST"]][["w"]] <- c(0,1) #!

# Vocabulary
meanvar[[SC]][["VO"]][["w2"]] <- c(0,1) #!

# Listening
meanvar[[SC]][["LI"]][["w6"]] <- c(0,1) #!

save(meanvar, file = "data-raw/meanvar.RData")

# Corrections because of dropout etc.
correction <- list()
load(file = "data-raw/correction.RData")
correction[[SC]][["RE"]][["w7"]] <- 0 #!
correction[[SC]][["RE"]][["w10"]] <- c(0,0) #!

save(correction, file = "data-raw/correction.RData")
