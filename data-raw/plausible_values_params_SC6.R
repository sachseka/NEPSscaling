## Link constants and correction terms for starting cohort 6

rm(list = ls())

# output object
meanvar <- list()
load(file = "data-raw/meanvar.RData")

# starting cohort
SC <- 'SC6'

# Reading
meanvar[[SC]][["RE"]][["w9"]] <- c(-0.104688810504687, 0.8320544)

# Mathematics
meanvar[[SC]][["MA"]][["w9"]] <- c(-0.1109509, 1.16) #! im technical report: pcm$all$mod: 1.23277

# Information and Communication Technology literacy
meanvar[[SC]][["IC"]][["w5"]] <- c(0, 1) #!

# Science
meanvar[[SC]][["SC"]][["w5"]] <- c(0, 1) #!

# Save output object
save(meanvar, file = "data-raw/meanvar.RData")


# Item difficulties for reading w3 (to be reused in w5)
domain <- 'RE'; wave <- 'w3'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 1, ]
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# rotation variable
position <- data.frame(ID_t = read$ID_t, position = rep(NA, nrow(read)))
position[position$ID_t %in% read$ID_t, 'position'] <- read[, 'tx80211_w3']
position[!is.na(position$position) & (position$position == 122 | position$position == 124), 'position'] <- 0 # maths first
position[!is.na(position$position) & (position$position == 123 | position$position == 125), 'position'] <- 1 # reading first
if (sum(is.na(position[, 2])) > 0) {
    position[is.na(position$position), 'position'] <- sample(0:1, length(position[is.na(position$position), 'position']), replace = T)
}
position <- position[, 2, drop = FALSE]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# estimation model
formulaA <- ~ 0 + item + item:step + position
design <- TAM::designMatrices.mfr2(resp = resp, formulaA = formulaA, facets = position, constraint = 'cases')
resp2 <- design$gresp$gresp.noStep
ind <- which(apply(resp2, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
A <- design$A$A.3d
xsi.elim <- design$xsi.elim
A <- A[, , -xsi.elim[, 2]]
B <- design$B$B.3d
B[ind, , ] <- 0.5 * B[ind, , ]
mod1 <- TAM::tam.mml(resp = resp2
                    , irtmodel = 'PCM2'
                    , A = A
                    , B = B
                    , verbose = FALSE
)
# wave 5
wave <- 'w5'
read <- data[rowSums(!is.na(data[, names(data) %in% item_labels[[SC]][[domain]][[wave]]])) >= nvalid, ]
read <- read[read$wave_w3 == 0 & read$wave_w5 == 1, ]
# test data
resp <- read[, names(read) %in% item_labels[[SC]][[domain]][[wave]]]
# model estimation
ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items
Q <- matrix(1, nrow = ncol(resp), ncol = 1)
Q[ind, ] <- 0.5 * Q[ind, ]
mod2 <- TAM::tam.mml(resp = resp
                    , irtmodel = 'PCM2'
                    , Q = Q
                    , verbose = FALSE
)
rownames(mod1$xsi.fixed.estimated) <- gsub(':', '_', rownames(mod1$xsi.fixed.estimated))
l <- merge(mod1$xsi.fixed.estimated, mod2$xsi.fixed.estimated, by = 0)
rownames(l) <- l[, 1]
l <- l[order(l[, 4]), c(4,3)]
colnames(l) <- colnames(mod2$xsi.fixed.estimated)
item_diff_SC6_RE_w3 <- as.matrix(l)
# clear environment
rm(mod1, mod2, read, resp, ind, wave, Q, A, B, design, l, position, resp2, xsi.elim, formulaA)
save(item_diff_SC6_RE_w3, file = 'data-raw/item_diff_SC6_RE_w3.RData')
