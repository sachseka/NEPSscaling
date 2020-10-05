## Link constants and correction terms for starting cohort 1
library(dplyr)

# output object
link_constant <- list()
load(file = "data-raw/link_constant.RData")

# starting cohort
SC <- "SC1"
link_constant[[SC]] <- list()

# Mathematics
data <- haven::read_sav("../SUFs/SC1/SC1_xTargetCompetencies_D_7-0-0.sav") # no TR
link_constant[[SC]][["MA"]][["w7"]] <- as.numeric(data %>%
    filter(wave_w7 == 1 & wave_w5 == 1) %>%
    select(man5_sc1, man7_sc1u) %>%
    mutate(const = man7_sc1u - man5_sc1) %>%
    summarise_at(vars(const), mean, na.rm = TRUE))
# link_constant[[SC]][["MA"]][["w9"]] <- as.numeric(data %>%
#     filter(wave_w7 == 1 & wave_w9 == 1) %>%
#     select(man9_sc1u, man7_sc1u) %>%
#     mutate(const = man9_sc1u - man7_sc1u) %>%
#     summarise_at(vars(const), mean, na.rm = TRUE))

# Science
# link_constant[[SC]][["SC"]][["w8"]] <- as.numeric(data %>%
#     filter(wave_w8 == 1 & wave_w6 == 1) %>%
#     select(scn8_sc1u, scn6_sc1u) %>%
#     mutate(const = scn8_sc1u - scn6_sc1u) %>%
#     summarise_at(vars(const), mean, na.rm = TRUE))

# Vocabulary
# link_constant[[SC]][["VO"]][["w6"]] <- as.numeric(data %>%
#     filter(wave_w4 == 1 & wave_w6 == 1) %>%
#     select(von4_sc1u, von6_sc1u) %>%
#     mutate(const = von6_sc1u - von4_sc1u) %>%
#     summarise_at(vars(const), mean, na.rm = TRUE))
# link_constant[[SC]][["VO"]][["w8"]] <- as.numeric(data %>%
#     filter(wave_w8 == 1 & wave_w6 == 1) %>%
#     select(von8_sc1u, von6_sc1u) %>%
#     mutate(const = von8_sc1u - von6_sc1u) %>%
#     summarise_at(vars(const), mean, na.rm = TRUE))

rm(data)

save(link_constant, file = "data-raw/link_constant.RData")


# # Estimate item parameters for SC1 CD wave 1 (no TR)
# --> taken from not-published technical report!
# --> NOT: mean deviation of 1.7 with tr data!
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC1/SC1_xDirectMeasures_D_7-0-0.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select(contains("cdn")) %>%
    filter(!is.na(cdn1_sc1)) %>%
    select(-cdn1_sc1, -cdn1_sc2, -cdn10001, -cdn10002_c, -cdn1c015_c,
           -cdn1c016_c)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC1_CD_w1 <- mod$xsi.fixed.estimated
save(item_difficulty_SC1_CD_w1,
     file = "data-raw/item_difficulty_SC1_CD_w1.RData")


# Estimate item parameters for SC1 MA wave 7 (no TR)
library(haven)
library(dplyr)
library(TAM)
suf <- read_spss("../SUFs/SC1/SC1_xTargetCompetencies_D_7-0-0.sav")
dat <- suf %>%
    arrange(ID_t) %>%
    select("man7z211_c", "man7z201_c", "man5v181_sc1n7_c", "man7z101_c",
           "man7r111_c", "man7g051_c", "man7g061_c", "man7v011_c", "man7r151_c",
           "man7g131_c", "man7z041_c", "man7d071_c", "man7g191_c", "man7r121_c",
           "man7z081_c", "man7v091_c", "man7z171_c", "man7d021_c", "man7z221_c",
           "man5z081_sc1n7_c", "man7g031_c", "man7z231_c", "man7r181_c", "man7v161_c",
           "man7z141_c", "man7_sc1") %>%
    filter(!is.na(man7_sc1)) %>%
    select(-man7_sc1)
apply(dat, 2, table, useNA = "always")
mod <- tam.mml(
    resp = dat, irtmodel = "1PL", verbose = FALSE
)
item_difficulty_SC1_MA_w7 <- mod$xsi.fixed.estimated
save(item_difficulty_SC1_MA_w7,
     file = "data-raw/item_difficulty_SC1_MA_w7.RData")

# # Vocabulary waves 4, 6
# dat <- suf %>%
#     arrange(ID_t) %>%
#     select("von4_sc3", "von40001_c", "von40002_c", "von40003_c", "von40004_c",
#            "von40005_c", "von40006_c", "von40007_c", "von40008_c", "von40009_c",
#            "von40010_c", "von40011_c", "von40012_c","von40013_c", "von40014_c",
#            "von40015_c", "von40016_c", "von40017_c", "von40018_c", "von40019_c",
#            "von40020_c", "von40021_c", "von40022_c", "von40023_c", "von40024_c",
#            "von40025_c", "von40026_c", "von40027_c", "von40028_c", "von40029_c",
#            "von40030_c", "von40031_c", "von40032_c", "von40033_c","von40034_c",
#            "von40035_c", "von40036_c", "von40037_c", "von40038_c", "von40039_c",
#            "von40040_c","von40041_c", "von40042_c", "von40043_c", "von40044_c",
#            "von40045_c", "von40046_c", "von40047_c","von40048_c", "von40049_c",
#            "von40050_c", "von40051_c", "von40052_c", "von40053_c", "von40054_c",
#            "von40055_c", "von40056_c", "von40057_c", "von40058_c", "von40059_c",
#            "von40060_c", "von40061_c","von40062_c", "von40063_c", "von40064_c",
#            "von40065_c", "von40066_c", "von40067_c", "von40068_c","von40069_c",
#            "von40070_c", "von40071_c", "von40072_c", "von40073_c", "von40074_c",
#            "von40075_c","von40076_c", "von40077_c", "von40078_c", "von40079_c",
#            "von40080_c", "von40081_c", "von40082_c","von40083_c", "von40084_c",
#            "von40085_c", "von40086_c", "von40087_c", "von40088_c", "von40089_c",
#            "von40090_c", "von40091_c", "von40092_c", "von40093_c", "von40094_c",
#            "von40095_c", "von40096_c","von40097_c", "von40098_c", "von40099_c",
#            "von40100_c", "von40101_c", "von40102_c", "von40103_c","von40104_c",
#            "von40105_c", "von40106_c", "von40107_c", "von40108_c", "von40109_c",
#            "von40110_c","von40111_c", "von40112_c", "von40113_c", "von40114_c",
#            "von40115_c", "von40116_c", "von40117_c","von40118_c", "von40119_c",
#            "von40120_c", "von40121_c", "von40122_c", "von40123_c", "von40124_c",
#            "von40125_c", "von40126_c", "von40127_c", "von40128_c", "von40129_c",
#            "von40130_c", "von40131_c","von40132_c", "von40133_c", "von40134_c",
#            "von40135_c", "von40136_c", "von40137_c", "von40138_c","von40139_c",
#            "von40140_c", "von40141_c", "von40142_c", "von40143_c", "von40144_c",
#            "von40145_c","von40146_c", "von40147_c", "von40148_c", "von40149_c",
#            "von40150_c", "von40151_c", "von40152_c","von40153_c", "von40154_c",
#            "von40155_c", "von40156_c", "von40157_c", "von40158_c", "von40159_c",
#            "von40160_c", "von40161_c", "von40162_c", "von40163_c", "von40164_c",
#            "von40165_c", "von40166_c","von40167_c", "von40168_c", "von40169_c",
#            "von40170_c", "von40171_c", "von40172_c", "von40173_c","von40174_c",
#            "von40175_c", "von40176_c", "von40177_c", "von40178_c", "von40179_c",
#            "von40180_c","von40181_c", "von40182_c", "von40183_c", "von40184_c",
#            "von40185_c", "von40186_c", "von40187_c","von40188_c", "von40189_c",
#            "von40190_c", "von40191_c", "von40192_c", "von40193_c", "von40194_c",
#            "von40195_c", "von40196_c", "von40197_c", "von40198_c", "von40199_c",
#            "von40200_c", "von40201_c","von40202_c", "von40203_c", "von40204_c",
#            "von40205_c", "von40206_c", "von40207_c", "von40208_c","von40209_c",
#            "von40210_c","von40211_c", "von40212_c", "von40213_c", "von40214_c",
#            "von40215_c","von40216_c", "von40217_c", "von40218_c", "von40219_c",
#            "von40220_c", "von40221_c", "von40222_c","von40223_c", "von40224_c",
#            "von40225_c", "von40226_c", "von40227_c", "von40228_c") %>%
#     filter(!is.na(von4_sc3)) %>%
#     select(-von4_sc3) %>%
#     select(which(colSums(!is.na(dat)) > 2))
# apply(dat, 2, table, useNA = "always")
# mod <- tam.mml(
#     resp = dat, irtmodel = "1PL", verbose = FALSE
# )
# item_difficulty_SC1_VO_w4 <- mod$xsi.fixed.estimated
# save(item_difficulty_SC1_VO_w4,
#      file = "data-raw/item_difficulty_SC1_VO_w4.RData")
