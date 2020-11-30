## Code providing the internal data (item difficulties and WLEs)
## for plausible values estimation.
## The parameters are prepared in advance to ensure that proper
## corrections for test design changes, dropout etc. are applied
rm(list = ls())

# latent mean and variance of ability
load(file = "data-raw/link_constant.RData")

# item labels for item selection
item_labels <-
  list(
    SC1 = list(
      CD = list(
        w1 = c("cdn1c001_c", "cdn1c002_c", "cdn1c003_c", "cdn1c004_c",
               "cdn1c005_c", "cdn1c006_c", "cdn1c007_c", "cdn1c008_c",
               "cdn1c009_c", "cdn1c010_c", "cdn1c011_c", "cdn1c012_c",
               "cdn1c013_c", "cdn1c014_c")#, "cdn1c015_c", "cdn1c016_c")
      ),
      MA = list(
        w5 = c("man5z17s_c", "man5z021_c", "man5v181_c", "man5z161_c", "man5r14s_c",
        "man5d191_c", "man5z051_c", "man5g151_c", "man5r131_c", "man5g111_c",
        "man5z121_c", "man5v041_c", "man5z081_c", "man5d091_c", "man5z201_c",
        "man5g101_c", "man5z011_c", "man5r071_c", "man5d031_c", "man5v061_c"),
        w7 = c("man7z211_c", "man7z201_c", "man5v181_sc1n7_c", "man7z101_c",
        "man7r111_c", "man7g051_c", "man7g061_c", "man7v011_c", "man7r151_c",
        "man7g131_c", "man7z041_c", "man7d071_c", "man7g191_c", "man7r121_c",
        "man7z081_c", "man7v091_c", "man7z171_c", "man7d021_c", "man7z221_c",
        "man5z081_sc1n7_c", "man7g031_c", "man7z231_c", "man7r181_c", "man7v161_c",
        "man7z141_c")#,
#        w9 = c()
      ),
      SC = list(
        w6 = c(
        "sck10420_sc1n6_c", "scn6130s_c", #"scn66000_c",
        "sck16120_sc1n6_c", "sck1102s_sc1n6_c", "sck11030_sc1n6_c",
        "sck11110_sc1n6_c", "sck11120_sc1n6_c", "sck16010_sc1n6_c",
        "sck16020_sc1n6_c", "sck10510_sc1n6_c", "sck10530_sc1n6_c",
        "sck1162s_sc1n6_c", "sck10710_sc1n6_c", "sck10720_sc1n6_c",
        "scn60100_c", "sck11330_sc1n6_c", "sck10910_sc1n6_c",
        "scn61800_c", "sck16210_sc1n6_c")#,
#        w8 = c()
      )
    ),
    SC2 = list(
      RE = list(
        w6 = c("reg50110_sc2g4_c","reg5012s_sc2g4_c","reg50130_sc2g4_c","reg50140_sc2g4_c",
               "reg50150_sc2g4_c","reg5016s_sc2g4_c","reg50170_sc2g4_c","reg50210_sc2g4_c",
               "reg50220_sc2g4_c","reg50230_sc2g4_c","reg50240_sc2g4_c","reg50250_sc2g4_c",
               "reg5026s_sc2g4_c","reg50310_sc2g4_c","reg50320_sc2g4_c","reg50330_sc2g4_c",
               "reg50340_sc2g4_c","reg50350_sc2g4_c","reg50360_sc2g4_c","reg50370_sc2g4_c",
               "reg50410_sc2g4_c","reg5042s_sc2g4_c","reg50430_sc2g4_c","reg50440_sc2g4_c",
               "reg50460_sc2g4_c","reg50510_sc2g4_c","reg5052s_sc2g4_c","reg50530_sc2g4_c",
               "reg50540_sc2g4_c","reg5055s_sc2g4_c",#"reg50560_sc2g4_c",
               "reg50570_sc2g4_c"),
        w9 = c("reg70110_sc2g7_c", "reg70120_sc2g7_c", "reg7013s_sc2g7_c",
               "reg70140_sc2g7_c", "reg7015s_sc2g7_c", "reg7016s_sc2g7_c",
               "reg70210_sc2g7_c", "reg70220_sc2g7_c", "reg7023s_sc2g7_c",
               "reg7024s_sc2g7_c", #"reg7024s_sc2g7_c_d"
               "reg70250_sc2g7_c", "reg7026s_sc2g7_c", "reg70310_sc2g7_c",
               "reg70320_sc2g7_c", "reg7033s_sc2g7_c", #"reg7033s_sc2g7_c_d"
               "reg70340_sc2g7_c", "reg70350_sc2g7_c", "reg70360_sc2g7_c",
               "reg70410_sc2g7_c", "reg70420_sc2g7_c", "reg70430_sc2g7_c",
               "reg70440_sc2g7_c", "reg7045s_sc2g7_c", #"reg7045s_sc2g7_c_d"
               "reg70460_sc2g7_c", "reg7051s_sc2g7_c", "reg70520_sc2g7_c",
               "reg7053s_sc2g7_c", "reg70540_sc2g7_c", "reg7055s_sc2g7_c",
               "reg70560_sc2g7_c", "reg70610_sc2g7_c", "reg70620_sc2g7_c",
               "reg7063s_sc2g7_c", "reg70640_sc2g7_c", "reg70650_sc2g7_c",
               "reg7066s_sc2g7_c", "reg70670_sc2g7_c", "reg7071s_sc2g7_c",
               "reg70720_sc2g7_c", "reg70730_sc2g7_c", "reg70740_sc2g7_c",
               "reg7075s_sc2g7_c")
      ),
      MA = list(
        w2 = c("mak2z221_c","mak2z231_c","mak2z101_c","mak2r111_c",
               "mak2g041_c","mak2g051_c","mak2v001_c","mak2r151_c",
               "mak2z031_c","mak2d062_c","mak2z161_c","mak2z171_c",
               "mak2g211_c","mak2r131_c","mak2z091_c","mak2v08s_c",
               "mak2z201_c","mak2d011_c","mak2z241_c","mak2z121_c",
               "mak2v071_c","mak2g021_c","mak2z251_c","mak2r191_c",
               "mak2v181_c","mak2z141_c"),
        w3 = c("mag1v051_c","mag1r141_c","mag1g171_c","mag1d131_c",
               "mag1d132_c","mag1z061_c","mag1v01s_c","mag1z20s_c",
               "mag1d09s_c","mag1z121_c","mag1g181_c","mag1d081_c",
               "mag1r151_c","mag1z111_c","mag1v021_c","mag1z071_c",
               "mag1d041_c","mag1g031_c","mag1z161_c","mag1v101_c",
               "mag1r19s_c"),
        w4 = c("mag1v051_sc2g2_c","mag2v071_c","mag2r031_c","mag2d061_c",
               "mag1d131_sc2g2_c","mag2r131_c","mag2v121_c","mag2q061_c",
               "mag2r111_c","mag1d09s_sc2g2_c","mag1z121_sc2g2_c",
               "mag2g12s_c","mag1d081_sc2g2_c","mag2g021_c","mag2r151_c",
               "mag1v021_sc2g2_c","mag1z071_sc2g2_c","mag2d101_c",
               "mag1g031_sc2g2_c","mag2v041_c","mag2q011_c",
               "mag1r19s_sc2g2_c","mag2g091_c","mag2q051_c"),
        w6 = c("mag5d041_sc2g4_c", "mag4q101_c", "mag4r021_c", "mag5v271_sc2g4_c",
               "mag4q011_c", "mag4r071_c", "mag4d131_c",
               "mag5q231_sc2g4_c", "mag5q301_sc2g4_c", "mag4v121_c",
               "mag5d051_sc2g4_c", #"mag4q060_c",
               "mag4d031_c", #"mag5q140_sc2g4_c",
               "mag4v111_c", "mag4r041_c", "mag4r042_c", "mag4q051_c", "mag4q091_c",
               "mag4q092_c", "mag4d14s_c", "mag5v071_sc2g4_c",
               "mag5r191_sc2g4_c", "mag4d081_c")#,
#         w9 = c()
      ),
      IC = list(
        w5 = c("icg3052x_c","icg3350x_c","icg3021x_c","icg3610x_c",
               "icg3621x_c","icg3371x_c","icg3081x_c","icg3102x_c",
               "icg3591x_c","icg3092x_c","icg3381x_c","icg3400x_c",
               "icg3661x_c","icg3410x_c","icg3420x_c","icg3432x_c",
               "icg3440x_c","icg3322x_c","icg3461x_c","icg3211x_c",
               "icg3510x_c","icg3221x_c","icg3601x_c","icg3260x_c",
               "icg3301x_c","icg3270x_c","icg3292x_c","icg3481x_c",
               "icg3541x_c","icg3550x_c")
      ),
      SC = list(
        w1 = c("sck10420_c","sck10430_c","sck16120_c","sck16130_c",
               "sck1102s_c","sck11030_c","sck1033s_c","sck10210_c",
               "sck1023s_c","sck11110_c","sck11120_c","sck16010_c",
               "sck16020_c","sck10510_c","sck10530_c","sck11610_c",
               "sck1162s_c","sck10710_c","sck10720_c","sck11310_c",
               "sck11330_c","sck10910_c","sck10920_c","sck16210_c",
               "sck16220_c"),
        w3 = c("scg10820_c","scg10840_c","scg11510_c","scg10650_c",
               "scg16510_c","scg1652s_c","scg16110_c","scg1091s_c",
               "scg10920_c","scg1011s_c","scg10120_c","scg11210_c",
               "scg11110_c","scg11130_c","scg16530_c","scg16020_c",
               "scg16030_c","scg11610_c","scg11710_c","scg10310_c",
               "scg10520_c","scg16310_c","scg16220_c","scg11440_c",
               "scg10410_c"),
        w5 = c("scg30109_c","scg33510_c","scg3181s_c","scg37110_c",
               "scg31010_c","scg36710_c","scg3131s_c","scg34010_c",
               "scg32220_c","scg33710_c","scg36210_c","scg36920_c",
               "scg32620_c","scg31510_c","scg30310_c","scg3641s_c",
               "scg30520_c","scg37410_c","scg33310_c","scg3091s_c",
               "scg33610_c","scg32910_c")#,
#         w9 = c()
      ),
      NR = list(
        w4 = c("nrg20101_c", "nrg20102_c", "nrg20103_c", "nrg20104_c", "nrg20105_c",
        "nrg20106_c", "nrg20201_c", "nrg20202_c", "nrg20203_c", "nrg20204_c",
        "nrg20205_c", "nrg20301_c", "nrg20302_c", "nrg20306_c", "nrg20401_c",
        "nrg20402_c", "nrg20403_c", "nrg20404_c", "nrg20501_c", "nrg20502_c",
        "nrg20503_c", "nrg20504_c", "nrg20505_c", "nrg20506_c", "nrg20507_c",
        "nrg20601_c", "nrg20602_c", "nrg20603_c", "nrg20604_c", "nrg20702_c",
        "nrg20704_c", "nrg20801_c", "nrg20802_c", "nrg20803_c", "nrg20804_c")
      ),
      NT = list(
        w4 = c("ntg20101_c", "ntg20102_c", "ntg20103_c", "ntg20104_c", "ntg20105_c",
        "ntg20106_c", "ntg20201_c", "ntg20202_c", "ntg20203_c", "ntg20204_c",
        "ntg20205_c", "ntg20301_c", "ntg20302_c", "ntg20306_c", "ntg20401_c",
        "ntg20402_c", "ntg20403_c", "ntg20404_c", "ntg20501_c", "ntg20502_c",
        "ntg20503_c", "ntg20504_c", "ntg20505_c", "ntg20506_c", "ntg20507_c",
        "ntg20601_c", "ntg20602_c", "ntg20603_c", "ntg20604_c", "ntg20702_c",
        "ntg20704_c", "ntg20801_c", "ntg20802_c", "ntg20803_c", "ntg20804_c")
      ),
      ORA = list(
          w6 = c("org41001_c","org41002_c","org41003_c","org41004_c",
                 "org41005_c","org41006_c","org41007_c","org41008_c",
                 "org41009_c","org41010_c","org41011_c","org41012_c",
                 "org41013_c","org41014_c","org41015_c","org41016_c",
                 "org41017_c","org41018_c","org41019_c","org41020_c",
                 "org41021_c","org41022_c","org41023_c","org41024_c",
                 "org41025_c","org41026_c","org41027_c","org41028_c",
                 "org41029_c","org41030_c","org41031_c","org41032_c",
                 "org41033_c","org41034_c","org41035_c","org41036_c",
                 "org41037_c")
      ),
      ORB = list(
          w6 = c("org42001_c","org42002_c","org42003_c","org42004_c",
                 "org42005_c","org42006_c","org42007_c","org42008_c",
                 "org42009_c","org42010_c","org42011_c","org42012_c",
                 "org42013_c","org42014_c","org42015_c","org42016_c",
                 "org42017_c","org42018_c","org42019_c","org42020_c",
                 "org42021_c","org42022_c","org42023_c","org42024_c",
                 "org42025_c","org42026_c","org42027_c","org42028_c",
                 "org42029_c","org42030_c","org42031_c","org42032_c",
                 "org42033_c","org42034_c","org42035_c","org42036_c",
                 "org42037_c","org42038_c","org42039_c","org42040_c",
                 "org42041_c","org42042_c","org42043_c","org42044_c",
                 "org42045_c","org42046_c","org42047_c","org42048_c",
                 "org42049_c","org42050_c","org42051_c","org42052_c",
                 "org42053_c","org42054_c","org42055_c","org42056_c",
                 "org42057_c","org42058_c","org42059_c","org42060_c",
                 "org42061_c","org42062_c","org42063_c","org42064_c",
                 "org42065_c","org42066_c","org42067_c","org42068_c",
                 "org42069_c","org42070_c","org42071_c","org42072_c",
                 "org42073_c","org42074_c","org42075_c","org42076_c",
                 "org42077_c","org42078_c","org42079_c","org42080_c",
                 "org42081_c","org42082_c","org42083_c","org42084_c",
                 "org42085_c","org42086_c","org42087_c","org42088_c",
                 "org42089_c","org42090_c","org42091_c","org42092_c",
                 "org42093_c","org42094_c","org42095_c","org42096_c",
                 "org42097_c","org42098_c","org42099_c","org42100_c",
                 "org42101_c","org42102_c","org42103_c","org42104_c",
                 "org42105_c","org42106_c","org42107_c","org42108_c",
                 "org42109_c","org42110_c","org42111_c","org42112_c",
                 "org42113_c","org42114_c","org42115_c","org42116_c",
                 "org42117_c","org42118_c","org42119_c","org42120_c",
                 "org42121_c","org42122_c","org42123_c","org42124_c",
                 "org42125_c","org42126_c","org42127_c","org42128_c",
                 "org42129_c","org42130_c")
      ),
      VO = list(
        w1 = c("vok10002_c", "vok10007_c", "vok10008_c", "vok10009_c", "vok10010_c",
        "vok10011_c", "vok10012_c", "vok10013_c", "vok10014_c", "vok10015_c",
        "vok10016_c", "vok10017_c", "vok10018_c", "vok10019_c", "vok10020_c",
        "vok10021_c", "vok10022_c", "vok10023_c", "vok10024_c", "vok10025_c",
        "vok10026_c", "vok10027_c", "vok10028_c",
        "vok10031_c", "vok10032_c", "vok10033_c", "vok10034_c", "vok10035_c",
        "vok10036_c", "vok10037_c", "vok10038_c", "vok10039_c", "vok10040_c",
        "vok10041_c", "vok10042_c", "vok10043_c", "vok10045_c",
        "vok10046_c", "vok10047_c", "vok10048_c", "vok10049_c", "vok10050_c",
        "vok10051_c", "vok10052_c", "vok10053_c", "vok10054_c", "vok10055_c",
        "vok10056_c", "vok10057_c", "vok10058_c", "vok10060_c",
        "vok10061_c", "vok10062_c", "vok10063_c", "vok10064_c", "vok10065_c",
        "vok10066_c"),
        w3 = c("vok10067_sc2g1_c", "vok10043_sc2g1_c", "vok10053_sc2g1_c",
        "vok10049_sc2g1_c", "vog60001_sc2g1_c", "vok10025_sc2g1_c",
        "vok10076_sc2g1_c", "vok10050_sc2g1_c", "vog10009_c", "vog60009_sc2g1_c",
        "vok10060_sc2g1_c", "vok10066_sc2g1_c", "vok10063_sc2g1_c",
        "vok10040_sc2g1_c", "vok10074_sc2g1_c", "vok10033_sc2g1_c",
        "vog90015_sc2g1_c", "vok10051_sc2g1_c", "vok10061_sc2g1_c",
        "vog60051_sc2g1_c", "vog90007_sc2g1_c", "vog60015_sc2g1_c",
        "vok10057_sc2g1_c", "vok10072_sc2g1_c", "vog90016_sc2g1_c",
        "vog90032_sc2g1_c", "vog60010_sc2g1_c", "vok10041_sc2g1_c",
        "vok10052_sc2g1_c", "vog60032_sc2g1_c", "vok10031_sc2g1_c",
        "vok10045_sc2g1_c", "vok10039_sc2g1_c", "vog10034_c", "vok10034_sc2g1_c",
        "vok10058_sc2g1_c", "vog90031_sc2g1_c", "vog60049_sc2g1_c",
        "vok10065_sc2g1_c", "vog10040_c", "vok10071_sc2g1_c", "vok10069_sc2g1_c",
        "vog60025_sc2g1_c", "vog10044_c", "vok10028_sc2g1_c", "vog10046_c",
        "vog60027_sc2g1_c", "vog60047_sc2g1_c", "vok10022_sc2g1_c",
        "vok10038_sc2g1_c", "vog90028_sc2g1_c", "vok10047_sc2g1_c",
        "vok10046_sc2g1_c", "vog60019_sc2g1_c", "vok10048_sc2g1_c", "vog10056_c",
        "vog90020_sc2g1_c", "vok10037_sc2g1_c", "vog60030_sc2g1_c", "vog10060_c",
        "vok10077_sc2g1_c", "vog10062_c", "vog10063_c", "vok10042_sc2g1_c",
        "vok10064_sc2g1_c", "vok10026_sc2g1_c"),
        w5 = c("vog10034_sc2g3_c",
               "vok10043_sc2g3_c", "vog90031_sc2g3_c",
               "vog10060_sc2g3_c", "vog10009_sc2g3_c",
               "vog60041_sc2g3_c", "vog60025_sc2g3_c",
               "vok10075_sc2g3_c", "vok10033_sc2g3_c", "vog90015_sc2g3_c",
               "vok10061_sc2g3_c", "vok10065_sc2g3_c", #"vog60015_sc2g3_c", # not in SUF, but described in TR
               "vok10072_sc2g3_c",
               "vog60030_sc2g3_c", "vog60029_sc2g3_c", "vog90003_sc2g3_c",
               "vog10062_sc2g3_c",
               "vok10026_sc2g3_c", "vog60037_sc2g3_c", "vok10058_sc2g3_c",
               "vog60049_sc2g3_c",
               "vok10076_sc2g3_c", "vok10040_sc2g3_c", "vog10040_sc2g3_c",
               "vok10071_sc2g3_c", "vok10060_sc2g3_c",
               "vog10044_sc2g3_c", "vog60045_sc2g3_c",
               "vog90035_sc2g3_c", "vok10074_sc2g3_c",
               "vog60027_sc2g3_c",
               "vok10051_sc2g3_c", "vog60047_sc2g3_c", "vok10073_sc2g3_c",
               "vog90037_sc2g3_c",
               "vok10038_sc2g3_c", "vok10047_sc2g3_c",
               "vok10057_sc2g3_c", "vok10046_sc2g3_c", "vog60019_sc2g3_c",
               "vok10048_sc2g3_c", "vog90016_sc2g3_c",
               "vog90032_sc2g3_c", "vog60010_sc2g3_c", "vog60032_sc2g3_c",
               "vog60054_sc2g3_c", "vok10064_sc2g3_c", "vog90028_sc2g3_c")
      ),
      GR = list(
        w1 = c("grk1a101_c","grk1a202_c","grk1a303_c","grk1a404_c",
"grk1b105_c","grk1b206_c","grk1b307_c","grk1b408_c",
"grk1c109_c","grk1c210_c","grk1c311_c","grk1c412_c",
"grk1d113_c","grk1d214_c","grk1e115_c","grk1e216_c",
"grk1f117_c","grk1f218_c","grk1g119_c","grk1g220_c",
"grk1h121_c","grk1h222_c","grk1i123_c","grk1i224_c",
"grk1j125_c","grk1j226_c","grk1k127_c","grk1k228_c",
"grk1l129_c","grk1l230_c","grk1m131_c","grk1m232_c",
"grk1n133_c","grk1n234_c","grk1o135_c","grk1o236_c",
"grk1p137_c","grk1p238_c","grk1q139_c","grk1q240_c",
"grk1r141_c","grk1r242_c","grk1s143_c","grk1s244_c",
"grk1t145_c","grk1t246_c","grk1u147_c","grk1u248_c"),
        w3 = c("grg1d101_c","grg1d202_c","grg1j103_c","grg1j204_c",
"grg1k105_c","grg1k206_c","grg1l107_c","grg1l208_c",
"grg1m109_c","grg1m210_c","grg1n111_c","grg1n212_c",
"grg1o113_c","grg1o214_c","grg1o315_c","grg1o416_c",
"grg1p117_c","grg1p218_c","grg1p319_c","grg1p420_c",
"grg1q121_c","grg1q222_c","grg1q323_c","grg1q424_c",
"grg1r125_c","grg1r226_c","grg1r327_c","grg1r428_c",
"grg1s129_c","grg1s230_c","grg1s331_c","grg1s432_c",
"grg1t133_c","grg1t234_c","grg1t335_c","grg1t436_c",
"grg1u137_c","grg1u238_c","grg1u339_c","grg1u440_c")
      )
    ),
    SC3 = list(
      RE = list(
        w1 = c("reg50110_c","reg5012s_c","reg50130_c","reg50140_c",
               "reg50150_c","reg5016s_c","reg50170_c","reg50210_c",
               "reg50220_c","reg50230_c","reg50240_c","reg50250_c",
               "reg5026s_c","reg50310_c","reg50320_c","reg50330_c",
               "reg50340_c","reg50350_c","reg50360_c","reg50370_c",
               "reg50410_c","reg5042s_c","reg50430_c","reg50440_c",
               "reg50460_c","reg50510_c","reg5052s_c","reg50530_c",
               "reg50540_c","reg5055s_c","reg50560_c","reg50570_c"),
        w3 = c("reg70110_c","reg70120_c","reg7013s_c","reg70140_c",
               "reg7015s_c","reg7016s_c","reg70610_c","reg70620_c",
               "reg7063s_c",
               "reg70640_c","reg70650_c","reg7066s_c","reg70210_c",
               "reg70220_c",
               "reg7023s_c","reg7024s_c","reg70250_c","reg7026s_c",
               "reg70310_c","reg70320_c","reg7033s_c","reg70340_c",
               "reg70350_c","reg70360_c","reg70410_c","reg70420_c",
               "reg70430_c","reg70440_c","reg7045s_c","reg70460_c",
               "reg7051s_c","reg70520_c","reg7053s_c","reg7055s_c",
               "reg70560_c","reg7071s_c",
               "reg70720_c","reg70730_c","reg70740_c","reg7075s_c"),
        w6 = c("reg90610_c","reg90620_c","reg9063s_c","reg90640_c",
               #"reg90650_c",
               "reg90660_c","reg90670_c","reg90680_c",
               "reg90810_c","reg90820_c","reg9083s_c","reg90840_c",
               "reg90850_c","reg90860_c","reg90870_c","reg90210_sc3g9_c",
               "reg90220_sc3g9_c","reg90230_sc3g9_c",#"reg90240_sc3g9_c",
               "reg90250_sc3g9_c","reg90710_c","reg90720_c","reg90730_c",
               "reg9074s_c","reg90750_c","reg9091s_c","reg90920_c",
               "reg90930_c","reg90940_c","reg90950_c","reg90960_c",
               "reg9097s_c","reg90410_sc3g9_c","reg90420_sc3g9_c",
               "reg90430_sc3g9_c","reg90440_sc3g9_c","reg90450_sc3g9_c",
               "reg90460_sc3g9_c","reg9047s_sc3g9_c","reg90510_sc3g9_c",
               "reg90520_sc3g9_c","reg90530_sc3g9_c","reg90540_sc3g9_c",
               "reg90550_sc3g9_c","reg90560_sc3g9_c","reg90570_sc3g9_c"),
        w9 = c("reg1205010_sc3g12_c", "reg1205020_sc3g12_c",
               "reg1205030_sc3g12_c", "reg120504s_sc3g12_c",
               "reg1205050_sc3g12_c", "reg1205060_sc3g12_c",
               "reg1205070_sc3g12_c", "reg122301s_sc3g12_c",
               "reg1223020_sc3g12_c", "reg1223040_sc3g12_c",
               "reg122305s_sc3g12_c", "reg1223060_sc3g12_c",
               "reg1226020_sc3g12_c", "reg1226030_sc3g12_c",
               "reg1226040_sc3g12_c", "reg1226050_sc3g12_c",
               "reg1226060_sc3g12_c", "reg1226080_sc3g12_c",
               "reg121602s_sc3g12_c", "reg121603s_sc3g12_c",
               "reg1216040_sc3g12_c", "reg121605s_sc3g12_c",
               "reg1216060_sc3g12_c", "reg1220010_sc3g12_c",
               "reg122002s_sc3g12_c", "reg1220030_sc3g12_c",
               "reg1220040_sc3g12_c", "reg122005s_sc3g12_c",
               "reg1220060_sc3g12_c", "reg1229010_sc3g12_c",
               "reg1229020_sc3g12_c", "reg1229030_sc3g12_c",
               "reg1229060_sc3g12_c", "reg122907s_sc3g12_c",
               "reg1229080_sc3g12_c", "reg1229100_sc3g12_c",
               "reg122501s_sc3g12_c", "reg1225030_sc3g12_c",
               "reg1225060_sc3g12_c", "reg1225050_sc3g12_c",
               "reg122504s_sc3g12_c")
      ),
      MA = list(
        w1 = c("mag5d041_c","mag5q291_c","mag5q292_c","mag5v271_c",
               "mag5r171_c","mag5q231_c","mag5q301_c","mag5q221_c",
               "mag5d051_c","mag5d052_c","mag5q14s_c","mag5q121_c",
               "mag5r101_c","mag5r201_c","mag5q131_c","mag5d02s_c",
               "mag5d023_c","mag5v024_c","mag5r251_c","mag5v01s_c",
               "mag5v321_c","mag5v071_c","mag5r191_c","mag5v091_c"),
        w3 = c("mag9q071_sc3g7_c","mag7v071_c","mag7r081_c","mag7q051_c",
               "mag5q301_sc3g7_c","mag9d151_sc3g7_c","mag5d051_sc3g7_c",
               "mag5d052_sc3g7_c","mag9v011_sc3g7_c","mag9v012_sc3g7_c",
               "mag7q041_c","mag7d042_c","mag7r091_c","mag9q181_sc3g7_c",
               "mag7d011_c","mag7v012_c","mag7v031_c","mag5r251_sc3g7_c",
               "mag7d061_c","mag5v321_sc3g7_c","mag9v091_sc3g7_c",
               "mag5r191_sc3g7_c","mag7r02s_c"),
        w5 = c("mag9d151_sc3g9_c","mag9d201_sc3g9_c","mag9d05s_c","mag9d061_c",
               "mag9d111_c","mag9d09s_c","mag9d131_c","mag9q021_sc3g9_c",
               "mag9q071_sc3g9_c","mag9q081_sc3g9_c","mag9q101_sc3g9_c",
               "mag9q181_sc3g9_c","mag9q211_sc3g9_c","mag9q121_c","mag9q151_c",
               "mag9q191_c","mag9q021_c","mag9q041_c","mag9q011_c","mag9q031_c",
               "mag9r051_sc3g9_c","mag9r061_sc3g9_c","mag9r111_sc3g9_c",
               "mag9r191_sc3g9_c","mag9r261_sc3g9_c","mag9r10s_c","mag9r14s_c",
               "mag9v011_sc3g9_c","mag9v012_sc3g9_c","mag9v091_sc3g9_c",
               "mag9v121_sc3g9_c","mag9v131_sc3g9_c","mag9v13s_sc3g9_c",
               "mag9v081_c"),
        w9 = c("maa3q071_sc3g12_c", "mag12v101_sc3g12_c", "mag12q121_sc3g12_c",
               "mag12v122_sc3g12_c", "mag12r011_sc3g12_c", "mag12v061_sc3g12_c",
               "mag12r091_sc3g12_c", "mag9r051_sc3g12_c", "mag12q081_sc3g12_c",
               "mag12d021_sc3g12_c", "mag12q051_sc3g12_c", "mag9d201_sc3g12_c",
               "mag9v121_sc3g12_c", "mas1q021s_sc3g12_c", "mas1d081_sc3g12_c",
               "maa3d112_sc3g12_c", "mag9r061_sc3g12_c", "maa3r011_sc3g12_c",
               "mag12d071_sc3g12_c", "mag12r041_sc3g12_c", "mag12v131_sc3g12_c",
               "mag12d031_sc3g12_c", "maa3d131_sc3g12_c", "maa3d132_sc3g12_c",
               "mag9v011_sc3g12_c", "maa3r121_sc3g12_c", "mag12q111_sc3g12_c",
               "maa3q101_sc3g12_c", "mag9q101_sc3g12_c", "mag12v132_sc3g12_c")
      ),
      IC = list(
        w2 = c("icg6001x_c","icg6003x_c","icg6005x_c","icg6006x_c",
               "icg6009x_c","icg6011x_c","icg6012x_c","icg6013x_c",
               "icg6014x_c","icg6015x_c","icg6020x_c","icg6016x_c",
               "icg6018x_c","icg6021x_c","icg6024x_c","icg6025x_c",
               "icg6031x_c","icg6032x_c","icg6033x_c","icg6034x_c",
               "icg6036x_c","icg6039x_c","icg6042x_c","icg6047x_c",
               "icg6048x_c","icg6049x_c","icg6046x_c","icg6053x_c",
               "icg6054x_c","icg6059x_c"),
        w5 = c("icg5005x_sc3g9_c","icg5034x_sc3g9_c","icg5009x_sc3g9_c",
               "icg5051x_c","icg5018x_sc3g9_c","icg9106x_sc3g9_c",
               "icg5015x_sc3g9_c","icg5046x_sc3g9_c","icg5033x_sc3g9_c",
               "icg9110x_sc3g9_c","icg5045x_c","icg5054x_sc3g9_c",
               "icg5021x_sc3g9_c","icg9114x_sc3g9_c","icg5059x_sc3g9_c",
               "icg9116x_sc3g9_c","icg5035x_c","icg9118x_sc3g9_c",
               "icg9119x_sc3g9_c","icg5003x_sc3g9_c","icg5029x_c",
               "icg9122x_sc3g9_c","icg9123x_sc3g9_c","icg12041x_sc3g9_c",
               "icg12042x_c","icg12060s_sc3g9_c","icg12036x_c",
               "icg5039x_sc3g9_c","icg12018s_sc3g9_c","icg5053x_sc3g9_c",
               "icg9131x_sc3g9_c","icg9132x_sc3g9_c","icg5049x_sc3g9_c",
               "icg12022x_c","icg9138x_sc3g9_c","icg9140s_sc3g9_c",
               "icg9102s_sc3g9_c","icg5047x_sc3g9_c","icg12034x_sc3g9_c",
               "icg9113x_sc3g9_c","icg12040x_sc3g9_c","icg12043x_c",
               "icg9117s_sc3g9_c","ica5021s_sc3g9_c","icg9128x_sc3g9_c" ,
               "icg9133s_sc3g9_c","icg9136s_sc3g9_c","icg12027x_c",
               "icg9101x_sc3g9_c","icg9103x_sc3g9_c","icg9107s_sc3g9_c",
               "icg12138s_sc3g9_c","icg12016s_sc3g9_c","icg9111x_sc3g9_c",
               "icg12047s_sc3g9_c","icg12046s_sc3g9_c","ica5052s_sc3g9_c",
               "icg9125s_sc3g9_c","icg9129x_sc3g9_c","icg12050s_sc3g9_c"),
        w9 = c("icg12018s_sc3g12_c", "ica4003x_sc3g12_c", "icg12107s_sc3g12_c",
               "icg12004s_sc3g12_c", "icg12010x_sc3g12_c", "icg12011x_sc3g12_c",
               "ica4008x_sc3g12_c", "icg12060s_sc3g12_c", "icg12013s_sc3g12_c",
               "ica4018s_sc3g12_c", "icg12016s_sc3g12_c", "ica4019x_sc3g12_c",
               "icg12121x_sc3g12_c", "icg12028s_sc3g12_c", "ica4023x_sc3g12_c",
               "ica4027x_sc3g12_c", "icg12033x_sc3g12_c", "icg12034x_sc3g12_c",
               "icg12035x_sc3g12_c", "icg12040x_sc3g12_c", "icg12037s_sc3g12_c",
               "icg12138s_sc3g12_c", "icg12047s_sc3g12_c", "icg12041x_sc3g12_c",
               "icg12046s_sc3g12_c", "ica4021s_sc3g12_c", "ica4052s_sc3g12_c",
               "icg12048s_sc3g12_c", "icg12050s_sc3g12_c", "icg12054s_sc3g12_c",
               "icg12109s_sc3g12_c", "icg12119s_sc3g12_c")
      ),
      SC = list(
        w2 = c("scg6103s_c","scg61050_c","scg60120_c",
               "scg60410_c","scg60430_c","scg66310_c","scg66320_c",
               "scg66340_c","scg61410_c","scg6142s_c","scg61430_c",
               "scg6144s_c","scg60510_c","scg60530_c","scg6661s_c",
               "scg66620_c","scg66630_c","scg6664s_c","scg6111s_c",
               "scg6113s_c","scg66040_c","scg61310_c",
               "scg61330_c","scg6061s_c","scg60620_c"),
        w5 = c("scg90110_sc3g9_c","scg9012s_sc3g9_c","scg90510_sc3g9_c",
               "scg9052s_sc3g9_c","scg90920_sc3g9_c","scg90930_sc3g9_c",
               "scg9611s_sc3g9_c","scg96120_sc3g9_c","scg96410_sc3g9_c",
               "scg96420_sc3g9_c","scg9061s_sc3g9_c","scg90630_sc3g9_c",
               "scg90810_sc3g9_c","scg9083s_sc3g9_c","scg91030_sc3g9_c",
               "scg91040_sc3g9_c","scg91050_sc3g9_c","scg9042s_sc3g9_c",
               "scg9043s_sc3g9_c","scg9651s_sc3g9_c","scg96530_sc3g9_c",
               "scg90320_sc3g9_c","scg90330_sc3g9_c","scg9621s_sc3g9_c",
               "scg96220_sc3g9_c","scg91110_sc3g9_c","scg91120_sc3g9_c",
               "scg91130_sc3g9_c","scg97410_sc3g9_c","scg9771s_c",
               #"scg116320_sc3g9_c",
               "scg98910_c","scg9751s_c","scg9752s_c",
               "scg98010_c","scg97910_c","scg98210_c","scg98310_c"#,
               #"scg116210_sc3g9_c"
               ),
        w8 = c("scg116420_sc3g11_c", "scg110620_sc3g11_c", "scg110630_sc3g11_c",
               "scg11012s_sc3g11_c", "scg11083s_sc3g11_c", "scg110720_sc3g11_c",
               "scg11032s_sc3g11_c", "scg110330_sc3g11_c", "scg116510_sc3g11_c",
               "scg11652s_sc3g11_c", "scs56320_sc3g11_c", "scg110510_sc3g11_c",
               "scg110520_sc3g11_c", "scg110540_sc3g11_c", "scg11123s_sc3g11_c",
               "scg11102s_sc3g11_c", "scg11021s_sc3g11_c", "scg11022s_sc3g11_c",
               "scg11112s_sc3g11_c", "scg116210_sc3g11_c", "scg11622s_sc3g11_c",
               "scg116320_sc3g11_c", "scg110930_sc3g11_c", "scs5131s_sc3g11_c",
               "scs5132s_sc3g11_c")
      ),
      ORA = list(
        w1 = c("org51001_c","org51002_c","org51003_c","org51004_c",
               "org51005_c","org51006_c","org51007_c","org51008_c",
               "org51009_c","org51010_c","org51011_c","org51012_c",
               "org51013_c","org51014_c","org51015_c","org51016_c",
               "org51017_c","org51018_c","org51019_c","org51020_c",
               "org51021_c","org51022_c","org51023_c","org51024_c",
               "org51025_c","org51026_c","org51027_c","org51028_c",
               "org51029_c","org51030_c","org51031_c","org51032_c",
               "org51033_c","org51034_c","org51035_c","org51036_c",
               "org51037_c","org51038_c","org51039_c","org51040_c",
               "org51041_c","org51042_c","org51043_c","org51044_c",
               "org51045_c","org51046_c","org51047_c","org51048_c",
               "org51049_c","org51050_c"),
        w3 = c("org51023_sc3g7_c","org51030_sc3g7_c","org51040_sc3g7_c",
               "org51003_sc3g7_c","org51015_sc3g7_c","org51011_sc3g7_c",
               "org51004_sc3g7_c","org51042_sc3g7_c","org51038_sc3g7_c",
               "org51037_sc3g7_c","org51041_sc3g7_c","org51033_sc3g7_c",
               "org51009_sc3g7_c","org51031_sc3g7_c","org51010_sc3g7_c",
               "org51032_sc3g7_c","org51035_sc3g7_c","org71001_c",
               "org71002_c","org71003_c","org71004_c","org71005_c",
               "org71006_c","org71007_c","org71008_c","org71009_c",
               "org71010_c","org71011_c","org71012_c","org71013_c",
               "org71014_c","org71015_c","org71016_c","org71017_c",
               "org71018_c","org71019_c","org71020_c","org71021_c",
               "org71022_c","org71023_c","org71024_c","org71025_c",
               "org71026_c","org71027_c","org71028_c","org71029_c",
               "org71030_c","org71031_c","org71032_c","org71033_c",
               "org71034_c","org71035_c","org71036_c","org71037_c",
               "org71038_c","org71039_c","org71040_c","org71041_c",
               "org71042_c","org71043_c","org71044_c","org71045_c",
               "org71046_c","org71047_c","org71048_c","org71049_c",
               "org71050_c","org71051_c","org71052_c","org71053_c",
               "org71054_c","org71055_c","org71056_c","org71057_c",
               "org71058_c","org71059_c","org71060_c","org71061_c"),
        w5 = c("org51030_sc3g9_c","org51040_sc3g9_c","org51011_sc3g9_c",
               "org51004_sc3g9_c","org51042_sc3g9_c","org51038_sc3g9_c",
               "org51037_sc3g9_c","org51009_sc3g9_c","org51031_sc3g9_c",
               "org51032_sc3g9_c","org71001_sc3g9_c","org71002_sc3g9_c",
               "org71003_sc3g9_c","org71004_sc3g9_c","org71005_sc3g9_c",
               "org71006_sc3g9_c","org71007_sc3g9_c","org71008_sc3g9_c",
               "org71009_sc3g9_c","org71010_sc3g9_c","org71011_sc3g9_c",
               "org71012_sc3g9_c","org71013_sc3g9_c","org71014_sc3g9_c",
               "org71015_sc3g9_c","org71016_sc3g9_c","org71017_sc3g9_c",
               "org71018_sc3g9_c","org71019_sc3g9_c","org71020_sc3g9_c",
               "org71021_sc3g9_c","org71022_sc3g9_c","org71023_sc3g9_c",
               "org71024_sc3g9_c","org71025_sc3g9_c","org71026_sc3g9_c",
               "org71027_sc3g9_c","org71028_sc3g9_c","org71029_sc3g9_c",
               "org71030_sc3g9_c","org71031_sc3g9_c","org71032_sc3g9_c",
               "org91001_c","org91002_c","org91003_c","org91004_c",
               "org91005_c","org91006_c","org91007_c","org91008_c",
               "org91009_c","org91010_c","org91011_c","org91012_c",
               "org91013_c","org91014_c","org91015_c","org91016_c",
               "org91017_c","org91018_c","org91019_c","org91020_c",
               "org91021_c","org91022_c","org91023_c","org91024_c",
               "org91025_c","org91026_c","org91027_c","org91028_c",
               "org91029_c","org91030_c","org91031_c","org91032_c",
               "org91033_c","org91034_c","org91035_c")
      ),
      ORB = list(
        w1 = c("org52001_c","org52002_c","org52003_c","org52004_c",
               "org52005_c","org52006_c","org52007_c","org52008_c",
               "org52009_c","org52010_c","org52011_c","org52012_c",
               "org52013_c","org52014_c","org52015_c","org52016_c",
               "org52017_c","org52018_c","org52019_c","org52020_c",
               "org52021_c","org52022_c","org52023_c","org52024_c",
               "org52025_c","org52026_c","org52027_c","org52028_c",
               "org52029_c","org52030_c","org52031_c","org52032_c",
               "org52033_c","org52034_c","org52035_c","org52036_c",
               "org52037_c","org52038_c","org52039_c","org52040_c",
               "org52041_c","org52042_c","org52043_c","org52044_c",
               "org52045_c","org52046_c","org52047_c","org52048_c",
               "org52049_c","org52050_c","org52051_c","org52052_c",
               "org52053_c","org52054_c","org52055_c","org52056_c",
               "org52057_c","org52058_c","org52059_c","org52060_c",
               "org52061_c","org52062_c","org52063_c","org52064_c",
               "org52065_c","org52066_c","org52067_c","org52068_c",
               "org52069_c","org52070_c","org52071_c","org52072_c",
               "org52073_c","org52074_c","org52075_c","org52076_c",
               "org52077_c","org52078_c","org52079_c","org52080_c",
               "org52081_c","org52082_c","org52083_c","org52084_c",
               "org52085_c","org52086_c","org52087_c","org52088_c",
               "org52089_c","org52090_c","org52091_c","org52092_c",
               "org52093_c","org52094_c","org52095_c","org52096_c",
               "org52097_c","org52098_c","org52099_c","org52100_c",
               "org52101_c","org52102_c","org52103_c","org52104_c",
               "org52105_c","org52106_c","org52107_c","org52108_c",
               "org52109_c","org52110_c","org52111_c","org52112_c",
               "org52113_c","org52114_c","org52115_c","org52116_c",
               "org52117_c","org52118_c","org52119_c","org52120_c",
               "org52121_c","org52122_c","org52123_c","org52124_c",
               "org52125_c","org52126_c","org52127_c","org52128_c",
               "org52129_c","org52130_c","org52131_c","org52132_c",
               "org52133_c","org52134_c","org52135_c","org52136_c",
               "org52137_c","org52138_c","org52139_c","org52140_c",
               "org52141_c","org52142_c","org52143_c","org52144_c",
               "org52145_c","org52146_c","org52147_c","org52148_c",
               "org52149_c","org52150_c","org52151_c","org52152_c",
               "org52153_c","org52154_c","org52155_c","org52156_c",
               "org52157_c","org52158_c","org52159_c","org52160_c",
               "org52161_c","org52162_c","org52163_c","org52164_c",
               "org52165_c","org52166_c","org52167_c","org52168_c",
               "org52169_c","org52170_c","org52171_c","org52172_c",
               "org52173_c","org52174_c","org52175_c","org52176_c",
               "org52177_c","org52178_c","org52179_c","org52180_c",
               "org52181_c"),
        w3 = c("org52060_sc3g7_c","org52163_sc3g7_c","org52016_sc3g7_c",
               "org52133_sc3g7_c","org52107_sc3g7_c","org52078_sc3g7_c",
               "org52175_sc3g7_c","org52168_sc3g7_c","org52139_sc3g7_c",
               "org52169_sc3g7_c","org52138_sc3g7_c","org52068_sc3g7_c",
               "org52109_sc3g7_c","org52073_sc3g7_c","org52032_sc3g7_c",
               "org52096_sc3g7_c","org52074_sc3g7_c","org52082_sc3g7_c",
               "org52153_sc3g7_c","org52116_sc3g7_c","org52086_sc3g7_c",
               "org52129_sc3g7_c","org52128_sc3g7_c","org52048_sc3g7_c",
               "org52125_sc3g7_c","org52007_sc3g7_c","org52126_sc3g7_c",
               "org52043_sc3g7_c","org52002_sc3g7_c","org52119_sc3g7_c",
               "org52178_sc3g7_c","org52148_sc3g7_c","org52033_sc3g7_c",
               "org52147_sc3g7_c","org52098_sc3g7_c","org52112_sc3g7_c",
               "org52059_sc3g7_c","org52029_sc3g7_c","org52072_sc3g7_c",
               "org52144_sc3g7_c","org52030_sc3g7_c","org52143_sc3g7_c",
               "org52111_sc3g7_c","org52176_sc3g7_c","org52071_sc3g7_c",
               "org52075_sc3g7_c","org52146_sc3g7_c","org52097_sc3g7_c",
               "org52093_sc3g7_c","org52069_sc3g7_c","org52070_sc3g7_c",
               "org52142_sc3g7_c","org52094_sc3g7_c","org52154_sc3g7_c",
               "org52083_sc3g7_c","org52006_sc3g7_c","org52170_sc3g7_c",
               "org52140_sc3g7_c","org52092_sc3g7_c","org52084_sc3g7_c",
               "org52123_sc3g7_c","org52025_sc3g7_c","org52171_sc3g7_c",
               "org52141_sc3g7_c","org52173_sc3g7_c","org52027_sc3g7_c",
               "org52028_sc3g7_c","org52026_sc3g7_c","org52110_sc3g7_c",
               "org72001_c","org72002_c","org72003_c","org72004_c",
               "org72005_c","org72006_c","org72007_c","org72008_c",
               "org72009_c","org72010_c","org72011_c","org72012_c",
               "org72013_c","org72014_c","org72015_c","org72016_c",
               "org72017_c","org72018_c","org72019_c","org72020_c",
               "org72021_c","org72022_c","org72023_c","org72024_c",
               "org72025_c","org72026_c","org72027_c","org72028_c",
               "org72029_c","org72030_c","org72031_c","org72032_c",
               "org72033_c","org72034_c","org72035_c","org72036_c",
               "org72037_c","org72038_c","org72039_c","org72040_c",
               "org72041_c","org72042_c","org72043_c","org72044_c",
               "org72045_c","org72046_c","org72047_c","org72048_c",
               "org72049_c","org72050_c","org72051_c","org72052_c",
               "org72053_c","org72054_c","org72055_c","org72056_c",
               "org72057_c","org72058_c","org72059_c","org72060_c",
               "org72061_c","org72062_c","org72063_c","org72064_c",
               "org72065_c","org72066_c","org72067_c","org72068_c",
               "org72069_c","org72070_c","org72071_c","org72072_c",
               "org72073_c","org72074_c","org72075_c","org72076_c",
               "org72077_c","org72078_c","org72079_c","org72080_c",
               "org72081_c","org72082_c","org72083_c","org72084_c",
               "org72085_c","org72086_c","org72087_c","org72088_c",
               "org72089_c","org72090_c","org72091_c","org72092_c",
               "org72093_c","org72094_c","org72095_c","org72096_c",
               "org72097_c","org72098_c","org72099_c","org72100_c",
               "org72101_c","org72102_c","org72103_c","org72104_c",
               "org72105_c","org72106_c","org72107_c","org72108_c",
               "org72109_c","org72110_c","org72111_c","org72112_c",
               "org72113_c","org72114_c","org72115_c","org72116_c",
               "org72117_c","org72118_c","org72119_c","org72120_c",
               "org72121_c","org72122_c","org72123_c","org72124_c",
               "org72125_c","org72126_c","org72127_c","org72128_c",
               "org72129_c","org72130_c","org72131_c","org72132_c",
               "org72133_c","org72134_c","org72135_c","org72136_c",
               "org72137_c","org72138_c","org72139_c","org72140_c",
               "org72141_c","org72142_c","org72143_c","org72144_c",
               "org72145_c","org72146_c","org72147_c","org72148_c",
               "org72149_c","org72150_c","org72151_c","org72152_c",
               "org72153_c","org72154_c","org72155_c","org72156_c",
               "org72157_c","org72158_c","org72159_c","org72160_c",
               "org72161_c","org72162_c","org72163_c","org72164_c",
               "org72165_c","org72166_c","org72167_c","org72168_c",
               "org72169_c","org72170_c","org72171_c","org72172_c",
               "org72173_c","org72174_c","org72175_c","org72176_c",
               "org72177_c","org72178_c","org72179_c","org72180_c",
               "org72181_c","org72182_c","org72183_c","org72184_c",
               "org72185_c","org72186_c","org72187_c","org72188_c",
               "org72189_c","org72190_c","org72191_c","org72192_c",
               "org72193_c","org72194_c","org72195_c","org72196_c",
               "org72197_c","org72198_c","org72199_c","org72200_c",
               "org72201_c","org72202_c","org72203_c","org72204_c",
               "org72205_c","org72206_c","org72207_c","org72208_c",
               "org72209_c","org72210_c","org72211_c","org72212_c",
               "org72213_c","org72214_c","org72215_c","org72216_c",
               "org72217_c","org72218_c","org72219_c","org72220_c",
               "org72221_c","org72222_c","org72223_c","org72224_c",
               "org72225_c","org72226_c","org72227_c","org72228_c",
               "org72229_c","org72230_c","org72231_c","org72232_c"),
        w5 = c("org52078_sc3g9_c","org52138_sc3g9_c","org52068_sc3g9_c",
               "org52048_sc3g9_c","org52126_sc3g9_c","org52043_sc3g9_c",
               "org52178_sc3g9_c","org52148_sc3g9_c","org52098_sc3g9_c",
               "org52112_sc3g9_c","org52029_sc3g9_c","org52144_sc3g9_c",
               "org52111_sc3g9_c","org52071_sc3g9_c","org52075_sc3g9_c",
               "org52097_sc3g9_c","org52070_sc3g9_c","org52142_sc3g9_c",
               "org52094_sc3g9_c","org52083_sc3g9_c","org52170_sc3g9_c",
               "org52140_sc3g9_c","org52025_sc3g9_c","org52171_sc3g9_c",
               "org72001_sc3g9_c","org72002_sc3g9_c","org72003_sc3g9_c",
               "org72004_sc3g9_c","org72005_sc3g9_c","org72006_sc3g9_c",
               "org72007_sc3g9_c","org72008_sc3g9_c","org72009_sc3g9_c",
               "org72010_sc3g9_c","org72011_sc3g9_c","org72012_sc3g9_c",
               "org72013_sc3g9_c","org72014_sc3g9_c","org72015_sc3g9_c",
               "org72016_sc3g9_c","org72017_sc3g9_c","org72018_sc3g9_c",
               "org72019_sc3g9_c","org72020_sc3g9_c","org72021_sc3g9_c",
               "org72022_sc3g9_c","org72023_sc3g9_c","org72024_sc3g9_c",
               "org72025_sc3g9_c","org72026_sc3g9_c","org72027_sc3g9_c",
               "org72028_sc3g9_c","org72029_sc3g9_c","org72030_sc3g9_c",
               "org72031_sc3g9_c","org72032_sc3g9_c","org72033_sc3g9_c",
               "org72034_sc3g9_c","org72035_sc3g9_c","org72036_sc3g9_c",
               "org72037_sc3g9_c","org72038_sc3g9_c","org72039_sc3g9_c",
               "org72040_sc3g9_c","org72041_sc3g9_c","org72042_sc3g9_c",
               "org72043_sc3g9_c","org72044_sc3g9_c","org72045_sc3g9_c",
               "org72046_sc3g9_c","org72047_sc3g9_c","org72048_sc3g9_c",
               "org72049_sc3g9_c","org72050_sc3g9_c","org72051_sc3g9_c",
               "org72052_sc3g9_c","org72053_sc3g9_c","org72054_sc3g9_c",
               "org72055_sc3g9_c","org72056_sc3g9_c","org72057_sc3g9_c",
               "org72058_sc3g9_c","org72059_sc3g9_c","org72060_sc3g9_c",
               "org72061_sc3g9_c","org72062_sc3g9_c","org72063_sc3g9_c",
               "org72064_sc3g9_c","org72065_sc3g9_c","org72066_sc3g9_c",
               "org72067_sc3g9_c","org72068_sc3g9_c","org72069_sc3g9_c",
               "org72070_sc3g9_c","org72071_sc3g9_c","org72072_sc3g9_c",
               "org72073_sc3g9_c","org72074_sc3g9_c","org72075_sc3g9_c",
               "org72076_sc3g9_c","org72077_sc3g9_c","org72078_sc3g9_c",
               "org72079_sc3g9_c","org72080_sc3g9_c","org72081_sc3g9_c",
               "org72082_sc3g9_c","org72083_sc3g9_c","org72084_sc3g9_c",
               "org72085_sc3g9_c","org72086_sc3g9_c","org72087_sc3g9_c",
               "org72088_sc3g9_c","org72089_sc3g9_c","org72090_sc3g9_c",
               "org72091_sc3g9_c","org72092_sc3g9_c","org92001_c",
               "org92002_c","org92003_c","org92004_c","org92005_c",
               "org92006_c","org92007_c","org92008_c","org92009_c",
               "org92010_c","org92011_c","org92012_c","org92013_c",
               "org92014_c","org92015_c","org92016_c","org92017_c",
               "org92018_c","org92019_c","org92020_c","org92021_c",
               "org92022_c","org92023_c","org92024_c","org92025_c",
               "org92026_c","org92027_c","org92028_c","org92029_c",
               "org92030_c","org92031_c","org92032_c","org92033_c",
               "org92034_c","org92035_c","org92036_c","org92037_c",
               "org92038_c","org92039_c","org92040_c","org92041_c",
               "org92042_c","org92043_c","org92044_c","org92045_c",
               "org92046_c","org92047_c","org92048_c","org92049_c",
               "org92050_c","org92051_c","org92052_c","org92053_c",
               "org92054_c","org92055_c","org92056_c","org92057_c",
               "org92058_c","org92059_c","org92060_c","org92061_c",
               "org92062_c","org92063_c","org92064_c","org92065_c",
               "org92066_c","org92067_c","org92068_c","org92069_c",
               "org92070_c","org92071_c","org92072_c","org92073_c",
               "org92074_c","org92075_c","org92076_c","org92077_c",
               "org92078_c","org92079_c","org92080_c","org92081_c",
               "org92082_c","org92083_c","org92084_c","org92085_c",
               "org92086_c","org92087_c","org92088_c","org92089_c",
               "org92090_c","org92091_c","org92092_c","org92093_c",
               "org92094_c","org92095_c","org92096_c","org92097_c",
               "org92098_c","org92099_c","org92100_c","org92101_c",
               "org92102_c","org92103_c","org92104_c","org92105_c",
               "org92106_c","org92107_c")
      ),
      NR = list(
        w3 = c("nrg70101_c","nrg70102_c","nrg70103_c",
               "nrg70104_c","nrg70105_c","nrg70201_c",
               "nrg70202_c","nrg70203_c","nrg70204_c",
               "nrg70205_c","nrg70206_c","nrg70301_c",
               "nrg70302_c","nrg70303_c","nrg70304_c",
               "nrg70401_c","nrg70402_c","nrg70403_c",
               "nrg70405_c","nrg70501_c","nrg70502_c",
               "nrg70504_c","nrg70601_c","nrg70602_c",
               "nrg70603_c","nrg70604_c","nrg70701_c",
               "nrg70702_c","nrg70703_c","nrg70704_c",
               "nrg70705_c"),
        w6 = c("nrg90101_sc3g9_c","nrg90102_sc3g9_c","nrg90103_sc3g9_c",
               "nrg90201_sc3g9_c","nrg90202_sc3g9_c","nrg90203_sc3g9_c",
               "nrg90301_sc3g9_c","nrg90302_sc3g9_c","nrg90303_sc3g9_c",
               "nrg90304_sc3g9_c","nrg90401_sc3g9_c","nrg90402_sc3g9_c",
               "nrg90403_sc3g9_c","nrg90404_sc3g9_c","nrg90405_sc3g9_c",
               "nrg90501_sc3g9_c","nrg90502_sc3g9_c","nrg90503_sc3g9_c",
               "nrg90504_sc3g9_c","nrg90505_sc3g9_c","nrg90506_sc3g9_c",
               "nrg90601_sc3g9_c","nrg90602_sc3g9_c","nrg90603_sc3g9_c",
               "nrg90604_sc3g9_c","nrg90605_sc3g9_c","nrg90701_sc3g9_c",
               "nrg90702_sc3g9_c","nrg90703_sc3g9_c","nrg90704_sc3g9_c",
               "nrg90705_sc3g9_c","nrg90706_sc3g9_c")
      ),
      NT = list(
        w3 = c("ntg70101_c","ntg70102_c","ntg70103_c",
               "ntg70104_c","ntg70105_c","ntg70201_c",
               "ntg70202_c","ntg70203_c","ntg70204_c",
               "ntg70205_c","ntg70206_c","ntg70301_c",
               "ntg70302_c","ntg70303_c","ntg70304_c",
               "ntg70401_c","ntg70402_c","ntg70403_c",
               "ntg70405_c","ntg70501_c","ntg70502_c",
               "ntg70504_c","ntg70601_c","ntg70602_c",
               "ntg70603_c","ntg70604_c","ntg70701_c",
               "ntg70702_c","ntg70703_c","ntg70704_c",
               "ntg70705_c"),
        w6 = c("ntg90101_sc3g9_c","ntg90102_sc3g9_c","ntg90103_sc3g9_c",
               "ntg90201_sc3g9_c","ntg90202_sc3g9_c","ntg90203_sc3g9_c",
               "ntg90301_sc3g9_c","ntg90302_sc3g9_c","ntg90303_sc3g9_c",
               "ntg90304_sc3g9_c","ntg90401_sc3g9_c","ntg90402_sc3g9_c",
               "ntg90403_sc3g9_c","ntg90404_sc3g9_c","ntg90405_sc3g9_c",
               "ntg90501_sc3g9_c","ntg90502_sc3g9_c","ntg90503_sc3g9_c",
               "ntg90504_sc3g9_c","ntg90505_sc3g9_c","ntg90506_sc3g9_c",
               "ntg90601_sc3g9_c","ntg90602_sc3g9_c","ntg90603_sc3g9_c",
               "ntg90604_sc3g9_c","ntg90605_sc3g9_c","ntg90701_sc3g9_c",
               "ntg90702_sc3g9_c","ntg90703_sc3g9_c","ntg90704_sc3g9_c",
               "ntg90705_sc3g9_c","ntg90706_sc3g9_c")
      ),
      LI = list(
        w6 = c("lig9011s_c","lig9012s_c","lig9013s_c",
               "lig9014s_c","lig9015s_c","lig9016s_c",
               "lig9017s_c","lig9018s_c","lig9021s_c",
               "lig9022s_c","lig9023s_c","lig9024s_c",
               "lig9025s_c","lig9026s_c","lig9027s_c",
               "lig9028s_c")
      ),
      EF = list(
        w7 = c("efg10022s_sc3g10_c","efg10108s_sc3g10_c","efg10094s_sc3g10_c",
               "efg10059s_sc3g10_c","efg10002s_sc3g10_c","efg10008s_sc3g10_c",
               "efg10098s_sc3g10_c","efg10065a_sc3g10_c","efg10065b_sc3g10_c",
               #"efg10065c_sc3g10_c",
               "efg10065d_sc3g10_c","efg10075s_sc3g10_c",
               "efg10057a_sc3g10_c"),
        w9 = c("efg10022s_sc3g12_c", "efg12b00s_sc3g12_c", "efg10108s_sc3g12_c",
               "efg12d001_sc3g12_c", "efg12d002_sc3g12_c", "efg12d003_sc3g12_c",
               "efg12d004_sc3g12_c", "efg12d005_sc3g12_c")
      ),
      ST = list(
        w9 = c(
          "stg12nhs_sc3g12_c", "stg12egs_sc3g12_c", "stg12mts_sc3g12_c",
          "stg12cws_sc3g12_c", "stg12pds_sc3g12_c"
          # "stg12nh01_sc3g12_c", "stg12nh02_sc3g12_c", "stg12nh03_sc3g12_c",
          # "stg12nh04_sc3g12_c", "stg12nh05_sc3g12_c", "stg12eg01_sc3g12_c",
          # "stg12eg02_sc3g12_c", "stg12eg03_sc3g12_c", "stg12eg04_sc3g12_c",
          # "stg12eg05_sc3g12_c", "stg12eg06_sc3g12_c", "stg12eg07_sc3g12_c",
          # "stg12mt01_sc3g12_c", "stg12mt02_sc3g12_c", "stg12mt03_sc3g12_c",
          # "stg12mt04_sc3g12_c", "stg12mt05_sc3g12_c", "stg12cmt06_sc3g12_c",
          # "stg12cw01_sc3g12_c", "stg12cw02_sc3g12_c", "stg12cw03_sc3g12_c",
          # "stg12cw04_sc3g12_c", "stg12cw05_sc3g12_c", "stg12cw06_sc3g12_c",
          # "stg12cw07_sc3g12_c", "stg12pd01_sc3g12_c", "stg12pd02_sc3g12_c",
          # "stg12cpd03_sc3g12_c", "stg12pd04_sc3g12_c", "stg12pd05_sc3g12_c",
          # "stg12pd06_sc3g12_c", "stg12pd07_sc3g12_c"
        )
      )
    ),
    SC4 = list(
      RE = list(
        w2 = c(
          "reg90110_c", "reg90120_c", "reg90150_c", "reg9016s_c",
          "reg9017s_c", "reg90210_c", "reg90220_c", "reg90230_c",
          "reg90240_c", "reg90250_c", "reg90310_c", "reg90320_c",
          "reg9033s_c", "reg90340_c", "reg90350_c", "reg90360_c",
          "reg90370_c", "reg90410_c", "reg90420_c", "reg90430_c",
          "reg90440_c", "reg90450_c", "reg90460_c", "reg9047s_c",
          "reg90510_c", "reg90520_c", "reg90530_c", "reg90540_c",
          "reg90550_c", "reg90560_c", "reg90570_c"
        ),
        w7 = c(
          "reg120110_c", "reg120120_c", "reg120130_c", "reg12014s_c", "reg120150_c",
          "reg120160_c", "reg120170_c", "reg12021s_c", "reg120220_c", "reg120230_c",
          "reg12024s_c", "reg120250_c", "reg12026s_c", "reg120310_c", "reg120320_c",
          "reg120330_c", "reg120340_c", "reg120350_c", "reg120360_c", # "reg12041s_c",
          "reg12042s_c", "reg120430_c", "reg12044s_c", "reg120450_c", "reg120510_c",
          "reg12052s_c", "reg120530_c", "reg120540_c", "reg12055s_c", "reg120560_c",
          "reg120610_c", "reg120620_c", "reg120630_c", "reg120640_c", "reg12065s_c",
          "reg120660_c", "reg120670_c", "reg12071s_c", "reg120720_c", "reg120730_c",
          "reg120740_c", "reg12075s_c"
        ),
        w10 = c(
          "rea90101s_sc4a10_c", "rea90102s_sc4a10_c", "rea901030_sc4a10_c",
          "rea90104s_sc4a10_c", "rea90105s_sc4a10_c",
          "rea906010_sc4a10_c", "rea906020_sc4a10_c",
          "rea906030_sc4a10_c", "rea90604s_sc4a10_c",
          "rea90201s_sc4a10_c", "rea902020_sc4a10_c", "rea902030_sc4a10_c",
          "rea902040_sc4a10_c", "rea90205s_sc4a10_c", "rea90206s_sc4a10_c",
          "rea903010_sc4a10_c", "rea903020_sc4a10_c", "rea903030_sc4a10_c",
          "rea903040_sc4a10_c", "rea90305s_sc4a10_c", "rea903060_sc4a10_c",
          "rea90307s_sc4a10_c",
          "rea904010_sc4a10_c", "rea90402s_sc4a10_c", "rea90403s_sc4a10_c",
          "rea904040_sc4a10_c", "rea905010_sc4a10_c", "rea905020_sc4a10_c",
          "rea905030_sc4a10_c", "rea905040_sc4a10_c", "rea905050_sc4a10_c",
          "rea905060_sc4a10_c", "rea907010_sc4a10_c",
          "rea907020_sc4a10_c", "rea907030_sc4a10_c", "rea90704s_sc4a10_c"
        )
      ),
      MA = list(
        w1 = c(
          "mag9q071_c", "mag9v131_c", "mag9v13s_c", "mag9r261_c",
          "mag9r111_c", "mag9d171_c", "mag9d151_c", "mag9r051_c",
          "mag9v011_c", "mag9v012_c", "mag9q161_c", "mag9d201_c",
          "mag9r191_c", "mag9v121_c", "mag9q181_c", "mag9r25s_c",
          "mag9r061_c", "mag9q081_c", "mag9q101_c", "mag9q021_c",
          "mag9v091_c", "mag9q211_c"
        ),
        w7 = c(
          "maa3q071_sc4g12_c", "mag12v101_c", "mag12q121_c", "mag12v122_c",
          "maa3d131_sc4g12_c", "maa3d132_sc4g12_c", "mag12r011_c", "mag12v061_c",
          "mag12r091_c", "mag9r051_sc4g12_c",
          "mag9v011_sc4g12_c", "mag12q081_c", "mag12d021_c", "mag12q051_c",
          "mag9d201_sc4g12_c", "mag9v121_sc4g12_c",
          "maa3r121_sc4g12_c", "mag12q111_c", "mas1q02s_sc4g12_c",
          "mas1d081_sc4g12_c", "maa3d112_sc4g12_c", "mag9r061_sc4g12_c",
          "maa3q101_sc4g12_c", "mag9q101_sc4g12_c", "maa3r011_sc4g12_c",
          #"mag12d071_c",
          "mag12r041_c", "mag12v131_c", "mag12v132_c",
          "mag12d031_c"
        ),
        w10 = c(
          "maa3q071_sc4a10_c", "mag12v101_sc4a10_c", "mag12v122_sc4a10_c",
          "mag12r011_sc4a10_c", "mas1v032_sc4a10_c", "maa9q081_sc4a10_c",
          "maa9r311_sc4a10_c", "maa9d331_sc4a10_c",
          "mag9v011_sc4a10_c", "mag12r091_sc4a10_c", "mas1v062_sc4a10_c",
          "mag9r051_sc4a10_c", "maa9q19s_sc4a10_c", "maa9d09s_sc4a10_c",
          "maa9v193_sc4a10_c", "maa3r081_sc4a10_c",
          "maa9q211_sc4a10_c", "maa9v251_sc4a10_c", "maa9q011_sc4a10_c",
          "mag12q051_sc4a10_c", "maa9v151_sc4a10_c", "maa3v082_sc4a10_c",
          "maa9d13s_sc4a10_c", "mag9d201_sc4a10_c",
          "maa9r03s_sc4a10_c", "maa9q161_sc4a10_c", "mas1q041_sc4a10_c",
          "maa9r221_sc4a10_c", "mas1q02s_sc4a10_c", "maa9r171_sc4a10_c",
          "maa9v141_sc4a10_c", "mas1d081_sc4a10_c",
          "maa3r121_sc4a10_c", "maa9d111_sc4a10_c", "mag12q111_sc4a10_c",
          "maa3d112_sc4a10_c", "maa9r321_sc4a10_c", "mag9r061_sc4a10_c",
          "maa9v27s_sc4a10_c", "maa3q101_sc4a10_c",
          "maa9d051_sc4a10_c", "mas1q011_sc4a10_c", "mag9q101_sc4a10_c",
          # "maa9d23s_sc4a10_c",
          "maa9d121_sc4a10_c", "maa9d20s_sc4a10_c", "maa9r301_sc4a10_c",
          "mag12d031_sc4a10_c", "mag12r041_sc4a10_c",
          "maa9r26s_sc4a10_c", "maa9v07s_sc4a10_c", "maa9v28s_sc4a10_c",
          "mag12v131_sc4a10_c"
        )
      ),
      IC = list(
        w1 = c(
          "icg9101x_c", "icg9102s_c", "icg9103x_c", "icg9104x_c",
          "icg9105x_c", "icg9106x_c", "icg9107s_c", "icg9109x_c",
          "icg9110x_c", "icg9111x_c", "icg9112x_c", "icg9113x_c",
          "icg9114x_c", "icg9116x_c", "icg9117s_c", "icg9118x_c",
          "icg9119x_c", "icg9121x_c", "icg9122x_c", "icg9123x_c",
          "icg9124x_c", "icg9125s_c", "icg9126x_c", "icg9127x_c",
          "icg9128x_c", "icg9129x_c", "icg9130x_c", "icg9131x_c",
          "icg9132x_c", "icg9133s_c", "icg9134x_c", "icg9135x_c",
          "icg9136s_c", "icg9137x_c", "icg9138x_c", "icg9140s_c"
        ),
        w7 = c(
          "icg12018s_c", "ica5003x_c", "icg12107s_c", "icg12004s_c",
          "icg12010x_c", "icg12011x_c", "ica5008x_c", "icg12060s_c",
          "icg12013s_c", "icg12016s_c", "ica5019x_c",
          "icg12121x_c", "icg12028s_c", "ica5023x_c", "ica5027x_c",
          "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
          "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
          "icg12046s_c", "ica5021s_c", "ica5052s_c", "icg12048s_c",
          "icg12050s_c", "icg12054s_c", "icg12109s_c", "icg12119s_c"
        )
      ),
      SC = list(
        w1 = c(
          "scg90110_c", "scg9012s_c", "scg90510_c",
          "scg9052s_c", "scg90920_c", "scg90930_c",
          "scg9611s_c", "scg96120_c", "scg96410_c",
          "scg96420_c", "scg9061s_c", "scg90630_c",
          "scg90810_c", "scg9083s_c", "scg91030_c",
          "scg91040_c", "scg91050_c", "scg9042s_c",
          "scg9043s_c", "scg9651s_c", "scg96530_c",
          "scg90320_c", "scg90330_c", "scg9621s_c",
          "scg96220_c", "scg91110_c", "scg91120_c",
          "scg91130_c"
        ),
        w5 = c(
          "scg116420_c", "scg110620_c", "scg110630_c", "scg11012s_c",
          "scg11083s_c", "scg110720_c", "scg11032s_c", "scg110330_c",
          "scg116510_c", "scg11652s_c", "scg11602s_c", "scg110510_c",
          "scg110520_c", "scg110540_c", "scg11123s_c", "scg11102s_c",
          "scg11021s_c", "scg11022s_c", "scg11112s_c", "scg116210_c",
          "scg11622s_c", "scg116320_c", "scg110930_c",
          "scs5131s_sc4g11_c"
        )
      ),
      NR = list(
        w2 = c(
          "nrg90101_c", "nrg90102_c", "nrg90103_c", "nrg90201_c",
          "nrg90202_c", "nrg90203_c", "nrg90301_c", "nrg90302_c",
          "nrg90303_c", "nrg90304_c", "nrg90401_c", "nrg90402_c",
          "nrg90403_c", "nrg90404_c", "nrg90405_c", "nrg90502_c",
          "nrg90503_c", "nrg90504_c", "nrg90505_c", "nrg90506_c",
          "nrg90601_c", "nrg90602_c", "nrg90603_c", "nrg90604_c",
          "nrg90605_c", "nrg90701_c", "nrg90702_c", "nrg90703_c",
          "nrg90704_c", "nrg90705_c", "nrg90706_c"
        )
      ),
      NT = list(
        w2 = c(
          "ntg90101_c", "ntg90102_c", "ntg90103_c", "ntg90201_c",
          "ntg90202_c", "ntg90203_c", "ntg90301_c", "ntg90302_c",
          "ntg90303_c", "ntg90304_c", "ntg90401_c", "ntg90402_c",
          "ntg90403_c", "ntg90404_c", "ntg90405_c", "ntg90502_c",
          "ntg90503_c", "ntg90504_c", "ntg90505_c", "ntg90506_c",
          "ntg90601_c", "ntg90602_c", "ntg90603_c", "ntg90604_c",
          "ntg90605_c", "ntg90701_c", "ntg90702_c", "ntg90703_c",
          "ntg90704_c", "ntg90705_c", "ntg90706_c"
        )
      ),
      EF = list(
        w3 = c(
          "efg10022s_c", "efg10108s_c", "efg10094s_c", "efg10059s_c",
          "efg10002s_c", "efg10008s_c", "efg10098s_c", "efg10065a_c",
          "efg10065b_c", # "efg10065c_c",
          "efg10065d_c", "efg10075s_c",
          "efg10057a_c"
        ),
        w7 = c(
          "efg10022s_sc4g12_c", "efg12b00s_c", "efg10108s_sc4g12_c",
          "efg12d001_c", "efg12d002_c", "efg12d003_c", "efg12d004_c",
          "efg12d005_c"
        )
      ),
      ST = list(
        w7 = c(
          "stg12nh01_c", "stg12nh02_c", "stg12nh03_c",
          "stg12nh04_c", "stg12nh05_c", "stg12eg01_c",
          "stg12eg02_c", "stg12eg03_c", "stg12eg04_c",
          "stg12eg05_c", "stg12eg06_c", "stg12eg07_c",
          "stg12mt01_c", "stg12mt02_c", "stg12mt03_c",
          "stg12mt04_c", "stg12mt05_c", "stg12cmt06_c",
          "stg12cw01_c", "stg12cw02_c", "stg12cw03_c",
          "stg12cw04_c", "stg12cw05_c", "stg12cw06_c",
          "stg12cw07_c", "stg12pd01_c", "stg12pd02_c",
          "stg12cpd03_c", "stg12pd04_c", "stg12pd05_c",
          "stg12pd06_c", "stg12pd07_c"
        )
      )
    ),
    SC5 = list(
      RE = list(
        w1 = c(
          "res10110_c", "res1012s_c", "res10130_c", "res10140_c",
          "res10160_c", "res10170_c", "res10180_c", "res10190_c",
          "res1021s_c", "res1022s_c", "res10230_c", "res1024s_c",
          "res10250_c", "res10260_c", "res10270_c", "res10310_c",
          "res1032s_c", "res10330_c", "res10340_c", "res10350_c",
          "res10360_c", "res10370_c", "res10380_c", "res10410_c",
          "res10420_c", "res1043s_c", "res10440_c", "res10450_c"
        ),
        w12 = c(
          "rea906010_sc5s12_c", "rea906020_sc5s12_c",
          "rea906030_sc5s12_c", "rea90604s_sc5s12_c",
          "rea90201s_sc5s12_c", "rea902020_sc5s12_c", "rea902030_sc5s12_c",
          "rea90205s_sc5s12_c", "rea90206s_sc5s12_c", "rea903010_sc5s12_c",
          "rea903020_sc5s12_c", "rea903030_sc5s12_c", "rea903040_sc5s12_c",
          "rea90305s_sc5s12_c", "res1203080_c", "rea904010_sc5s12_c",
          "rea90402s_sc5s12_c", "res1204050_c",
          "rea907010_sc5s12_c", "rea907020_sc5s12_c", "rea90704s_sc5s12_c"
        )
      ),
      MA = list(
        w1 = c(
          "maa2q071_sc5s1_c", "mas1r092_c", "mas1v093_c",
          "mas1v032_c", "maa2d131_sc5s1_c", "maa2d132_sc5s1_c",
          "mas1v062_c", "mas1v063_c", "maa2r081_sc5s1_c",
          "maa2v082_sc5s1_c", "mas1q041_c", "mas1v042_c",
          "mas1q02s_c", "maa2d111_sc5s1_c", "maa2d112_sc5s1_c",
          "maa2r011_sc5s1_c", "mas1q011_c", "mag9r061_sc5s1_c",
          "mas1d071_c", "mas1d072_c"
        ),
        w12 = c(
          "maa3q071_sc5s12_c", "mag12v101_sc5s12_c", "mag12v122_sc5s12_c",
          "mag12r011_sc5s12_c", "mas1v032_sc5s12_c", "maa9q081_sc5s12_c",
          "maa9r311_sc5s12_c", "maa9d331_sc5s12_c",
          "mag9v011_sc5s12_c", "mag12r091_sc5s12_c", "mas1v062_sc5s12_c",
          "mag9r051_sc5s12_c", "maa9q19s_sc5s12_c", "maa9d09s_sc5s12_c",
          "maa9v193_sc5s12_c", "maa3r081_sc5s12_c",
          "maa9q211_sc5s12_c", "maa9v251_sc5s12_c", "maa9q011_sc5s12_c",
          "mag12q051_sc5s12_c", "maa9v151_sc5s12_c", "maa3v082_sc5s12_c",
          "maa9d13s_sc5s12_c", "mag9d201_sc5s12_c",
          "maa9r03s_sc5s12_c", "maa9q161_sc5s12_c", "mas1q041_sc5s12_c",
          "maa9r221_sc5s12_c", "mas1q02s_sc5s12_c", "maa9r171_sc5s12_c",
          "maa9v141_sc5s12_c", "mas1d081_sc5s12_c",
          "maa3r121_sc5s12_c", "maa9d111_sc5s12_c", "mag12q111_sc5s12_c",
          "maa3d112_sc5s12_c", "maa9r321_sc5s12_c", "mag9r061_sc5s12_c",
          "maa9v27s_sc5s12_c", "maa3q101_sc5s12_c",
          "maa9d051_sc5s12_c", "mas1q011_sc5s12_c", "mag9q101_sc5s12_c",
          # "maa9d23s_sc5s12_c",
          "maa9d121_sc5s12_c", "maa9d20s_sc5s12_c", "maa9r301_sc5s12_c",
          "mag12d031_sc5s12_c", "mag12r041_sc5s12_c",
          "maa9r26s_sc5s12_c", "maa9v07s_sc5s12_c", "maa9v28s_sc5s12_c",
          "mag12v131_sc5s12_c"
        )
      ),
      IC = list(
        w5 = c(
          "ics3001x_c", "ics3002s_c", "ics3003x_c", "ics3010x_c", "ics3011x_c",
          "ics3012x_c", "ics3014x_c", "ics3024x_c", "ics3015x_c", "ics3031x_c",
          "ics3038x_c", "ics3019x_c", "ics3023x_c", "ics3028x_c", "ics3029s_c",
          "ics3048x_c", "ics3049s_c", "ics3018s_c", "ics3041x_c", "ics3043x_c",
          "ics3030x_c", "ics3032x_c", "ics3033x_c", "ics3034x_c", "ics3045x_c",
          "ics3035s_c", "ics3037x_c", "ics3042x_c", "ics3044x_c", "ics3047x_c"
        )
      ),
      SC = list(
        w5 = c(
          "scs36310_c", "scs36320_c", "scs36220_c", "scs3623s_c", "scs30510_c",
          "scs30520_c", "scs31210_c", "scs31220_c", "scs31240_c", "scs30920_c",
          "scs30930_c", "scs30940_c", "scs3021s_c", "scs3022s_c", "scs36020_c",
          "scs3643s_c", "scs3642s_c", "scs3031s_c", "scs3033s_c", "scs3112s_c",
          "scs3131s_c", "scs3132s_c", "scs3133s_c", "scs3012s_c", "scs30130_c",
          "scs3061s_c", "scs30630_c", "scs30640_c", "scs30810_c"
        )
      ),
      BA = list(
        w7 = c(
          "bas7mar1_c", "bas7mar2_c", "bas7mar3_c", "bas7mar4_c", "bas7mar5_c",
          "bas7mar6_c", "bas7org1_c", "bas7org2_c", "bas7org3_c", "bas7org4_c",
          "bas7org5_c", "bas7org6_c", "bas7fin1_c", "bas7fin2_c", "bas7fin3_c",
          "bas7fin4_c", "bas7fin5_c", "bas7fin6_c", "bas7acc1_c", "bas7acc2_c",
          "bas7acc3_c", "bas7acc4_c", "bas7acc5_c", "bas7acc6_c", "bas7mic1_c",
          "bas7mic2_c", "bas7mic3_c", "bas7mic4_c", "bas7mic5_c", # "bas7mic6_c", # removed due to dif
          # "bas7mac1_c",
          "bas7mac2_c", "bas7mac3_c", "bas7mac4_c", "bas7mac5_c",
          "bas7mac6_c"
        )
      ),
      EF = list(w12 = c(
        "efs121010_c", "efs121020_c", "efs121030_c", "efs121040_c",
        "efs121050_c", "efs121060_c", "efs121070_c", "efs121080_c",
        "efs121090_c", "efs121100_c", "efs122010_c", "efs122020_c",
        "efs122030_c", "efs122040_c", "efs122050_c", "efs123011_c",
        "efs123012_c", "efs124010_c", "efs12402s_c", "efs124030_c",
        "efs125010_c", "efs125020_c", "efs125030_c"
      ))
    ),
    SC6 = list(
      RE = list(
        w3 = c(
          "rea30110_c", "rea3012s_c", "rea30130_c", "rea30140_c", "rea3015s_c", "rea30210_c",
          "rea30220_c", "rea30230_c", "rea30240_c", "rea30250_c", "rea3028s_c", "rea30310_c",
          "rea30320_c", "rea30330_c", "rea30340_c", "rea30350_c", "rea30360_c", "rea30370_c",
          "rea3038s_c", "rea30410_c", "rea3042s_c", "rea30430_c", "rea30440_c", "rea30450_c",
          "rea30460_c", "rea30510_c", "rea3052s_c", "rea30530_c", "rea3054s_c", "rea30550_c"
        ),
        w5 = c(
          "rea30110_c", "rea3012s_c", "rea30130_c", "rea30140_c", "rea3015s_c", "rea30210_c",
          "rea30220_c", "rea30230_c", "rea30240_c", "rea30250_c", "rea3028s_c", "rea30310_c",
          "rea30320_c", "rea30330_c", "rea30340_c", "rea30350_c", "rea30360_c", "rea30370_c",
          "rea3038s_c", "rea30410_c", "rea3042s_c", "rea30430_c", "rea30440_c", "rea30450_c",
          "rea30460_c", "rea30510_c", "rea3052s_c", "rea30530_c", "rea3054s_c", "rea30550_c"
        ),
        w9 = c(
          "rea90101s_c", "rea90102s_c", "rea901030_c", "rea90104s_c", "rea90105s_c",
          "rea906010_c", "rea906020_c", "rea906030_c", "rea90604s_c",
          "rea90201s_c", "rea902020_c", "rea902030_c", "rea902040_c", "rea90205s_c",
          "rea90206s_c",
          "rea903010_c", "rea903020_c", "rea903030_c", "rea903040_c", "rea90305s_c",
          "rea903060_c", "rea90307s_c",
          "rea904010_c", "rea90402s_c", "rea90403s_c", "rea904040_c",
          "rea905010_c", "rea905020_c", "rea905030_c", "rea905040_c", "rea905050_c",
          "rea905060_c",
          "rea907010_c", "rea907020_c", "rea907030_c", "rea90704s_c"
        )
      ),
      MA = list(
        w3 = c(
          "maa3q071_c", "mag9v131_sc6a3_c", "mag9r261_sc6a3_c", "mag9r111_sc6a3_c",
          "maa3d131_c", "maa3d132_c", "mag9r051_sc6a3_c", "maa3d041_c", "maa3r081_c",
          "maa3v082_c", "mag9d201_sc6a3_c", "maa3r091_c", "mag9v121_sc6a3_c",
          "maa3r121_c", "maa3d112_c", "maa3r011_c", "maa3q101_c", "mag5v321_sc6a3_c",
          "mag9q021_sc6a3_c", "maa3v061_c", "maa3q021_c"
        ),
        w9 = c(
          "maa3q071_sc6a9_c", "mag12v101_sc6a9_c", "mag12v122_sc6a9_c",
          "mag12r011_sc6a9_c", "mas1v032_sc6a9_c", "maa9q081_c",
          "maa9r311_c", "maa9d331_c",
          "mag9v011_sc6a9_c", "mag12r091_sc6a9_c", "mas1v062_sc6a9_c",
          "mag9r051_sc6a9_c", "maa9q19s_c", "maa9d09s_c", "maa9v193_c",
          "maa3r081_sc6a9_c",
          "maa9q211_c", "maa9v251_c", "maa9q011_c", "mag12q051_sc6a9_c",
          "maa9v151_c", "maa3v082_sc6a9_c", "maa9d13s_c", "mag9d201_sc6a9_c",
          "maa9r03s_c", "maa9q161_c", "mas1q041_sc6a9_c", "maa9r221_c",
          "mas1q02s_sc6a9_c", "maa9r171_c", "maa9v141_c", "mas1d081_sc6a9_c",
          "maa3r121_sc6a9_c", "maa9d111_c", "mag12q111_sc6a9_c",
          "maa3d112_sc6a9_c", "maa9r321_c", "mag9r061_sc6a9_c", "maa9v27s_c",
          "maa3q101_sc6a9_c",
          "maa9d051_c", "mas1q011_sc6a9_c", "mag9q101_sc6a9_c", "maa9d121_c",
          "maa9d20s_c", "maa9r301_c", "mag12d031_sc6a9_c", "mag12r041_sc6a9_c",
          "maa9r26s_c", "maa9v07s_c", "maa9v28s_c", "mag12v131_sc6a9_c"
        )
      ),
      SC = list(
        w5 = c(
          "sca56120_c", "sca56130_c", "sca51110_c", "sca51140_c", "sca50410_c", "sca5652s_c",
          "sca56540_c", "sca51430_c", "sca51440_c", "sca50210_c", "sca50220_c", "sca50710_c",
          "sca50720_c", "sca56310_c", "sca56320_c", "sca5091s_c", "sca56020_c", "sca56030_c",
          "sca50520_c", "sca50530_c", "sca51020_c", "sca51030_c"
        )
      ),
      IC = list(
        w5 = c(
          "ica5001x_c", "ica5003x_c", "ica5005x_c", "ica5004s_c", "ica5006x_c", "ica5007x_c", "ica5008x_c",
          "ica5010x_c", "ica5017s_c", "ica5018s_c", "ica5015s_c", "ica5019x_c", "ica5016s_c", "ica5020s_c",
          "ica5023x_c", "ica5027x_c", "ica5026x_c", "ica5029x_c", "ica5028x_c", "ica5030x_c",
          "icg9119x_sc6a5_c", "ica5050s_c", "icg9122x_sc6a5_c", "ica5047s_c", "ica5046x_c", "ica5021s_c",
          "ica5052s_c", "ica5054x_c", "ica5057x_c"
        )
      )
    )
  )

# testlet variable
testlet_position <- list(
  SC1 = list(
    CD = list(
      w1 = matrix(c(NA,
                    NA), 1, 2)
    ),
    MA = list(
      w5 = matrix(c(599,
                    599), 1, 2),
      w7 = matrix(c(NA,
                    NA), 1, 2)#,
      # w9 = matrix(c(NA, NA), 1, 2)
    ),
    SC = list(
      w6 = matrix(c(600,
                    600), 1, 2)#,
      # w8 = matrix(c(NA, NA), 1, 2)
    )
  ),
  SC2 = list(
    RE = list(
      w6 = matrix(c(405,
                    405), 1, 2)#,
      # w9 = matrix(c(NA, NA), 1, 2)
    ),
    MA = list(
      w2 = matrix(c(152,
                    152), 1, 2),
      w3 = matrix(c(252, 253, 254, 255, 256, 257, 258, 259,
                    270, 271, 273, 274, 275, 276, 277, NA), 8, 2),
      w4 = matrix(c(391,
                    391), 1, 2),
      w6 = matrix(c(405,
                    405), 1, 2)#,
      # w9 = matrix(c(NA, NA), 1, 2)
    ),
    IC = list(
      w5 = matrix(c(366, 367,
                    366, 367), 2, 2)
    ),
    SC = list(
      w1 = matrix(c(131,
                    131), 1, 2),
      w3 = matrix(c(270, 271, 273, 274, 275, 276, 277, NA,
                    252, 253, 254, 255, 256, 257, 258, 259), 8, 2),
      w5 = matrix(c(366, 367,
                    366, 367), 2, 2)#,
      # w9 = matrix(c(NA, NA), 1, 2)
    ),
    NR = list(
      w4 = matrix(c(391,
                    391), 1, 2)
    ),
    NT = list(
      w4 = matrix(c(391,
                    391), 1, 2)
    ),
    ORA = list(
      w6 = matrix(c(405,
                    405), 1, 2)
    ),
    ORB = list(
      w6 = matrix(c(405,
                    405), 1, 2)
    ),
    VO = list( # VO-GR always in same order, but rotated with MD and on day 1 or 2
      w1 = matrix(c(131,
                    131), 1, 2),
      w3 = matrix(c(252, 253, 254, 255, 256, 257, 258, 259, 270, 271, 272, 273, 274, 275, 276, 277,
                    252, 253, 254, 255, 256, 257, 258, 259, 270, 271, 272, 273, 274, 275, 276, 277), 16, 2),
      w5 = matrix(c(366, 367,
                    366, 367), 2, 2)
    ),
    GR = list(
      w1 = matrix(c(131,
                    131), 1, 2),
      w3 = matrix(c(252, 253, 254, 255, 256, 257, 258, 259, 270, 271, 272, 273, 274, 275, 276, 277,
                    252, 253, 254, 255, 256, 257, 258, 259, 270, 271, 272, 273, 274, 275, 276, 277), 16, 2)
    )
  ),
  SC3 = list(
    RE = list(
      w1 = matrix(c(132,
                    133), 1, 2),
      w3 = matrix(c(263, 265,
                    262, 264), 2, 2),
      w6 = matrix(c(374, 375,
                    374, 375), 2, 2),
      w9 = matrix(c(627, 628, 629, 630,
                    623, 624, 625, 626), 4, 2)
    ),
    MA = list(
      w1 = matrix(c(133,
                    132), 1, 2),
      w3 = matrix(c(262, 264,
                    263, 265), 2, 2),
      w5 = matrix(c(539:592,
                    539:592), 54, 2),
      w9 = matrix(c(623, 624, 625, 626, 627, 628, 629, 630,
                    623, 624, 625, 626, 627, 628, 629, 630), 8, 2)
    ),
    IC = list(
      w2 = matrix(c(169,
                    170), 1, 2),
      w5 = matrix(c(539:565,
                    566:592), 27, 2),
      w9 = matrix(c(623, 624, 625, 626,
                    627, 628, 629, 630), 4, 2)
    ),
    SC = list(
      w2 = matrix(c(170,
                    169), 1, 2),
      w5 = matrix(c(566:592,
                    539:565), 27, 2),
      w8 = matrix(c(493,
                    493), 1, 2)
    ),
    ORA = list(
      w1 = matrix(c(132, 133,
                    132, 133), 2, 2),
      w3 = matrix(c(262, 263, 264, 265,
                    262, 263, 264, 265), 4, 2),
      w5 = matrix(c(539:592,
                    539:592), 54, 2)
    ),
    ORB = list(
      w1 = matrix(c(132, 133,
                    132, 133), 2, 2),
      w3 = matrix(c(262, 263, 264, 265,
                    262, 263, 264, 265), 4, 2),
      w5 = matrix(c(539:592,
                    539:592), 54, 2)
    ),
    NR = list(
      w3 = matrix(c(262, 263, 264, 265,
                    262, 263, 264, 265), 4, 2),
      w6 = matrix(c(374, 375,
                    374, 375), 2, 2)
    ),
    NT = list(
      w3 = matrix(c(262, 263, 264, 265,
                    262, 263, 264, 265), 4, 2),
      w6 = matrix(c(374, 375,
                    374, 375), 2, 2)
    ),
    LI = list(
      w6 = matrix(c(374, 375,
                    374, 375), 2, 2)
    ),
    EF = list(
      w7 = matrix(c(414, 415, 416,
                    414, 415, 416), 3, 2),
      w9 = matrix(c(623, 625, 627, 629,
                    624, 626, 628, 630), 4, 2)
    ),
    ST = list(
      w9 = matrix(c(624, 626, 628, 630,
                    623, 625, 627, 629), 4, 2)
    )
  ),
  SC4 = list(
    RE = list(
      w2 = matrix(c(130,
                    130), 1, 2),
      w7 = matrix(c(283, 284, 285, 286, 287, 288, 300, 301, 302, 303,
                    281, 282, 289, 290, 291, 292, 296, 297, 298, 299), 10, 2),
      w10 = matrix(c(470, 471, 474, 475,
                     472, 473, NA, NA), 4, 2)
    ),
    MA = list(
      w1 = matrix(c(128, 129,
                    128, 129), 2, 2),
      w7 = matrix(c(289, 290, 291, 292, 293, 294, rep(NA, 6),
                    284, 285, 287, 288, 296, 297, 298, 299, 300, 301, 302, 303), 12, 2),
      w10 = matrix(c(472, 473, 476,
                     470, 471, NA), 3, 2)
    ),
    IC = list(
      w1 = matrix(c(128,
                    129), 1, 2),
      w7 = matrix(c(281, 282, 295, 296, 297, 298, 299,
                    283, 286, 300, 301, 302, 303, NA), 7, 2)
    ),
    SC = list(
      w1 = matrix(c(129,
                    128), 1, 2),
      w5 = matrix(c(206,
                    206), 1, 2)
    ),
    NR = list(
      w2 = matrix(c(130,
                    130), 1, 2)
    ),
    NT = list(
      w2 = matrix(c(130,
                    130), 1, 2)
    ),
    EF = list(
      w3 = matrix(c(NA,
                    NA), 1, 2),
      w7 = matrix(c(298, 299, 302, 303,
                    296, 297, 300, 301), 4, 2)
    ),
    ST = list(
      w7 = matrix(c(296, 297, 300, 301,
                    298, 299, 302, 303), 4, 2)
    )
  ),
  SC5 = list(
    RE = list(
      w1 = matrix(c(126,
                    127), 1, 2),
      w12 = matrix(c(459, 462, 465, 468,
                     458, 463, 464, 469), 4, 2)
    ),
    MA = list(
      w1 = matrix(c(127,
                    126), 1, 2),
      w12 = matrix(c(458, 460, 464, 466,
                     459, 461, 465, 467), 4, 2)
    ),
    IC = list(
      w5 = matrix(c(331, 333, 335, NA,
                    330, 332, 334, 336), 4, 2)
    ),
    SC = list(
      w5 = matrix(c(330, 332, 334, 336,
                    331, 333, 335, NA), 4, 2)
    ),
    BA = list(
      w7 = matrix(c(NA,
                    NA), 1, 2)
    ),
    EF = list(w12 = matrix(c(461, 463, 467, 469,
                             460, 462, 466, 468), 4, 2))
  ),
  SC6 = list(
    RE = list(
      w3 = matrix(c(123, 125,
                    122, 124), 2, 2),
      w5 = matrix(c(249,
                    249), 1, 2),
      w9 = matrix(c(444, 445, 448, 449, 452, 453, 454, 455,
                    446, 447, 450, 451, 456, 457, NA, NA), 8, 2)
    ),
    MA = list(
      w3 = matrix(c(122, 124,
                    123, 125), 2, 2),
      w9 = matrix(c(446, 447, 450, 451, 456, 457, NA, NA,
                    444, 445, 448, 449, 452, 453, 454, 455), 8, 2)
    ),
    SC = list(
      w5 = matrix(c(247,
                    248), 1, 2)
    ),
    IC = list(
      w5 = matrix(c(248,
                    247), 1, 2)
    )
  )
)

# # wle variable names for calculating longitudinal IDs
# wle_names <- list(
#   SC1 = list(
#     CD = list(w1 = "cdn1_sc1"),
#     SC = list(w6 = "scn6_sc1"),
#     MA = list(
#       w5 = "man5_sc1", w7 = "man7_sc1u"
#     )
#   ),
#   SC2 = list(
#     SC = list(
#       w1 = "sck1_sc1u", w3 = "scg1_sc1u", w5 = "scg3_sc1u"
#     ),
#     MA = list(
#       w2 = "mak2_sc1u", w3 = "mag1_sc1u", w4 = "mag2_sc1u", w6 = "mag4_sc1u"
#     ),
#     VO = list(
#       w1 = "vok1_sc1u", w3 ="vog1_sc1u", w5 = "vog3_sc1u"
#     ),
#     GR = list(w1 = "grk1_sc1u", w3 = "grg1_sc1u"),
#     RE = list(
#       w4 = "reg2_sc1u", w6 = "reg4_sc1u"
#     ),
#     NR = list(w4 = "nrg2_sc1u"),
#     NT = list(w4 = "ntg2_sc1u"),
#     IC = list(w5 = "icg3_sc1u"),
#     ORA = list(w6 = "org4_sc1a"),
#     ORB = list(w6 = "org4_sc1b")
#   ),
#   SC3 = list(
#     MA = list(
#       w1 = "mag5_sc1u", w3 = "mag7_sc1u", w5 = "mag9_sc1u", w9 = "mag12_sc1u"
#     ),
#     RE = list(
#       w1 = "reg5_sc1u", w3 = "reg7_sc1u", w6 = "reg9_sc1u", w9 = "reg12_sc1u"
#     ),
#     ORA = list(w1 = "org5_sc1a", w3 = "org7_sc1a", w5 = "org9_sc1a"), # !
#     ORB = list(w1 = "org5_sc1b", w3 = "org7_sc1b", w5 = "org9_sc1b"), # !
#     SC = list(
#       w2 = "scg6_sc1", w5 = "scg9_sc1u", w8 = "scg11_sc1" # !
#     ),
#     IC = list(
#       w2 = "icg6_sc1u", w5 = "icg9_sc1u", w9 = "icg12_sc1u"
#     ),
#     NR = list(w3 = "nrg7_sc1", w6 = "nrg9_sc3g9_sc1"), # !
#     NT = list(w3 = "ntg7_sc1", w6 = "ntg9_sc3g9_sc1"), # !
#     LI = list(w6 = "lig9_sc1u"),
#     EF = list(w7 = "efg10_sc1u", w9 = "efg12_sc1u"),
#     ST = list(w9 = "stg12_sc1u")
#   ),
#   SC4 = list(
#     SC = list(w1 = "scg9_sc1u", w5 = "scg11_sc1u"),
#     MA = list(w1 = "mag9_sc1u", w7 = "mag12_sc1u", w10 = "maa10_sc1u"),
#     IC = list(w1 = "icg9_sc1u", w7 = "icg12_sc1u"),
#     NR = list(w2 = "nrg9_sc1u"),
#     NT = list(w2 = "ntg9_sc1u"),
#     RE = list(w2 = "reg9_sc1u", w7 = "reg12_sc1u", w10 = "rea10_sc1u"),
#     EF = list(w3 = "efg10_sc1u", w7 = "efg12_sc1u"),
#     ST = list(w7 = "stg12_sc1u")
#   ),
#   SC5 = list(
#     MA = list(w1 = "mas1_sc1u", w12 = "mas12_sc1u"),
#     RE = list(w1 = "res1_sc1u", w12 = "res12_sc1u"),
#     SC = list(w5 = "scs3_sc1u"),
#     IC = list(w5 = "ics3_sc1u"),
#     BA = list(w7 = "bas7_sc1u"),
#     EF = list(w12 = "efs12_sc1u")
#   ),
#   SC6 = list(
#     MA = list(w3 = "maa3_sc1u", w9 = "maa9_sc1u"),
#     RE = list(w3 = "rea3_sc1u", w5 = "rea5_sc1u", w9 = "rea9_sc1u"),
#     SC = list(w5 = "sca5_sc1u"),
#     IC = list(w5 = "ica5_sc1u")
#   )
# )

# item difficulties for competence tests
load(file = "data-raw/xsi_fixed.RData")

# difference matrix for SC4 English w3, w7
load(file = "data-raw/diffMat.RData")

usethis::use_data(
  item_labels, link_constant, xsi.fixed, diffMat, testlet_position, #wle_names,
  internal = TRUE, overwrite = TRUE
  )
