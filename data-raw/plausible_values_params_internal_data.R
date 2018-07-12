## Code providing the internal data (item difficulties and WLEs)
## for plausible values estimation.
## The parameters are prepared in advance to ensure that proper
## corrections for test design changes, dropout etc. are applied
rm(list = ls())

# EAPs without background model
load(file = 'data-raw/eaps.RData')

# fixed item parameters for SC6 reading wave 5
load(file = "data-raw/item_diff_SC6_RE_w3.RData")

# latent mean and variance of ability
load(file = "data-raw/meanvar.RData")

# item labels for item selection
item_labels <- list(SC5 = list(RE = list(w1 = c("res10110_c", "res1012s_c", "res10130_c", "res10140_c",
                                                "res10160_c", "res10170_c", "res10180_c", "res10190_c",
                                                "res1021s_c", "res1022s_c", "res10230_c", "res1024s_c",
                                                "res10250_c", "res10260_c", "res10270_c", "res10310_c",
                                                "res1032s_c", "res10330_c", "res10340_c", "res10350_c",
                                                "res10360_c", "res10370_c", "res10380_c", "res10410_c",
                                                "res10420_c", "res1043s_c", "res10440_c", "res10450_c"),
                                         w12 = c()),
                               MA = list(w1 = c("maa2q071_sc5s1_c", "mas1r092_c", "mas1v093_c",
                                                "mas1v032_c", "maa2d131_sc5s1_c", "maa2d132_sc5s1_c",
                                                "mas1v062_c", "mas1v063_c", "maa2r081_sc5s1_c",
                                                "maa2v082_sc5s1_c", "mas1q041_c", "mas1v042_c",
                                                "mas1q02s_c", "maa2d111_sc5s1_c", "maa2d112_sc5s1_c",
                                                "maa2r011_sc5s1_c", "mas1q011_c", "mag9r061_sc5s1_c",
                                                "mas1d071_c", "mas1d072_c"),
                                         w12 = c()),
                               IC = list(w5 = c("ics3001x_c", "ics3002s_c", "ics3003x_c", "ics3010x_c", "ics3011x_c",
                                                "ics3012x_c", "ics3014x_c", "ics3024x_c", "ics3015x_c", "ics3031x_c",
                                                "ics3038x_c", "ics3019x_c", "ics3023x_c", "ics3028x_c", "ics3029s_c",
                                                "ics3048x_c", "ics3049s_c", "ics3018s_c", "ics3041x_c", "ics3043x_c",
                                                "ics3030x_c", "ics3032x_c", "ics3033x_c", "ics3034x_c", "ics3045x_c",
                                                "ics3035s_c", "ics3037x_c", "ics3042x_c", "ics3044x_c", "ics3047x_c")),
                               SC = list(w5 = c("scs36310_c", "scs36320_c", "scs36220_c", "scs3623s_c", "scs30510_c",
                                                "scs30520_c", "scs31210_c", "scs31220_c", "scs31240_c", "scs30920_c",
                                                "scs30930_c", "scs30940_c", "scs3021s_c", "scs3022s_c", "scs36020_c",
                                                "scs3643s_c", "scs3642s_c", "scs3031s_c", "scs3033s_c", "scs3112s_c",
                                                "scs3131s_c", "scs3132s_c", "scs3133s_c", "scs3012s_c", "scs30130_c",
                                                "scs3061s_c", "scs30630_c", "scs30640_c", "scs30810_c")),
                               BA = list(w7 = c("bas7mar1_c", "bas7mar2_c", "bas7mar3_c", "bas7mar4_c", "bas7mar5_c",
                                                "bas7mar6_c", "bas7org1_c", "bas7org2_c", "bas7org3_c", "bas7org4_c",
                                                "bas7org5_c", "bas7org6_c", "bas7fin1_c", "bas7fin2_c", "bas7fin3_c",
                                                "bas7fin4_c", "bas7fin5_c", "bas7fin6_c", "bas7acc1_c", "bas7acc2_c",
                                                "bas7acc3_c", "bas7acc4_c", "bas7acc5_c", "bas7acc6_c", "bas7mic1_c",
                                                "bas7mic2_c", "bas7mic3_c", "bas7mic4_c", "bas7mic5_c", #"bas7mic6_c",
                                                #"bas7mac1_c",
                                                "bas7mac2_c", "bas7mac3_c", "bas7mac4_c", "bas7mac5_c",
                                                "bas7mac6_c")),
                               EF = list(w12 = c())),
                    SC6 = list(RE = list(w3 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c')
                                         , w5 = c('rea30110_c', 'rea3012s_c', 'rea30130_c', 'rea30140_c', 'rea3015s_c', 'rea30210_c'
                                                  , 'rea30220_c', 'rea30230_c', 'rea30240_c', 'rea30250_c', 'rea3028s_c', 'rea30310_c'
                                                  , 'rea30320_c', 'rea30330_c', 'rea30340_c', 'rea30350_c', 'rea30360_c', 'rea30370_c'
                                                  , 'rea3038s_c', 'rea30410_c', 'rea3042s_c', 'rea30430_c', 'rea30440_c', 'rea30450_c'
                                                  , 'rea30460_c', 'rea30510_c', 'rea3052s_c', 'rea30530_c', 'rea3054s_c', 'rea30550_c')
                                         , w9 = c())
                               , MA = list(w3 = c('maa3q071_c', 'mag9v131_sc6a3_c', 'mag9r261_sc6a3_c', 'mag9r111_sc6a3_c'
                                                  , 'maa3d131_c', 'maa3d132_c', 'mag9r051_sc6a3_c', 'maa3d041_c', 'maa3r081_c'
                                                  , 'maa3v082_c', 'mag9d201_sc6a3_c', 'maa3r091_c', 'mag9v121_sc6a3_c'
                                                  , 'maa3r121_c', 'maa3d112_c', 'maa3r011_c', 'maa3q101_c', 'mag5v321_sc6a3_c'
                                                  , 'mag9q021_sc6a3_c', 'maa3v061_c', 'maa3q021_c'),
                                           w9 = c())
                               , SC = list(w5 = c('sca56120_c', 'sca56130_c', 'sca51110_c', 'sca51140_c', 'sca50410_c', 'sca5652s_c'
                                                  , 'sca56540_c', 'sca51430_c', 'sca51440_c', 'sca50210_c', 'sca50220_c', 'sca50710_c'
                                                  , 'sca50720_c', 'sca56310_c', 'sca56320_c', 'sca5091s_c', 'sca56020_c', 'sca56030_c'
                                                  , 'sca50520_c', 'sca50530_c', 'sca51020_c','sca51030_c'))
                               , IC = list(w5 = c('ica5001x_c','ica5003x_c','ica5005x_c','ica5004s_c','ica5006x_c','ica5007x_c','ica5008x_c'
                                                  ,'ica5010x_c','ica5017s_c','ica5018s_c','ica5015s_c','ica5019x_c','ica5016s_c','ica5020s_c'
                                                  ,'ica5023x_c','ica5027x_c','ica5026x_c','ica5029x_c','ica5028x_c','ica5030x_c'
                                                  ,'icg9119x_sc6a5_c','ica5050s_c','icg9122x_sc6a5_c','ica5047s_c','ica5046x_c','ica5021s_c'
                                                  ,'ica5052s_c','ica5054x_c','ica5057x_c'))))


setwd('../')
devtools::use_data(item_labels, eaps, item_diff_SC6_RE_w3, meanvar, pkg = 'NEPStools', internal = TRUE, overwrite = TRUE)
