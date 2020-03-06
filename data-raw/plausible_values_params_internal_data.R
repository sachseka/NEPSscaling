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
    SC3 = list(
      ST = list(
        w9 = c(
          "stg12nh01_sc3g12_c", "stg12nh02_sc3g12_c", "stg12nh03_sc3g12_c",
          "stg12nh04_sc3g12_c", "stg12nh05_sc3g12_c", "stg12eg01_sc3g12_c",
          "stg12eg02_sc3g12_c", "stg12eg03_sc3g12_c", "stg12eg04_sc3g12_c",
          "stg12eg05_sc3g12_c", "stg12eg06_sc3g12_c", "stg12eg07_sc3g12_c",
          "stg12mt01_sc3g12_c", "stg12mt02_sc3g12_c", "stg12mt03_sc3g12_c",
          "stg12mt04_sc3g12_c", "stg12mt05_sc3g12_c", "stg12cmt06_sc3g12_c",
          "stg12cw01_sc3g12_c", "stg12cw02_sc3g12_c", "stg12cw03_sc3g12_c",
          "stg12cw04_sc3g12_c", "stg12cw05_sc3g12_c", "stg12cw06_sc3g12_c",
          "stg12cw07_sc3g12_c", "stg12pd01_sc3g12_c", "stg12pd02_sc3g12_c",
          "stg12cpd03_sc3g12_c", "stg12pd04_sc3g12_c", "stg12pd05_sc3g12_c",
          "stg12pd06_sc3g12_c", "stg12pd07_sc3g12_c"
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
          "mag12d071_c", "mag12r041_c", "mag12v131_c", "mag12v132_c",
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
          "icg12018s_c", "ica5003x_sc4g12_c", "icg12107s_c", "icg12004s_c",
          "icg12010x_c", "icg12011x_c", "ica5008x_sc4g12_c", "icg12060s_c",
          "icg12013s_c", "icg12016s_c", "ica5019x_sc4g12_c",
          "icg12121x_c", "icg12028s_c", "ica5023x_sc4g12_c", "ica5027x_sc4g12_c",
          "icg12033x_c", "icg12034x_c", "icg12035x_c", "icg12040x_c",
          "icg12037s_c", "icg12138s_c", "icg12047s_c", "icg12041x_c",
          "icg12046s_c", "ica5021s_sc4g12_c", "ica5052s_sc4g12_c", "icg12048s_c",
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

# item difficulties for competence tests
load(file = "data-raw/xsi_fixed.RData")

# corrections
load(file = "data-raw/correction.RData")

# difference matrix for SC4 English w3, w7
load(file = "data-raw/diffMat.RData")

usethis::use_data(correction, item_labels, link_constant, xsi.fixed, diffMat,
  internal = TRUE, overwrite = TRUE
)
