#Other Findings
val1 <- mean(freMTPL2freq$ClaimNb[freMTPL2freq$DrivAge <= 25], na.rm = TRUE)
val2 <- mean(freMTPL2freq$ClaimNb[freMTPL2freq$DrivAge > 25 & freMTPL2freq$DrivAge <= 85],
             na.rm = TRUE)
print(val1 / val2)

val3 <- mean(freMTPL2freq$ClaimNb[freMTPL2freq$DrivAge > 25 & freMTPL2freq$DrivAge <= 85],
              na.rm = TRUE)
val4 <- mean(freMTPL2freq$ClaimNb[freMTPL2freq$DrivAge >= 85], na.rm = TRUE)
print(val4 / val3)
