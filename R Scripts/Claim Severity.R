library(caret)

#Link severity table to frequency table (left join)
full_table <- merge(freMTPL2sev, freMTPL2freq, by = "IDpol", all.x = TRUE)
head(full_table)

#Observe claim severity to select suitable GLM
plot(full_table$ClaimAmount)

#Remove abnormally large claim amounts to assess general shape
Q1 <- quantile(full_table$ClaimAmount, 0.25)
Q3 <- quantile(full_table$ClaimAmount, 0.75)
IQR <- IQR(full_table$ClaimAmount)
clean_full_table <- subset(full_table, 
                           ClaimAmount < (Q3 + 1.5 * IQR))
boxplot(clean_full_table$ClaimAmount, main = "Claim Severity")

#Fit gamma GLM
glm.fit <- glm(ClaimAmount ~ Exposure + Area + VehPower + VehAge + DrivAge
               + BonusMalus + VehBrand + VehGas + Region, 
               family = Gamma(link = "log"), data = full_table,
               )
summary(glm.fit)
               
#Prediction
result <- predict(glm.fit, 
                  data.frame(Exposure = 1, Area = "A", VehPower = 4, VehAge = 3, DrivAge = 18,
                             BonusMalus = 50, VehBrand = "B10",
                             VehGas = "Regular",Region = "R21"),
                  type = "response")
print(result)

result2 <- predict(glm.fit, 
                   data.frame(Exposure = 1, Area = "A", VehPower = 4, VehAge = 3, DrivAge = 40,
                              BonusMalus = 50, VehBrand = "B10",
                              VehGas = "Regular",Region = "R21"),
                   type = "response")
print(result2)

result3 <- predict(glm.fit, 
                   data.frame(Exposure = 1, Area = "A", VehPower = 4, VehAge = 3, DrivAge = 85,
                              BonusMalus = 50, VehBrand = "B10",
                              VehGas = "Regular",Region = "R21"),
                   type = "response")
print(result3)
