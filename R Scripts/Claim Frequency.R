library(caret)
library(MASS)

#Check for missing values/ incomplete cases
clean_freMTPL2freq <- na.omit(freMTPL2freq)
clean_freMTPL2sev <- na.omit(freMTPL2sev) 

#Compare mean and variance of frequency to choose appropriate GLM
var(freMTPL2freq$ClaimNb)
mean(freMTPL2freq$ClaimNb)

#Poisson
glm.fit <- glm(ClaimNb ~ Area + VehPower + VehAge + DrivAge + BonusMalus
               + VehBrand + VehGas + Region, 
               family = poisson(link = "log"), data = freMTPL2freq, 
               offset = log(Exposure))
summary(glm.fit)

#Negative Binomial
glm.fit2 <- glm.nb(ClaimNb ~ . - Exposure - IDpol,
                   data = freMTPL2freq)
summary(glm.fit2)

#Assess model fit
random_sample <- createDataPartition(freMTPL2freq$ClaimNb, 
                                     p = 0.8,list = FALSE)
training_set <- freMTPL2freq[random_sample, ]
test_set <- freMTPL2freq[-random_sample, ]

#Train model on training set
glm.fit <- glm(ClaimNb ~ . - Exposure - Density - IDpol, 
                family = poisson(link = "log"), data = training_set, 
                offset = log(Exposure))
summary(glm.fit)

#Test model on test set
predictions <- predict(glm.fit2, test_set)
head(predictions)

#Model performance metrics
CV <- data.frame(MAE = MAE(predictions, test_set$ClaimNb)
)

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
