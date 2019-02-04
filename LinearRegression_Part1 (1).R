library(MASS)
data(Boston)
?Boston
Boston
summary(Boston$medv)
hist(Boston$medv)

##log or sqrt transformation to convert target variable to normal distribution
hist(log(Boston$medv))
hist(sqrt(Boston$medv))
summary(log(Boston$medv))
summary(sqrt(Boston$medv))

Boston$medv_sqrt = sqrt(Boston$medv)
Boston$medv_log = log(Boston$medv)

### 
rows = 1:nrow(Boston)
trainRows = sample(rows,round(0.7*nrow(Boston)))
testRows = rows[-trainRows]

trainData = Boston[trainRows,]
testData = Boston[testRows,]

######Simple Linear Regression - Using only Lstat
plot(trainData$lstat,trainData$medv_log)
plot(trainData$lstat,trainData$medv)
cor(trainData$lstat,trainData$medv)

mod1 = lm(medv~lstat,data=trainData)
mod2 = lm(medv_log~lstat,data=trainData)
summary(mod1)
summary(mod2)
plot(Boston$lstat,Boston$medv)
abline(mod1)

plot(Boston$lstat,Boston$medv_log)
abline(mod2)

preds = predict(mod2,testData)
length(preds)

### calculating RMSE
testData$preds = 2.718**preds
sqrt(mean((testData$preds - testData$medv)**2))

###
summary(Boston$medv_log)

############## Multiple Linear Regression ###############
#### Checking Correlation
library(corrplot)
corrplot(cor(Boston[,1:14]),method = 'number')

#### Buidling the model. Ignoring hte actual medv and medv_sqrt features as they are replicas of target
mod1 = lm(medv_log~ .-medv-medv_sqrt,data=trainData)
summary(mod1)

###### Dropping other features as the model returns low significance (No Stars or P-values > 0.05) agains these features
mod1 = lm(medv_log~ .-medv-medv_sqrt-zn-indus-age-chas,data=trainData)
summary(mod1)

preds = predict(mod1,testData)
length(preds)

### RMSE
testData$preds = 2.718**preds

sqrt(mean((testData$preds - testData$medv)**2))
### The above RMSE is clearly lower than the simple linear regression
