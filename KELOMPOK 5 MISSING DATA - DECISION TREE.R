library(rpart)
library(rpart.plot)
library(party)
dataku <- read.delim("clipboard")
dataku$city <- as.factor(dataku$city)
summary(dataku)
str(dataku)
ind <- sample (2, nrow(dataku), replace = TRUE, prob = c(0.7, 0.3) )
trainData <- dataku[ind==1, ]
testData <- dataku[ind==2, ]
myFormula <- city ~ sqft_lot+ sqft_above+sqft_living+price
dataku_ctree <- ctree (myFormula, data = trainData)
table(predict(dataku_ctree), trainData$sqft_living)
print(dataku_ctree)
plot(dataku_ctree)
prediksi <- predict(dataku_ctree, trainData)
cm <- table(trainData[, 5], prediksi)
cm
accuracy <- (sum(diag(cm)))/sum(cm)
accuracy


