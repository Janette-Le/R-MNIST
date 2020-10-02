install.packages("e1071")
library(e1071)

install.packages("ggplot2")
library(ggplot2)

install.packages("lattice")
library(lattice)

install.packages("caret")
library(caret)

install.packages("dplyr")
library(dplyr)

install.packages("gbm")
library(gbm)


### Importing datasets
train <- read.csv("train.csv")
train <- train[,-1]
train$target <- as.factor(train$target)
str(train)

set.seed(12345)
row.number <- sample(x =1:nrow(train), size = 0.75 *nrow(train))

small_train <- train[row.number,]
small_test<- train[-row.number,]
actual <- data.frame(small_test[,1])
small_test <- small_test[,-1]

test <- read.csv("test.csv")
test <- test[, -1]

barplot(prop.table(table(train$target))*100, ylim  = c(0,100), main = "Count of 0 and 1 value in target colum - Train dataset")
########################################## Naive Bayes ############################################################

### Naive Bayes Fitting model
model <- naiveBayes(target ~., data = small_train)
summary(model)

### Accuracy
small_predicted <- predict(model, small_test)
confusionMatrix(small_predicted, actual$small_test...1.)

### Predict
naive_prediction <- predict(model, test)
View(naive_prediction)

table(naive_prediction)
prop.table(table(naive_prediction))*100

################################# Gradient Boosting: 150, 5, 0.01  ############################################################

### Gradient boosting fitting model: 150, 5, 0.01
gbm_model <- gbm(target ~., distribution ="multinomial",
            data = small_train, n.trees  = 150,
            interaction.depth = 5, shrinkage = 0.01)

pred <- predict.gbm(object = gbm_model, 
                    n.trees = 150,
                    newdata = small_test,
                    type = "response")

gbm_test <- as.data.frame(colnames(pred)[apply(pred, 1, which.max)])

confusionMatrix(gbm_test$`colnames(pred)[apply(pred, 1, which.max)]`, actual$small_test...1.)

### Gradient boosting fitting model: 150, 7, 0.1
gbm_model <- gbm(target ~., distribution ="multinomial",
                 data = small_train, n.trees  = 150,
                 interaction.depth = 7, shrinkage = 0.1)

pred <- predict.gbm(object = gbm_model, 
                    n.trees = 150,
                    newdata = small_test,
                    type = "response")

gbm_test <- as.data.frame(colnames(pred)[apply(pred, 1, which.max)])

confusionMatrix(gbm_test$`colnames(pred)[apply(pred, 1, which.max)]`, actual$small_test...1.)


### Gradient boosting fitting model: 150, 7, 0.3
gbm_model <- gbm(target ~., distribution ="multinomial",
                 data = small_train, n.trees  = 150,
                 interaction.depth = 7 , shrinkage = 0.3)

pred <- predict.gbm(object = gbm_model, 
                    n.trees = 150,
                    newdata = small_test,
                    type = "response")

gbm_test <- as.data.frame(colnames(pred)[apply(pred, 1, which.max)])

confusionMatrix(gbm_test$`colnames(pred)[apply(pred, 1, which.max)]`, actual$small_test...1.)

### Prediction
pred1 <- predict.gbm(object = gbm_model, 
                    n.trees = 150,
                    newdata = test,
                    type = "response")

gbm_prediction <- as.data.frame(colnames(pred1)[apply(pred1, 1, which.max)])
table(gbm_prediction)
prop.table(table(gbm_prediction)) * 100

confusionMatrix(gbm_prediction$`colnames(pred1)[apply(pred1, 1, which.max)]`, naive_prediction)
