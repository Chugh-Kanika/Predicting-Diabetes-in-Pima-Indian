diabetes <- read.csv("diabetes.csv")
nrow(diabetes)
head(diabetes)
set.seed(1)


summary(diabetes)

##replace 0 with NA
diabetes$Glucose[diabetes$Glucose == 0] <- NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA
diabetes$Insulin[diabetes$Insulin == 0] <- NA
diabetes$BMI[diabetes$BMI== 0] <- NA

##knn imputation
library(VIM)

aggr(x=diabetes)

aggr(x = diabetes.knn)
diabetes.knn <- kNN(diabetes, variable = c("Glucose", "BloodPressure", "SkinThickness",
                                "Insulin", "BMI"), k = 6)
summary(diabetes.knn)
head(diabetes.knn)
diabetes.knn <- subset(diabetes.knn, select = Pregnancies:Outcome)

##split train valid and test

train.rows <- sample(1:nrow(diabetes.knn),0.7*nrow(diabetes.knn))
valid.rows <- sample(setdiff(1:nrow(diabetes.knn),train.rows),0.2*nrow(diabetes.knn))
test.rows <- setdiff(1:nrow(diabetes.knn),union(train.rows,valid.rows))

train.data  <- diabetes.knn[train.rows,]
valid.data <- diabetes.knn[valid.rows,]
test.data <- diabetes.knn[test.rows,]

names(train.data)

##logistics regression

library(caret)

model1 <- glm(Outcome ~ . , data = train.data, family = "binomial")
summary(model1)
##train accuracy

train.prob <- predict(model1, train.data,type = "response")


##ROC curve
library(ROCR)

ROCR = prediction(train.prob,train.data$Outcome)

perf = performance(ROCR, "tpr","fpr")

plot(perf,print.cutoffs.at = seq(0,1,0.1))

## based on ROC curve to keep an optimum balance between
##True positive rate and false positive rate selected threshold is 0.35 

train.class <- ifelse(train.prob>0.35,1,0)
confusionMatrix(as.factor(train.class),as.factor(train.data$Outcome), positive = "1")

##valid accuracy

valid.prob <- predict(model1,valid.data, type = "response")

valid.class <- ifelse(valid.prob>0.35, 1,0)

confusionMatrix(as.factor(valid.class),as.factor(valid.data$Outcome), positive = "1")


##test accuracy

test.prob <- predict(model1,test.data, type = "response")



test.class <- ifelse(test.prob>0.35, 1,0)

confusionMatrix(as.factor(test.class),as.factor(test.data$Outcome), positive = "1")

## step model for logistic regression

library(gains)

step.model <- step(model1,direction = "both")
summary(step.model)

##train accuracy

train.prob1 <- predict(step.model, train.data,type = "response")
train.class1 <- ifelse(train.prob1>0.35,1,0)
confusionMatrix(as.factor(train.class1),as.factor(train.data$Outcome), positive = "1")


##valid accuracy

valid.prob1 <- predict(step.model,valid.data, type = "response")

valid.class1 <- ifelse(valid.prob1>0.35, 1,0)

confusionMatrix(as.factor(valid.class1),as.factor(valid.data$Outcome), positive = "1")


##test accuracy

test.prob1 <- predict(step.model,test.data, type = "response")

test.class1 <- ifelse(test.prob1>0.35, 1,0)

confusionMatrix(as.factor(test.class1),as.factor(test.data$Outcome), positive = "1")

library(ipred)
##bagging 

train.data$Outcome <- as.factor(train.data$Outcome)
valid.data$Outcome <- as.factor(valid.data$Outcome)
test.data$Outcome <- as.factor(test.data$Outcome)

model1.bag <- bagging(Outcome ~ . , data = train.data, nbagg = 11)

##training accuracy

actual.bag <- train.data$Outcome
pred.bag <- predict(model1.bag, newdata = train.data)

pred.bag.prob <- predict(model1.bag,newdata = train.data, type = "prob")




confusionMatrix(pred.bag,actual.bag,positive = '1')

##valid accuracy

actual1.bag <- valid.data$Outcome
predict1.bag <- predict(model1.bag,newdata = valid.data)

confusionMatrix(predict1.bag,actual1.bag, positive = '1')

##test accuracy

actual2.bag <- test.data$Outcome
predict2.bag <- predict(model1.bag, newdata = test.data)

confusionMatrix(predict2.bag,actual2.bag, positive = '1')

##Random Forest

library(randomForest)


forest.model <- randomForest(Outcome ~ . , data = train.data)

## Accuracy on training data
actual.forest <- train.data$Outcome
predicted.forest <- predict(forest.model)

confusionMatrix(predicted.forest,actual.forest,positive = "1")



##valid accuracy

actual1.forest <- valid.data$Outcome
predict.forest <- predict(forest.model,newdata = valid.data)

confusionMatrix(predict.forest,actual1.forest, positive = '1')

##test accuracy

actual2.forest <- test.data$Outcome
predict2.forest<- predict(forest.model, newdata = test.data)

confusionMatrix(predict2.forest,actual2.forest, positive = '1')


##heat map

forheatmap <- diabetes.knn[,c(1:8)]

heatmap(cor(forheatmap))

##scatter plot

help(bagging)
help("randomForest")
library(ggplot2)


graph <- ggplot(diabetes.knn, aes( x = Pregnancies, y = Outcome)) + 
  geom_point(size = 2, shape = 23, fill = "blue", color = "darkred") +
    labs(y = "Outcome", x = "Number of Pregnancies" , color = "Blue", title = "Outcome vs number of Pregnancies")
graph

##Scatter plot of all variables

plot(diabetes.knn)


library(corrplot)

corrplot(cor(forheatmap), method = "circle")
help("corrplot")

library(summarytools)

dfSummary(diabetes)
  