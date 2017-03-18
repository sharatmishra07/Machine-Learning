library(caret)
head(iris)
names(iris)
x= iris[,-5]
y=iris[,5]


#MOdel creation using NaiveBayes with cross validation
model= train(x,y,'nb',trControl = trainControl(method = 'cv',number = 10))

#show the model
model

#Use predict function for getting prediction value and result class
predict(model$finalModel,x)
mel$metric
model$results
model$pred
model$bestTune
model$call
model$dots


table(predict(model$finalModel,x)$class,y)
naive_iris <- NaiveBayes(Species~.,data=iris)
plot(naive_iris)


library(e1071)
head(BreastCancer)
load("BreastCancer.rda")
unzip("BreastCancer.rda")


install.packages("mlbench")
library(mlbench)
data("BreastCancer")

head(BreastCancer)
dim(BreastCancer)
breastCancer <- na.omit(BreastCancer)
is.na(breastCancer)
breastCancer <- breastCancer[,-1]
head(breastCancer)
a = breastCancer[,-10]
head(breastCancer[,10])
b = breastCancer[,10]
modelBreastCancer = train(a,b,'nb',trControl = trainControl(method = 'cv',number = 10))

modelBreastCancer

predict(modelBreastCancer$finalModel,a)

table(predict(modelBreastCancer$finalModel,a)$class,b)

modelBreastCancer$results


md1 <- NaiveBayes(breastCancer$Class~.,data = breastCancer)
md1

predictCancer <- predict(md1,a)

table(b,predictCancer$class)

