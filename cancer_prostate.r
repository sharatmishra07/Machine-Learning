cancer <- read.csv("Prostate_Cancer.csv",header = T)
cancerData <- cancer[,-1]
head(cancerData)
library(randomForest)
library(caret)

data_part <- createDataPartition(y=cancerData$diagnosis_result,p=0.8, list = F,times = 1)

train_data <- cancerData[data_part,]

test_data <- cancerData[-data_part,]
cancer.random <- randomForest(diagnosis_result~.,data = train_data,ntree = 700)
cancer.random
predicted_data <- predict(cancer.random,test_data)
tt <- table(predicted_data,test_data$diagnosis_result)
accuracy <- sum(diag(tt))/sum(tt)
accuracy
varImpPlot(cancer.random,type = 2,sort = T)


Prostate_Cancer <-read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE,header = T)
str(Prostate_Cancer)
Prostate_Cancer <-Prostate_Cancer[-1]
table(Prostate_Cancer$diagnosis_result)
Prostate_Cancer$diagnosis <- factor(Prostate_Cancer$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(Prostate_Cancer$diagnosis)) * 100, digits = 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
Prostate_Cancer_n <- as.data.frame(lapply(Prostate_Cancer[2:9], normalize))
summary(Prostate_Cancer_n$radius)
Prostate_train <-Prostate_Cancer_n[1:65,]
Prostate_test <- Prostate_Cancer_n[66:100,]
Prostate_train_labels <- Prostate_Cancer[1:65, 1]
Prostate_test_labels <- Prostate_Cancer[66:100, 1]
library(gmodels)

library(class)
Prostate_test_pred <- knn(train = Prostate_train, test = Prostate_test,cl = Prostate_train_labels, k=10)
CrossTable(x=Prostate_test_labels,y=Prostate_test_pred,prop.chisq=FALSE)
