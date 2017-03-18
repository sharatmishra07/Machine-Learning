
install.packages("class")

library(class)

pcrdata <- read.csv("binary.csv",header = T)
pcr <- pcrdata[-1.-1]

head(pcrdata)
pcrdata$admit <- factor(pcrdata$admit)
num.vars <- sapply(pcrdata,is.numeric)
pcrdata[num.vars] <- lapply(pcrdata[num.vars], scale)
nrow(pcrdata)
set.seed(100)

pcr_train <- pcrdata[1:240,]
pcr_test <- pcrdata[241:400,]

pcr_train_labels <- pcrdata[1:240,1]
pcr_test_labels <- pcrdata[241:400,1]

knn.1 <- knn(pcr_train,pcr_test,cl = pcr_train_labels,k=1)
knn.10 <- knn(pcr_train,pcr_test,cl = pcr_train_labels,k=10)
knn.20 <- knn(pcr_train,pcr_test,cl = pcr_train_labels,k=20)
knn.1

library(gmmodels)
install.packages("gmmodel")
CrossTable(x=pcr_test_labels,y=knn10,prop.chisq = F)


normalize <- function(x){
  return(x-min(x))/(max(x)-min(x)))
}

data <- pcrdata

mydata <- as.data.frame(lapply(data[2:3],normalize))
mydataTrain_labels <- data[1:nrow(mydataTrain),1]
mydataTest_labels <- data[1:nrow(mydataTest),1]

myNewData <- cbind(data$admit,mydata,data$rank)
#############################################

bin<- read.csv("binary.csv",header = T)
View(bin)
table(bin$admit)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
bin_n <- as.data.frame(lapply(bin[2:3], normalize))
summary(bin_n$gre)
summary(bin_n$gpa)
bin_train <- bin_n[1:260,]
bin_test <- bin_n[261:400,]
bin_train_labels <- bin[1:260, 1]
bin_test_labels <- bin[261:400, 1]
bin_test_pred <- knn(train = bin_train, test = bin_test,cl = bin_train_labels, k=4)
#install.packages("gmodels")
library(gmodels)
CrossTable(x=bin_test_labels,y=bin_test_pred,prop.chisq = FALSE)



bin_test_pred <- knn(train = bin_train, test = bin_test,cl = bin_train_labels, k=10)









