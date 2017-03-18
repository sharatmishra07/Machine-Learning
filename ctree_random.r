install.packages("party")
library(party)


input.dat <- readingSkills[c(1:150),]

png(file = "decision_tree.png")

output.tree <- ctree(nativeSpeaker~age + shoeSize + score,
                     data = input.dat,controls = ctree_control(maxdepth = 5))

dev.off()
plot(output.tree)

head(readingSkills)

library(datasets)

datafile <- read.csv("datafile.csv",header = T)

output.tree.irrig <- ctree(datafile$Total...Irrigation.potential.created~datafile$Irrigation.potential.created...Rabi+datafile$Irrigation.potential.created...Kharif,data = datafile,controls = ctree_control(maxdepth = 4))
dev.off()
plot(output.tree.irrig)


binarydata <- read.csv("binary.csv",header = T)

output.tree.binary <- ctree(admit~gre +gpa + rank,data = binarydata,controls = ctree_control(maxdepth = 4))
plot(output.tree.binary)


install.packages("randomForest")
library(randomForest)

output.forest <- randomForest(Total...Irrigation.potential.created~Irrigation.potential.created...Perennial +
                                Irrigation.potential.created...Kharif + Irrigation.potential.created...Rabi,
                                data = datafile, ntree = 700)

print(output.forest)
                              
                              
        
output.tree.binary <- randomForest(admit~gre + gpa + rank, data = binarydata, ntree = 700)
print(output.tree.binary)

cat("\014")
########

library(rpart)
library(caret)
library(randomForest)

splitdata <- createDataPartition(y=binarydata$admit,p=0.8, list = F,times = 1)

train_data <- binarydata[split,]
#x_train <- train_data[, -binarydata$admit]
#y_train <- train_data[,binarydata$admit]


test_data <- binarydata[-split,]
#x_test <- test[,-binarydata$admit]
#y_test <- test[,binarydata$admit]

#fit <- ctree(admit~.,data = train)
sapply(train_data, sd)

attach(train_data)
#two way contigency table

xtabs(~admit + rank, data = train_data)

train_data$rank <- factor(train_data$rank)
levels(test_data) <- levels(train_data)
fit <- rpart(admit~gre + gpa + rank, method = "class", data = train_data)

printcp(fit)
plotcp(fit)
summary(fit)

#plot the tree
plot(fit,uniform = T,main = "classification tree for marks")
text(fit,use.n = T,all = T,cex = 0.8)

#create attractive postscript plot of tree

#post(fit,file = "c:/tree.ps", title = "classification tree for marks ")

test_data$rank <- factor(test_data$rank)
test_data$tt <- predict(fit,test_data,type = "class")
testtable <- table(test_data$tt,test_data$admit)
accuracy <- sum(diag(testtable))/sum(testtable)
accuracy

##random forest
output.tree.binary.random <- randomForest(admit~gre + gpa + rank, data = train_data, ntree = 600)
print(output.tree.binary.random)
test_data$rank <- factor(test_data$rank)
test_data$tt <- predict(output.tree.binary.random,test_data,type = "class")
test_data$tt
testtable <- table(round(test_data$tt),test_data$admit)
accuracy <- sum(diag(testtable))/sum(testtable)
accuracy

predictRandom$tt <- predict(output.tree.binary.random,test_data,type = "response")

table(predictRandom$tt,test_data$admit)

##ctree

output.tree.binary.ctree <- ctree(admit~gre +gpa + rank,data = train_data,controls = ctree_control(maxdepth = 4))
test_data$rank <- factor(test_data$rank)
test_data$tt <- predict(output.tree.binary.ctree,test_data,type = "response")
test_data$tt
testtable <- table(round(test_data$tt),test_data$admit)
accuracy <- sum(diag(testtable))/sum(testtable)
accuracy
predictctree <- predict(output.tree.binary.ctree,test_data, type = "response")

plot(output.tree.binary.ctree)
output.tree.binary.ctree
accuracy_table <- table(predictctree,test_data$admit)
accuracy_ctree <- sum(diag(accuracy_table))/sum(accuracy_table)
accuracy_ctree
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          