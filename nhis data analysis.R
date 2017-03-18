imputation_plotNHIS <- aggr(NHIS,col = c("pink","blue"),
                        numbers = TRUE,sortVars = TRUE,
                        labels = names(NHIS),gap = 3,
                        ylab = c("Missing Data","Pattern"))


imputation_Data_NHIS <- mice(NHIS,m=4,maxit = 50,method = "pmm",seed = 500)

imput_data_NHIS <- complete(imputation_Data_NHIS,3)

imputation_plotNHIS2 <- aggr(imput_data_NHIS,col = c("pink","blue"),
                            numbers = TRUE,sortVars = TRUE,
                            labels = names(imput_data_NHIS),gap = 3,
                            ylab = c("Missing Data","Pattern"))

set.seed(100)
trainingNHISIndex <- sample(1:nrow(imput_data_NHIS),0.74*nrow(imput_data_NHIS))
trainingDataNHIS <- imput_data_NHIS[trainingNHISIndex,]
testDataNHIS <- imput_data_NHIS[-trainingNHISIndex,]
modelNHIS <- lm(as.numeric(imput_data_NHIS$weight)~as.numeric(imput_data_NHIS$height),data = trainingDataNHIS)

fitted(modelNHIS)
predictNHIS <- predict(modelNHIS,testDataNHIS)

summary(modelNHIS)

plot(imput_data_NHIS$weight~imput_data_NHIS$height,data = imput_data_NHIS)
abline(modelNHIS,col = "red")

layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
plot(modelNHIS)


