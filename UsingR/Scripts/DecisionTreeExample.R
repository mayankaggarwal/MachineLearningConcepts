setwd('D:\\Learning\\R\\WorkingDirectory')
credit <- read.csv("credit.csv")
credit$default <- factor(credit$default,levels = c(1,2),labels=c("no","yes"))
prop.table(table(credit$default))
#Data Preparation - creating random training and test datasets
set.seed(12345)
credit_rand <- credit[order(runif(1000)),] #runif creating list of 1000 random numbers 

credit_train <- credit_rand[1:700,]
credit_test <- credit_rand[701:1000,]

#Decision tree is available in C50 package
library(C50)
credit_model <- C5.0(credit_train[-21],credit_train$default)

summary(credit_model)

#Evaluating model performance
credit_pred <- predict(credit_model,credit_test)

library(gmodels)
CrossTable(credit_test$default,credit_pred,prop.chisq = FALSE, prop.c=FALSE,prop.r=FALSE,dnn=c('actual default','predicted default'))

#Adaptive boosting
credit_model10 <- C5.0(credit_train[-21],credit_train$default,trials=10)
credit_pred10 <- predict(credit_model10,credit_test)
CrossTable(credit_test$default,credit_pred10,prop.chisq = FALSE, prop.c=FALSE,prop.r=FALSE,dnn=c('actual default','predicted default'))
