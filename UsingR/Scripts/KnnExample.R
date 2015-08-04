setwd("D:/Learning/R/WorkingDirectory")
wbcd <- read.csv("wisc_bc_data.csv",stringsAsFactors = FALSE)
str(wbcd)
head(wbcd)
summary(wbcd)

#as most of the classifiers require target feature to be coded as factor
wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))

round(prop.table(table(wbcd$diagnosis))*100,digits = 1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#Normalizing function for normaizing feature values
normalize <- function(x){
  return((x - min(x))/(max(x)- min(x)))
}

wbcd <- wbcd[-1]
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))

wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

#contains KNN implementation
library(class)

wbcd_test_pred<- knn(wbcd_train,wbcd_test,cl=wbcd_train_labels,k=21)

library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE)

#improving model performance
wbcd_z <- as.data.frame(scale(wbcd[-1]))
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
wbcd_test_pred<- knn(wbcd_train,wbcd_test,cl=wbcd_train_labels,k=3)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE)
