setwd("D:\\Learning\\R\\WorkingDirectory")
sms_raw <- read.csv("sms_spam.csv",stringsAsFactors=FALSE)

str(sms_raw)
sms_raw$type <- factor(sms_raw$type)

#Data preparation
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

#to look contents of corpus function we need to use inspect function
inspect(sms_corpus[1:3])

corpus_clean <- tm_map(sms_corpus,tolower)
corpus_clean <- tm_map(corpus_clean,removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)

corpus_clean <- Corpus(VectorSource(corpus_clean))
sms_dtm <- DocumentTermMatrix(corpus_clean)
inspect(sms_dtm[1,])

#Data Preparation
nrow(sms_raw)
sms_raw_train <- sms_raw[1:4175,]
sms_raw_test <- sms_raw[4176:5574,]

sms_dtm_train <- sms_dtm[1:4175,]
sms_dtm_test <- sms_dtm[4176:5574,]

sms_corpus_train <- corpus_clean[1:4175]
sms_corpus_test <- corpus_clean[4176:5574]

#Visualizing data using wordcloud
library(wordcloud)
wordcloud(sms_corpus_train,min.freq=40,random.order=FALSE)

spam <- subset(sms_raw_train,type=="spam")
ham <- subset(sms_raw_train,type=="ham")

wordcloud(spam$text,max.words=40,scale=c(3,0.5))
wordcloud(ham$text,max.words=40,scale=c(3,0.5))

#Creating indicator features for frequent words
sms_dict <- findFreqTerms(sms_dtm_train,5)

sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))

#Change occurance of words in sparce matrix to a factor variable that simple indicates yes or no
#as Naive Bayes classifier is trained with categorical data

convert_counts <- function(x){
  x<- ifelse(x>0,1,0)
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
  return(x)
}

sms_train <- apply(sms_train,MARGIN=2,convert_counts)
sms_test <- apply(sms_test,MARGIN=2,convert_counts)


#training a model on data
library(e1071)
sms_classifier <- naiveBayes(sms_train,sms_raw_train$type)
sms_test_pred <- predict(sms_classifier,sms_test)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq=FALSE,prop.t=FALSE,dnn=c("predicted","actual"))

#Improving model preformance
sms_classifier2 <- naiveBayes(sms_train,sms_raw_train$type,laplace=1)
sms_test_pred2 <- predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq=FALSE,prop.t=FALSE,dnn=c("predicted","actual"))
