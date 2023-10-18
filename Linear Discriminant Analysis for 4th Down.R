require(car)
require(tidyverse)
require(dplyr)
require(ggplot2)
require(patchwork)
library(MASS) ##for lda function
library(klaR) ##for partimat function to produce 2-D partition plots
library(ICS) ##for multivariate normality tests

df <- read.csv("/Users/aidandowd/Desktop/stat4630_q2_data.csv")
set.seed(2021)
df <- df[,-1]
df <- df[,-8]
#LDA
##create training and test data
sample.data<-sample.int(nrow(df), floor(.50*nrow(df)), replace = F)
train<-df[sample.data, ]
test<-df[-sample.data, ]

#Testing for normality

pass<-train[which(train$play_type=="pass"),]
run<-train[which(train$play_type=="run"),]

##MVN tests for yes chd
ICS::mvnorm.kur.test(pass[,2:7])
ICS::mvnorm.skew.test(pass[,2:7])

##MVN tests for no chd
ICS::mvnorm.kur.test(run[,2:7])
ICS::mvnorm.skew.test(run[,2:7])

# This assumption is not met, as we reject the null and conclude that there is evidence against
# this assumption

#LDA
lda.df <- MASS::lda(play_type ~ ., data=train)
lda.df


lda.test <- predict(lda.df,test)
table(train$play_type, lda.test$class)
lda_acc <- mean(test$play_type == lda.test$class)

table(test$play_type, lda.test$posterior[,2]>0.5)
mean(lda.test$class==test$play_type)

#ROC and AUC
library(ROCR)
preds<-lda.test$posterior[,2]
rates<-ROCR::prediction(preds, test$play_type)
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Play Type on 4th Down")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-ROCR::performance(rates, measure = "auc")
number6 <- auc@y.values

# cross validation k = 5, k = 10


##this returns the k fold CV estimate for the test classification error rate
require(ipred)
library(boot) 
library(MASS) 
library(ipred)
require(carat)

set.seed(2021)

cv.da <- function(object, newdata) 
  
{
  
  return(predict(object, newdata = newdata)$class)
  
} 
df$play_type = ifelse(df$play_type == "pass", 1, 0)
df$play_type = as.factor(df$play_type)
k5 <- ipred::errorest(play_type ~ ., data=df, model=lda, estimator="cv", est.para=control.errorest(k=5), predict=cv.da) 
k10 <- ipred::errorest(play_type ~ ., data=df, model=lda, estimator="cv", est.para=control.errorest(k=10), predict=cv.da)

## LDA Actual Test Error Rate

lda.train <- MASS::lda(play_type ~ ., data=train)

lda.test <- predict(lda.train,test)

accuracy <- mean(test$play_type == lda.test$class)
lda_test_error <- 1- accuracy
lda_test_error

fd <- mtcars

bp <- ggplot(fd, aes(x = vs, y = qsec, fill = cyl)) + geom_col()
bp + coord_polar("y")



