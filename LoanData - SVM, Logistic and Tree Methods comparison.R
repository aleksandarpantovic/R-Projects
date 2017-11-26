loans <- read.csv('loan_data.csv')

str(loans)
summary(loans)

#adding factors to the labels
for (i in c('credit.policy','inq.last.6mths','delinq.2yrs','pub.rec','not.fully.paid')){
  loans[,i]<-factor(loans[,i])
}

#EDA
library(ggplot2)
library(GGally)
ggpairs(data=loans[,!names(loans) %in% c('inq.last.6mths')]) #takes around 30 sec

ggplot(loans,aes(fico))+geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
ggplot(loans,aes(factor(purpose)))+geom_bar(aes(fill=not.fully.paid))
ggplot(loans,aes(int.rate,fico)) +geom_point(aes(color=not.fully.paid),alpha=0.3)

#Splitting on a test and train samples
library(caTools)
set.seed(101)
spl = sample.split(loans$not.fully.paid,0.7)
train = subset(loans,spl==T)
test = subset(loans,spl==F)

library(e1071)
model <- svm(not.fully.paid ~ .,data=train)
summary(model)

predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
#model classified everything into same group

#Tuning the model
tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',ranges=list(cost=c(1,10), gamma=c(0.1,1)))
tune.results

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
a <- table(predicted.values,test$not.fully.paid)
accuracy.svm <- (a[1]+a[4]) / sum(a)  #83%



#Comparing with logistic regresssion
model.log <- glm(not.fully.paid ~ ., family = binomial(logit),data=train)
summary(model.log)

model.log <- glm(not.fully.paid~., data=train[,!colnames(train) %in% c("inq.last.6mths","delinq.2yrs","pub.rec")], family = binomial(logit))
summary(model.log)

model.log.step <- step(model.log)
summary(model.log.step)

#Predicting values with logistic regression model
test$predicted.step = predict(model.log.step,newdata = test,type='response')
test$predicted.log<-predict(model.log, newdata=test, type="response")


a<-table(test$not.fully.paid,test$predicted.log>0.5)
accuracy.log <- (a[1]+a[4]) / sum(a) #84%

#Comparing with a Random Forest Model
library(randomForest)
rf.model <- randomForest(not.fully.paid ~ ., data = train, importance = TRUE)

rf.model$confusion
rf.model$importance
pred <- predict(rf.model, test)
table(pred, test$not.fully.paid) #97%
