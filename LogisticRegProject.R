setwd("~/Downloads/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects")
adult <- read.csv('adult_sal.csv')
head(adult)
str(adult)
summary(adult)

#Data cleaning
library('dplyr')
#getting rid of double index column
adult <- select(adult,-X)

#grouping type of employer column
table(adult$type_employer)

employer_func <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job == 'Without-pay'){
    return('Unemployed')
  }else if(job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-employed')
  }else if (job=='Local-gov'|job=='State-gov'){
    return('SL-gov')
  }else{
    return(job)
  }
}

adult$type_employer<- sapply(adult$type_employer,employer_func)

table(adult$marital)

martial_func <- function(martial){
  martial<-as.character(martial)
  if (martial=='Separated' | martial=='Divorced' | martial=='Widowed'){
    return('Not-Married')
  }else if(martial=='Never-married'){
    return(martial)
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,martial_func)

levels(adult$country)

#library(countrycode)
#region_func<- function(reg){
#  reg <- as.character(reg)
#  for (i in length(reg)){
#    adult$area[i]<-countrycode(reg[i],'country.name','region')
#    }
#return(adult$area)
#  }

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America <- c('Canada','United-States','Puerto-Rico' )
Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')
Other <- c('South')

country_func <- function(country){
  if (country %in% Asia){
    return('Asia')
  }else if (country %in% North.America){
    return('North.America')
  }else if (country %in% Europe){
    return('Europe')
  }else if (country %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,country_func)
names(adult)[names(adult)=='country'] <- 'region'

adult$type_employer <- sapply(adult$type_employer,factor)
adult$region <- sapply(adult$region,factor)
adult$marital <- sapply(adult$marital,factor)

library(Amelia)
adult[adult=='?'] <- NA
missmap(adult)
adult <- na.omit(adult)

#EDA
library(ggplot2)
library(dplyr)
ggplot(adult,aes(age))+geom_histogram(aes(fill=income),color='black')
ggplot(adult,aes(hr_per_week))+geom_histogram()
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Logistic Regression
library(caTools)
set.seed(101)
sample <- sample.split(adult$income,SplitRatio = 0.7)
train <- subset(adult,sample==T)
test <- subset(adult,sample==F)

model <- glm(income ~ ., family = binomial(logit),data=train)
summary(model)

new.model <- step(model)

test$predicted.income = predict(model,newdata = test, type='response')
a<-table(test$income,test$predicted.income >0.5)
accuracy <- (a[1]+a[4]) / sum(a)
