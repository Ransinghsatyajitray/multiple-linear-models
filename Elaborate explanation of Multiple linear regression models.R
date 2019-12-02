# Dealing With Missing Values
dataset$age = ifelse(is.na(dataset$age),ave(dataset$age, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$age)

dataset$salary = ifelse(is.na(dataset$salary), ave(dataset$salary, FUN = function(x) mean(x, na.rm = 'TRUE')), dataset$salary)


dataset$age = as.numeric(format(round(dataset$age, 0)))

# Since we are not interested in having decimal places for age we will round it up using the above code. The argument 0 in the round function means no decimal places.

# Dealing With Categorical Data
# Categorical variables represent types of data which may be divided into groups. Examples of categorical variables are race, sex, age group, educational level etc.

dataset$nation = factor(dataset$nation, levels = c('India','Germany','Russia'), labels = c(1,2,3))

dataset$purchased_item = factor(dataset$purchased_item, levels = c('No','Yes'),  labels = c(0,1))

# Scaling The Features
training_set[,3:4] = scale(training_set[,3:4])
test_set[,3:4] = scale(test_set[,3:4])




#Experimenting the purrr package

library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)
library(readxl)
library(data.table) #fread
library(ggplot2)
library(ggforce) #this is for paginating the wrap
library(sqldf)
library(rebus)
library(scales)
library(broom)
library(corrplot) 


data(mtcars)

mtcars_100plus<-map(mtcars,function(x)x+100)

cyl_1<-split(mtcars,mtcars$cyl)


map(cyl_1,function(x)lm(mpg~wt,data=x)) #this is untidy

a<-mtcars%>%nest(-cyl)  #nest come from tidyr package 
#Here we grouped the data to specific cyl, here 2 columns get created one is for levels(cyl) and 
#the other column named data contains the list of other relevant information for the cyl

#The objective here is to do the linear regression for all mpg vs wt for all levels of cyls

a1<-mtcars%>%nest(-cyl)%>%mutate(model=map(data,function(x)lm(mpg~wt,data=x))) #the function(x) imbibe the data from data column created after nesting
#This creates new column called model which stores relavant information of the regression



a3<-mtcars%>%nest(-cyl)%>%mutate(model=map(data,function(x)lm(mpg~wt,data=x)),tidied=map(model,tidy)) 
#the tidied basically contain the term ,estimate ,stderror ,statistics ,pvalue




a4<-mtcars%>%nest(-cyl)%>%mutate(model=map(data,function(x)lm(mpg~wt,data=x)),tidied=map(model,tidy))%>%unnest(tidied) 
#This bring out the tidied columns out


# tidy  function -> for term ,estimate ,stderror ,statistics ,pvalue (overall information)
# augment function -> for residuals and othet statistics (fitted information)

a5<-mtcars%>%nest(-cyl)%>%mutate(model=map(data,function(x)lm(mpg~wt,data=x)),tidied=map(model,augment))%>%unnest(tidied) 



