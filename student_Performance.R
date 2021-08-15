#Change system to english
Sys.setenv(LANGUAGE="en")

library(ggplot2)
library(tidyverse)
library(readr)
library(performance)
library(see)
library(heplots)
library("ggpubr")
library(broom)
library(AICcmodavg)
library(dplyr)

#documenation on read_delim
#https://www.rdocumentation.org/packages/readr/versions/1.3.1/topics/read_delim
Performance <- read_delim(file = "C:/Users/User/Google Drive/HSLU/Sem 5/BUINT/rExercise/Group_02_Rcode/StudPerfs.csv",
                            delim = ",",
                            col_names = c("Gender","Ethnicity","ParentalEducation","Lunch",
                                          "Preparation","Math","Reading","Writing"),
                            skip = 2, #first two lines are not relevant
                            col_types = cols(
                              Gender = col_factor(levels = c("female","male")),
                              Ethnicity = col_factor(levels = c("group A", "group B", 
                                                                "group C", "group D","group E")),
                              ParentalEducation = 
                                col_factor(levels = c("high school","some high school", "some college", 
                                                      "associate's degree", "bachelor's degree", "master's degree")),
                              Lunch = col_factor(levels = c("free/reduced", "standard")),
                              Preparation = col_factor(levels = c("none", "completed")),
                              Math = col_integer(),
                              Reading = col_integer(),
                              Writing = col_integer()
                            ),
                            na = c("", "NA", "TEST", "Test", "TESTING","?????"))

summary(Performance)
str(Performance)
#clean NA entries but no NA entries so no entry was dropped
Performance <- na.omit(Performance)



#seperating the dataset. 5% test, 95% training
set.seed(578)
index_train <- sample(1:nrow(Performance),0.95*nrow(Performance))
Performance.train <- Performance[index_train,]
dim(Performance.train)
Performance.test <- Performance[-index_train,]
dim(Performance.test)


par(mfrow=c(1,1))
#Visualizing each categorical variable to see if it is correlated
#Gender
ggboxplot(Performance, x = "Gender", y = "Math", 
          ylab = "Math", xlab = "Gender")
ggboxplot(Performance, x = "Gender", y = "Reading", 
          ylab = "Reading", xlab = "Gender")
ggboxplot(Performance, x = "Gender", y = "Writing", 
          ylab = "Writing", xlab = "Gender")
#Ethnicity
ggboxplot(Performance, x = "Ethnicity", y = "Math", 
          ylab = "Math", xlab = "Ethnicity")
ggboxplot(Performance, x = "Ethnicity", y = "Reading", 
          ylab = "Reading", xlab = "Ethnicity")
ggboxplot(Performance, x = "Ethnicity", y = "Writing", 
          ylab = "Writing", xlab = "Ethnicity")
#Parental Education
ggboxplot(Performance, x = "ParentalEducation", y = "Math", 
          ylab = "Math", xlab = "Parental Education")
ggboxplot(Performance, x = "ParentalEducation", y = "Reading", 
          ylab = "Reading", xlab = "Parental Education")
ggboxplot(Performance, x = "ParentalEducation", y = "Writing", 
          ylab = "Writing", xlab = "Parental Education")
#Lunch
ggboxplot(Performance, x = "Lunch", y = "Math", 
          ylab = "Math", xlab = "Lunch")
ggboxplot(Performance, x = "Lunch", y = "Reading", 
          ylab = "Reading", xlab = "Lunch")
ggboxplot(Performance, x = "Lunch", y = "Writing", 
          ylab = "Writing", xlab = "Lunch")
#Preparation
ggboxplot(Performance, x = "Preparation", y = "Math", 
          ylab = "Math", xlab = "Preparation")
ggboxplot(Performance, x = "Preparation", y = "Reading", 
          ylab = "Reading", xlab = "Preparation")
ggboxplot(Performance, x = "Preparation", y = "Writing", 
          ylab = "Writing", xlab = "Preparation")



### MATH MODEL
###

#calculate simple linear regression for Gender vs Math
Math.Gender = aov(Math~Gender, data=Performance.train)
coef(Math.Gender)
#calculate simple linear regression for Ethnicity vs Math
Math.Ethnicity = aov(Math~Ethnicity, data=Performance.train)
#calculate simple linear regression for Parental Education vs Math
Math.ParentalEducation = aov(Math~ParentalEducation, data=Performance.train)
#calculate simple linear regression for Lunch vs Math
Math.Lunch = aov(Math~Lunch, data=Performance.train)
#calculate simple linear regression for Preparation vs Math
Math.Preparation = aov(Math~Preparation, data=Performance.train)
#Calculate multiple regression model
Math.Multiple.1 = aov(Math~Gender*Ethnicity*ParentalEducation*Lunch*Preparation, data=Performance.train)
summary(Math.Multiple.1)
#Calculate multitple linear regression model with all significance variables
Math.Multiple.2 = aov(
  Math~
    Gender+Ethnicity+ParentalEducation+Lunch+Preparation+Gender:Lunch:Preparation+ParentalEducation:Lunch:Preparation, 
  data=Performance.train)
summary(Math.Multiple.2)
#Calculate multiple linear regression model with all significance variables
Math.Multiple.3 = aov(Math~Gender+Ethnicity+ParentalEducation+Lunch+Preparation, data=Performance.train)
summary(Math.Multiple.3)

#Comparing models
comparison <- compare_performance(
  Math.Gender,Math.Ethnicity,Math.ParentalEducation, Math.Lunch, Math.Preparation,
  Math.Multiple.1,Math.Multiple.2,Math.Multiple.3,
  rank=TRUE)
comparison
#result shows that Math.Multiple.3 has the lowest AIC, best performance score of 86.72%
#Chose Math.Multiple.3
Math.Fit = Math.Multiple.3
coef(Math.Fit) #details of intercept b and slopes.

#predict the values of Math based the regression model in test datasets 
predicted.Math <- predict(Math.Fit, Performance.test)
predicted.Math
Performance.test
#Calculate the residuals 
residuals.Math <- as.tibble(predicted.Math) %>% 
  mutate(real = Performance.test$Math, n=row_number()) %>% 
  mutate(error= value-real)
residuals.Math <- residuals.Math %>% arrange(real)  %>% mutate(n = row_number())
#Plotting the residuals
ggplot(data=residuals.Math) + 
  #errors
  geom_point(aes(x=n,y=error),color="blue") + 
  geom_abline(slope = 0)
#Plotting the real value in order and its corresponding predicted value
ggplot(data=residuals.Math) + 
  #real data from test set
  geom_point(aes(x=n,y=real),color="blue") +
  #predicted data
  geom_point(aes(x=n,y=value),color="green", alpha=0.25) + 
  geom_segment(
    aes(x=n,xend=n,y=real,yend=value, color=factor(sign(value-real),levels=c(-1,1))), 
    size=1, alpha=0.5)+
  geom_line(aes(x=n,y=(value+real)/2),color="black") +
  theme(legend.position = "none")



### READING MODEL
###

#calculate simple linear regression for Gender vs Reading
Reading.Gender = aov(Reading~Gender, data=Performance.train)
#calculate simple linear regression for Ethnicity vs Reading
Reading.Ethnicity = aov(Reading~Ethnicity, data=Performance.train)
#calculate simple linear regression for Parental Education vs Reading
Reading.ParentalEducation = aov(Reading~ParentalEducation, data=Performance.train)
#calculate simple linear regression for Lunch vs Reading
Reading.Lunch = aov(Reading~Lunch, data=Performance.train)
#calculate simple linear regression for Preparation vs Reading
Reading.Preparation = aov(Reading~Preparation, data=Performance.train)
#Calculate multiple regression model
Reading.Multiple.1 = aov(Reading~Gender*Ethnicity*ParentalEducation*Lunch*Preparation, data=Performance.train)
summary(Reading.Multiple.1)
#Calculate multitple linear regression model with all significance variables
Reading.Multiple.2 = aov(
  Reading~
    Gender+Ethnicity+ParentalEducation+Lunch+Preparation+Gender:Lunch:Preparation, 
  data=Performance.train)
summary(Reading.Multiple.2)
#Calculate multiple linear regression model with all significance variables
Reading.Multiple.3 = aov(Reading~Gender+Ethnicity+ParentalEducation+Lunch+Preparation, data=Performance.train)
summary(Reading.Multiple.3)

#Comparing models
comparison <- compare_performance(
  Reading.Gender,Reading.Ethnicity,Reading.ParentalEducation, Reading.Lunch, Reading.Preparation,
  Reading.Multiple.1,Reading.Multiple.2,Reading.Multiple.3,
  rank=TRUE)
comparison
#result shows that Reading.Multiple.3 has the lowest AIC, best performance score of 86.72%
#Chose Reading.Multiple.3
Reading.Fit = Reading.Multiple.3
coef(Reading.Fit) #details of intercept b and slopes.

#predict the values of Reading based the regression model in test datasets 
predicted.Reading <- predict(Reading.Fit, Performance.test)
predicted.Reading
Performance.test
#Calculate the residuals 
residuals.Reading <- as.tibble(predicted.Reading) %>% 
  mutate(real = Performance.test$Reading, n=row_number()) %>% 
  mutate(error= value-real)
residuals.Reading <- residuals.Reading %>% arrange(real)  %>% mutate(n = row_number())
#Plotting the residuals
ggplot(data=residuals.Reading) + 
  #errors
  geom_point(aes(x=n,y=error),color="blue") + 
  geom_abline(slope = 0)
#Plotting the real value in order and its corresponding predicted value
ggplot(data=residuals.Reading) + 
  #real data from test set
  geom_point(aes(x=n,y=real),color="blue") +
  #predicted data
  geom_point(aes(x=n,y=value),color="green", alpha=0.25) + 
  geom_segment(
    aes(x=n,xend=n,y=real,yend=value, color=factor(sign(value-real),levels=c(-1,1))), 
    size=1, alpha=0.5)+
  geom_line(aes(x=n,y=(value+real)/2),color="black") +
  theme(legend.position = "none")



### WRITING MODEL
###

#calculate simple linear regression for Gender vs Writing
Writing.Gender = aov(Writing~Gender, data=Performance.train)
#calculate simple linear regression for Ethnicity vs Writing
Writing.Ethnicity = aov(Writing~Ethnicity, data=Performance.train)
#calculate simple linear regression for Parental Education vs Writing
Writing.ParentalEducation = aov(Writing~ParentalEducation, data=Performance.train)
#calculate simple linear regression for Lunch vs Writing
Writing.Lunch = aov(Writing~Lunch, data=Performance.train)
#calculate simple linear regression for Preparation vs Writing
Writing.Preparation = aov(Writing~Preparation, data=Performance.train)
#Calculate multiple regression model
Writing.Multiple.1 = aov(Writing~Gender*Ethnicity*ParentalEducation*Lunch*Preparation, data=Performance.train)
summary(Writing.Multiple.1)
#Calculate multitple linear regression model with all significance variables
Writing.Multiple.2 = aov(
  Writing~
    Gender+Ethnicity+ParentalEducation+Lunch+Preparation+Gender:Lunch:Preparation, 
  data=Performance.train)
summary(Writing.Multiple.2)
#Calculate multiple linear regression model with all significance variables
Writing.Multiple.3 = aov(Writing~Gender+Ethnicity+ParentalEducation+Lunch+Preparation, data=Performance.train)
summary(Writing.Multiple.3)

#Comparing models
comparison <- compare_performance(
  Writing.Gender,Writing.Ethnicity,Writing.ParentalEducation, Writing.Lunch, Writing.Preparation,
  Writing.Multiple.1,Writing.Multiple.2,Writing.Multiple.3,
  rank=TRUE)
comparison
#result shows that Writing.Multiple.3 has the lowest AIC, best performance score of 86.72%
#Chose Writing.Multiple.3
Writing.Fit = Writing.Multiple.3
coef(Writing.Fit) #details of intercept b and slopes.

#predict the values of Writing based the regression model in test datasets 
predicted.Writing <- predict(Writing.Fit, Performance.test)
predicted.Writing
Performance.test
#Calculate the residuals 
residuals.Writing <- as.tibble(predicted.Writing) %>% 
  mutate(real = Performance.test$Writing, n=row_number()) %>% 
  mutate(error= value-real)
residuals.Writing <- residuals.Writing %>% arrange(real)  %>% mutate(n = row_number())
#Plotting the residuals
ggplot(data=residuals.Writing) + 
  #errors
  geom_point(aes(x=n,y=error),color="blue") + 
  geom_abline(slope = 0)
#Plotting the real value in order and its corresponding predicted value
ggplot(data=residuals.Writing) + 
  #real data from test set
  geom_point(aes(x=n,y=real),color="blue") +
  #predicted data
  geom_point(aes(x=n,y=value),color="green", alpha=0.25) + 
  geom_segment(
    aes(x=n,xend=n,y=real,yend=value, color=factor(sign(value-real),levels=c(-1,1))), 
    size=1, alpha=0.5)+
  geom_line(aes(x=n,y=(value+real)/2),color="black") +
  theme(legend.position = "none")

