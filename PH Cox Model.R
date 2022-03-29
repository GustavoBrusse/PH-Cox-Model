#### Gustavo Brusse - EDSD 2018/2019 ###
#### Assignment Event History 4 ########

## Exercise 1

setwd("C:\\Users\\gbrusse\\Desktop\\Event History 4")
stay <- read.table("dataset1.txt",header=T)

install.packages("smcure")
library(smcure)
library(survival)
library(MASS)

## 1.1 creating a PH Cox model

surv.dat <- Surv(stay$stay, stay$exit) 
m1 <- coxph(surv.dat ~ owner * educ * age_20, data=stay)

# Best model by AIC criterion
step.cox<-stepAIC(m1) 
summary(step.cox) 

##  variables in???uence whether people move of stay: Owner and age_20
## direction do these covariates act: negative effect. If the person is owner, less probability of moving
## if the person is older, less probability of moving.
## least likely of moving owners and older persons


##  1.2 Cure fraction model

# create interaction dummy
stay$edu_medium <- NA
stay$edu_medium <- ifelse(stay$educ=="M",1,0)
stay$edu_high<-ifelse(stay$educ=="H",1,0)

# Iteratctions
stay$int1 <- stay$owner*stay$edu_medium
stay$int2 <- stay$owner*stay$edu_high


# smcure model
m2 <- smcure(Surv(stay,exit)  ~ owner + edu_medium + edu_high + age_20 +  int1 + int2,
               cureform = ~ owner  + edu_medium + edu_high + age_20 + int1 + int2, 
               model="ph",  data=stay)


m3<-smcure(Surv(stay,exit)  ~ owner + age_20 ,
           cureform = ~ owner + age_20 + edu_medium + edu_high + int1 + int2, 
           model="ph",  data=stay)

## owner and age of move in are the variables that influence their hazard


## Exercise 2

setwd("C:\\Users\\gbrusse\\Desktop\\Event History 4")
phd <- read.table("dataset2.txt", header=T, sep=",")

install.packages("smoothSurv")
library(smoothSurv)

# creating an event variable
phd$event[phd$duration!=0]<-1

## 2.1 AFT model
sm.aft1 <- smoothSurvReg(Surv(duration, event)~ sex + residence + topic + field, data=phd, 
                         lambda=exp(seq(2,-8, by=-0.5)))
summary(sm.aft1)

# Plot
plot(sm.aft1)


