####################
# PUBL 6312        #
# Semester Project #
####################

setwd("C:/Users/dirk2/Google Drive/Graduate School/Fall 2018/PUBL 6312/Semester Paper")

library(readr)
FedEmp <- read_csv("Data/FedEmp.csv")
attach(FedEmp)

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(stargazer)
require(aod)
require(ggplot2)

#breakdown of each variable
lapply(FedEmp[, c("Leaving", "Sex", "Educ", "FedTen", "Super", "Minority", "Q4", "Q13", "Q51", "Q61", "Q69", "Q70", "Q71")], table)

#convert variables to factors
FedEmp$LeavingD <- as.factor(FedEmp$Leaving)

#ordinal logistic regression
m <- glm(LeavingD~Sex+Educ+FedTen+Super+Minority+Q4+Q13+Q51+Q61+Q69+Q70+Q71, data=FedEmp, family="binomial")
summary(m)
stargazer(m,type="text")

#odds ratios
exp(cbind(OR = coef(m), confint(m)))

#predicted outcomes
predictdata<-cbind(LeavingD=seq(0,1,length=100),
                   Sex=mean(Sex),Educ=mean(Educ),
                   FedTen=mean(FedTen),Super=mean(Super),
                   Minority=mean(Minority),Q4=mean(Q4, na.rm=TRUE),
                   Q13=mean(Q13, na.rm=TRUE),Q51=mean(Q51, na.rm=TRUE),
                   Q61=mean(Q61, na.rm=TRUE),Q69=mean(Q69, na.rm=TRUE),
                   Q70=mean(Q70, na.rm=TRUE),Q71=mean(Q71, na.rm=TRUE))
opinion.hat<-predict(m,predictdata,type='response')