# Name: Helen Casendino
# Homework 1

##==Question 1a
x<- seq(-1,1, length.out=100)

##==Question 1b
y<- exp(x/2)

##==Question 1c
sum(y>1)
#50

##==Question 1d
fact<- rep(c("Small", "Medium", "Large"), times=5)
size<-factor(x=fact, labels=c("Small", "Medium", "Large"))

##==Question 1e
fact_2<- c(fact, "Unknown", "Unknown")
observedSize<- factor(x=fact_2, labels=c("Small", "Medium", "Large", "Unknown"))

##==Question 2a
library(readr)
fishPassage<- read_csv("fishPassage.csv")

##==Question 2b
min(fishPassage[fishPassage$Dam=="BON", "Wild.Steelhead"], na.rm=TRUE)
# 17375
fishPassage$Year[which(fishPassage$Wild.Steelhead==min(fishPassage[fishPassage$Dam=="BON", "Wild.Steelhead"], na.rm=TRUE))]
#1996
max(fishPassage[fishPassage$Dam=="BON", "Wild.Steelhead"], na.rm=TRUE)
# 149582
fishPassage$Year[which(fishPassage$Wild.Steelhead==max(fishPassage[fishPassage$Dam=="BON", "Wild.Steelhead"], na.rm=TRUE))]
#2001

##==Question 2c
sum(fishPassage[fishPassage$Dam=="BON" & fishPassage$Year==2007,3:13], na.rm = TRUE)
#3474405

##==Question 2d
fishPassage1995BON<- fishPassage[fishPassage$Dam=="BON" & fishPassage$Year>=1995, 1:13]

##==Question 2e
ratio<- (fishPassage1995BON$Coho.Jack)/(fishPassage1995BON$Coho.Adult)
cohoPassage<- matrix(data=c(fishPassage1995BON$Coho.Adult, fishPassage1995BON$Coho.Jack,ratio), ncol=3)

##==Question 2f
# Coho Adults
mean(cohoPassage[,1])
# Coho Jacks
mean(cohoPassage[,2])
# Ratio of jacks to adults
mean(cohoPassage[,3])

##==Question 2g
round(mean(cohoPassage[,1]), digits=2)
round(mean(cohoPassage[,2]), digits=2)
round(mean(cohoPassage[,3]), digits=2)

##==Question 2h
fishPassage[which(fishPassage$Chinook.Adult==max(fishPassage$Chinook.Adult)), 1:13]
