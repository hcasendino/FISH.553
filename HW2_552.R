# Name: Helen Casendino 
# Homework 2

#==Question 1a
means<- tapply(X=beaver1$temp, INDEX=beaver1$activ, FUN=mean)

#==Question 1b
plot(x=c(1:length(beaver1$temp)),  y=beaver1$temp, 
     xlab="observation #", ylab="temperature", main="Beaver 1 body temperature",
     type="l", xaxt="n",yaxt="n")
axis(side=1, at=c(seq(0,100,20), 114), labels=c(1,20,40,60,80,100,114))
axis(side=2, at=seq(36.4,37.4,.2))
abline(h=means[[1]], type="l",lty=2, col="snow3")
abline(h=means[[2]], type="l",lty=3, col="snow3")
legend(x="topleft", legend=c("inside mean temperature","outside mean temperature"), 
                             cex=.7, lty=c(2,3), col="snow3", bty="n")
points(x=which(beaver1$activ==1), y=beaver1$temp[which(beaver1$activ==1)], pch=16, col="green")


#==Question 1c
newbeav<- rbind(beaver1, c(346,2220,37.3,1))
newbeav

#==Question 1d
mean(newbeav$temp[newbeav$activ==1]) - mean(beaver1$temp[beaver1$activ==1]) 
# The new temp mean is 0.01095238 degrees higher than the old mean. 

#==Question 2a
library(MASS)
head(crabs)
tapply(X=crabs$FL,INDEX=crabs$sp, FUN=mean)
tapply(X=crabs$FL,INDEX=crabs$sp, FUN=sd)

#==Question 2b
# This line of code (a factor) is the same length as the crabs dataset (ie, one element for each observation).
# Each level is a unique combination of sex and species, and each observation in the crabs dataset has been assigned one of these levels. 
spsex<- crabs$sp:crabs$sex

#==Question 2c
levels(spsex)[1:4] <- c("Blue Female","Blue Male","Orange Female", "Orange Male")

#==Question 2d
tapply(X=crabs$FL,INDEX=spsex, FUN=mean)

#==Question 2e
crab.counts<- cbind(
             c(length(which(spsex=="Orange Female")), 
                length(which(spsex=="Orange Male")),
                length(which(spsex=="Blue Female")),
                length(which(spsex=="Blue Male"))), 
             c("Orange Female","Orange Male","Blue Female","Blue Male"))
# all have 50

#==Question 2f
plot(spsex, crabs$CL, xlab="Sex and Species", 
     ylab="Carapace length (mm)", 
     main="Crab Carapace Length Difference by Sex and Species", 
     cex=.7, col=c("blue", "turquoise", "orange","yellow"))
legend(x="topleft", bty="n", cex=0.7, col=c("blue", "turquoise", "orange","yellow"), 
       legend=(c("Blue Female","Blue Male","Orange Female","Orange Male")), pch=16)





