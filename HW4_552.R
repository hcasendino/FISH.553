# Name: Helen Casendino
# Homework 4 

##==Question 1a
Yelloweye<- read.csv ("YERockfish.csv")

##==Question 1b
Length.Age.Mat<- function(length, age, maturity){
  Length_Means<- tapply(X=length, INDEX=maturity, FUN=mean)
  Age_Means<- tapply(X=age, INDEX=maturity, FUN=mean, na.rm=TRUE)
  
  plot(age,length, xlab = "Age", ylab="Length", pch=20,xaxs="i",
       yaxt="n",xaxt="n", xlim=c(0,100), ylim=c(30,80))
  
    abline(h=Length_Means[[1]], col="gray")
    abline(h=Length_Means[[2]], col="blue", lty=2)
    text(x=50, y = 39, labels = c("Immature"),col="black", cex=1.0)
    text(x=50, y = 50, labels = c("Mature"),col="black", cex=1.0)
    
    axis(side=1, at=c(seq(0,80,20), 100), labels=c(0,20,40,60,80,100))
    axis(side=2, at=c(seq(30,70,10),80), labels=c(30,40,50,60,70,80))
    
    return(list("Length Means"=Length_Means, "Age Means"=Age_Means))
    
}

##==Question 1c
Cabezon<- read.csv ("Cabezon.csv")
Length.Age.Mat(Cabezon$length,Cabezon$age,Cabezon$maturity)

##==Question 1d
par(mfrow=c(1,2))
Length.Age.Mat(Yelloweye$length,Yelloweye$age,Yelloweye$maturity)
Length.Age.Mat(Cabezon$length,Cabezon$age,Cabezon$maturity)

