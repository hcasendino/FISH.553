# Name: Helen Casendino
# Homework 3 

##==Question 1a
mack.black<- read.csv("MACKBLACK.txt",dec =".", sep="")
names(mack.black)<- c("Year", "spawners", "recruits", "catch" ,"fishMortality")
mack.nafo<- read.csv("MACKNAFO.txt",dec =".", sep="")
names(mack.nafo)<- c("Year", "spawners", "recruits", "catch" ,"fishMortality")
mack.ices<- read.csv("MACKICES.txt",dec =".", sep="")
names(mack.ices)<- c("Year", "spawners", "recruits", "catch" ,"fishMortality")

##==Question 1b
mack.partial<- merge(mack.black,mack.nafo, by= "Year")
names(mack.partial)<- c("Year",
                      "spawners.black",
                      "recruits.black" ,
                      "catch.black",
                      "fishMortality.black",
                      "spawners.nafo",
                      "recruits.nafo" ,
                      "catch.nafo",
                      "fishMortality.nafo")

##==Question 1c
mack<-  merge(mack.partial,mack.ices, by= "Year")

##==Question 1d
names(mack)[10:13]<- c(
                "spawners.ices",
                "recruits.ices" ,
                "catch.ices" ,
                "fishMortality.ices")

##==Question 1e
matplot(mack$Year, mack[,c(2,6,10)],
        xlab="Years", ylab="Spawner Biomass (thousands of tons)",
        type="l", lty=2, main="Atlantic Mackerel")
legend(x="topleft", legend=c("Black Sea", "NAFO", "ICES"),
       bty="n", lty = 2, cex=.75, col=c("black", "red", "green"))


##==Question 2a
mean_vect<- c(40 ,42 ,51 ,55 ,58 ,62)
month_vect<- c(as.numeric(difftime(as.Date("2010-02-01"), as.Date("2010-01-01"))), 
               as.numeric(difftime(as.Date("2010-03-01"), as.Date("2010-02-01"))),
               as.numeric(difftime(as.Date("2010-04-01"), as.Date("2010-03-01"))),
               as.numeric(difftime(as.Date("2010-05-01"), as.Date("2010-04-01"))),
               as.numeric(difftime(as.Date("2010-06-01"), as.Date("2010-05-01"))),
               as.numeric(difftime(as.Date("2010-07-01"), as.Date("2010-06-01"))))

temperature<- data.frame(date=seq(from=as.Date("2010/1/1"), to=as.Date("2010/6/30"),by=1), 
                         temp=rep(NA, length.out=length(seq(from=as.Date("2010/1/1"), to=as.Date("2010/6/30"),by=1))))
for(i in 1:6){
  month_temp<-rnorm(n=month_vect[i], mean=mean_vect[i], sd=5)
  month_temp<- round(x=month_temp, digits=0)
  if(i==1){
    temperature[1:31,2]<- month_temp
  }
  if(i==2){
    temperature[32:59,2]<- month_temp
  }
  if(i==3){
    temperature[60:90,2]<- month_temp
  }
  if(i==4){
    temperature[91:120,2]<- month_temp
  }
  if(i==5){
    temperature[121:151,2]<- month_temp
  }
  if(i==6){
    temperature[152:181,2]<- month_temp
  }
}

##==Question 2b 
m<- c(rep("January", times=31), rep("February",times=28),
      rep("March",times=31), rep("April",times=30), 
      rep("May",times=31), rep("June", times=30))
tapply(X=temperature$temp, INDEX=m, FUN=mean)

##==Question 2c
temperature$date[duplicated(temperature$temp)==TRUE]

##==Question 2d
observations<- data.frame(date=seq(from=as.Date("2010/1/1"), to=as.Date("2010/7/31"),by=2), 
                          conditions=sample(x=c("sunny", "cloudy", "partly cloudy"), 
                                            size=length(seq(from=as.Date("2010/1/1"), to=as.Date("2010/7/31"),by=2)), replace=T),
                          "wind speed"= rnorm(n=length(seq(from=as.Date("2010/1/1"), to=as.Date("2010/7/31"),by=2)), mean=5, sd=3))
observations[observations$wind.speed < 0,3] <- 0

##==Question 2e
weather<- merge(temperature,observations, by = "date")

##==Question 2f
tapply(X=weather$temp, INDEX=weather$conditions, FUN=min)
tapply(X=weather$temp, INDEX=weather$conditions, FUN=max)

sum(weather$conditions=="cloudy")
sum(weather$conditions=="partly cloudy")
sum(weather$conditions=="sunny")



