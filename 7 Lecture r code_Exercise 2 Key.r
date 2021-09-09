#=======Exercise 2 Answer Key

#load data
speciesCode <- read.csv("speciesCode.csv")
speciesData <- read.csv("speciesData.csv")
tripData <- read.csv("tripData.csv")


#=======In-class exercise 2
#==Q1
SebastesRows <- grep("Sebastes",speciesCode$Scientific)
SebastesRows
SebastesCode <- speciesCode[SebastesRows,"SpeciesCode"]
SebastesCode
sort(unique(SebastesCode))
names(speciesData)
x <- speciesData[speciesData$SpeciesCode %in% SebastesCode, 
                 "SpeciesCode"]
x
sort(unique(x))  #check this is a subset
SebastesData2 <- speciesData[speciesData$SpeciesCode %in% 
                               SebastesCode,]
head(SebastesData2)

#==Q2
#Table of fates of rockfish by species code
table(SebastesData2$SpeciesCode, SebastesData2$Fate)
#Fate is a factor, convert to text to get rid of blank factor
table(SebastesData2$SpeciesCode, as.character(SebastesData2$Fate))


n<- speciesCode[SebastesRows, -2]
m<- merge(n,SebastesData2, by.x= "SpeciesCode" )
table(m$Common, as.character(m$Fate))

#==Q3
names(SebastesData2)
#needs na.rm=T because of the NAs
tapply(X=SebastesData2$Length, INDEX=SebastesData2$SpeciesCode, FUN=min)
tapply(X=SebastesData2$Length, INDEX=SebastesData2$SpeciesCode, 
       FUN=min, na.rm=T)
tapply(X=SebastesData2$Length, INDEX=SebastesData2$SpeciesCode, FUN=max, 
       na.rm=T)
