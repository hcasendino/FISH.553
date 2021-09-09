speciesCode<- read.csv("speciesCode.csv")
 speciesData<- read.csv("speciesData.csv")

speciesCounts<- table(speciesData$SpeciesCode)
 # largest number of fish with one code (same fish)
maxCode<- as.numeric(names(speciesCounts[which.max(speciesCounts)]))
head(speciesCounts)
# species info on most caught fish 
max.spp<- speciesCode[speciesCode$speciesCode==maxCode,]

#which row indices have boca 
bocaccioRows<- grep("Bocaccio", speciesCode$Common)
# all the rows with bocaci fish
speciesCode[bocaccioRows,]
#whats the species code of bocaccio fish 
bocaccioCode<- speciesCode[bocaccioRows,"SpeciesCode"]
#give me only species data with species code belonging to boca 
bocaccioData<- subset(speciesData,SpeciesCode==bocaccioCode)

#merge data
bocTrip<- merge(bocaccioData, tripData[,-1],by.x="TripNum",by.y="SimplifiedTripNum")


