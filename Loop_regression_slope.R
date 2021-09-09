### Loops Quiz

####======================

# For each animal using a loop
# Check if there are two or more data points
# If yes, regress length on age for that animal
# Store the slope
# Plot a histogram of the slopes of each regression

######===========================


data<- read.csv("Lect2.csv", sep = ",")

colnames(data) <- c("row", "id", "age", "weight")

animals <- unique(data$id)
n_loops <- length(animals)
slopes<- rep(NA, length=n_loops)

for(i in 1:n_loops){
  one.data<- data[data$id==animals[i],]
  
  if(nrow(one.data) >= 2){
    reg <- lm(data$weight~data$age)
    slopes[i]<- coefficients(reg)[2]
  }
}
slopes
hist(slopes)

# ? 
