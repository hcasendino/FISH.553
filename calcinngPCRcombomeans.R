
---
  title: "rareSp.dissimilarity"
author: "Helen Casendino"
date: "7/30/2021"
output: html_document
---
  
library(tidyverse)
library(vegan)
library(gridExtra)

# This code will look at the impact of rare species on dissimilarity between PCR replicates. 

## Looking at PCR triplicates

# We'll start with PCR.proportions.clean.csv, which has the proportions of each hash by PCR replicate (ONLY triplicate PCR samples) and biological samples with really 1 low read PCR replicate are removed (put into PCR.duplic.proportions.csv). 

trip.proportion.reads <- read.csv('../data/PCR.proportions.clean.csv')

dup.proportion.reads <- read.csv('../data/PCR.duplic.proportions.csv')


# Below is my start to making the subsetting loop with BCD centroids for each sample. So, for each sample, we'll have a rank of most common hashes to least common. We will subset this with varying exclusion proportions (ie, removing bottom 1% of hashes, bottom 5%). For each subset, calculate the centroid of 3 pairwise bray curtis dissimilarities: PCRs 1&2, 2%3 and 1&3.   

# df is a data frame with 6 columns: Miseq_run, bio, Hash, PCR1_prop, PCR2_prop, PCR3_prop
# exclusionProp is a vector of all the subsetting proportions you want. A vector of (0.01, 0.05) means you'll get the PCR centroid for when 1% of the lowest proportion samples are excluded, and when 5% of lowest prop samples are excluded.
# bioVector is a vector of all biological sample IDs in your dataset.

PCRcentroid_bySample <- function(df, exclusionProp, bioVector){
  
  mat <- matrix(NA, nrow = length(exclusionProp), ncol = length(bioVector))
  
  for(j in 1:length(bioVector)){
    
    bioSample <- bioVector[j]
    bioProps_Hashes <- df[df$bio == bioSample,] # to get all PCR props from that one sample
    
    meanProp.acrossPCR <- bioProps_Hashes %>% group_by(Hash) %>% summarise(meanProp.PCR = mean(c(PCR1_prop, PCR2_prop,PCR3_prop))) %>% arrange(desc(meanProp.PCR)) # to get rarity order
    
    for(i in 1:length(exclusionProp)){
      cutoff <- round(length(meanProp.acrossPCR$Hash) - length(meanProp.acrossPCR$Hash)*exclusionProp[i])
      sub.tib <- meanProp.acrossPCR[1:cutoff,]
      sub.tib <- bioProps_Hashes %>% filter(Hash %in% sub.tib$Hash) %>% select(!c(Hash, bio,Miseq_run))
      
      flip.tib <- t(sub.tib)
      dis <- vegdist(flip.tib)
      grouping <- factor(rep(1,3), labels=bioSample)
      
      centroid<- betadisper(dis, group = grouping)
      centroid$distances 
      mat[i,j] <- mean(centroid$distances)
    }
    
    print(j) # progress bar
  }
  return(mat)
}

# returns a matrix with biological replicate as columns and exclusion proportion as rows. 

exclvec<- seq(0,0.5, 0.01)
centroids <- PCRcentroid_bySample(trip.proportion.reads, exclvec, unique(trip.proportion.reads$bio))

# Now let's visualize the output of our data

### 1) Graph of centroid distances averaged over bio samples

mean.centroidDists <- rowMeans(as.data.frame(centroids)) # average distances to centroid of PCR reps for all exclusion proportion values across biological replicates

cutoff.centroid.df <- data.frame(exclusion.prop = exclvec, cent.distance = mean.centroidDists) # combines with exclusion prop vector

trip.centroids <- ggplot(cutoff.centroid.df, aes(x=exclusion.prop, y=cent.distance, color="orange")) + geom_point() +  theme_minimal() + theme(legend.position = "none") + xlab("% Rare Species Excluded") + ylab("Distance to Centroid") + ggtitle("Triplicates") 

### 2) Graph of centroid distances without averaging bio samples

centroidDists <- as.data.frame(centroids)
colnames(centroidDists) <- unique(trip.proportion.reads$bio)
centroidDists$exclprop <- exclvec

centroid.scatplot.df <- centroidDists %>% pivot_longer(unique(trip.proportion.reads$bio), names_to = "bio", values_to = "centroid")

# actual scatter plot 
centroid.scatplot.trip <- ggplot(centroid.scatplot.df) + geom_point(aes(x=exclprop, y=centroid, color=bio)) +  theme_minimal()  + theme(legend.position = "none") + xlab("% Rare Species Excluded") + ylab("Distance to Centroid")

# I'm also curious about what values these percent cutoffs are representing. Let's take an average proportion value, and see what percent of each bio sample's Hash prop data it makes up (see Prop_Percent.of.sample.func.R in code/miscellaneous for more details)

source('code/miscellaneous/Prop_Percent.of.sample.func.R') # load function
output <- Prop_Percent.of.sample(trip.proportion.reads, unique(trip.proportion.reads$bio), 0.01) # how much of each sample do Hashes with avg props < 0.02 take up?
hist(output)

# Interesting...so Hashes with avg props < 0.01 take up at least 85% of each sample.


## Looking at PCR duplicates

# Let's repeat with duplicate data.

# same function as above, just with two minor details changed (2 PCRs instead of 3)
PCRcentroid_bySample_dup <- function(df, exclusionProp, bioVector){
  
  mat <- matrix(NA, nrow = length(exclusionProp), ncol = length(bioVector))
  
  for(j in 1:length(bioVector)){
    
    bioSample <- bioVector[j]
    bioProps_Hashes <- df[df$bio == bioSample,] # to get all PCR props from that one sample
    
    meanProp.acrossPCR <- bioProps_Hashes %>% group_by(Hash) %>% summarise(meanProp.PCR = mean(c(PCR1_prop, PCR2_prop))) %>% arrange(desc(meanProp.PCR)) # to get rarity order
    
    for(i in 1:length(exclusionProp)){
      cutoff <- round(length(meanProp.acrossPCR$Hash) - length(meanProp.acrossPCR$Hash)*exclusionProp[i])
      sub.tib <- meanProp.acrossPCR[1:cutoff,]
      sub.tib <- bioProps_Hashes %>% filter(Hash %in% sub.tib$Hash) %>% select(!c(Hash, bio,Miseq_run))
      
      flip.tib <- t(sub.tib)
      dis <- vegdist(flip.tib)
      grouping <- factor(rep(1,2), labels=bioSample)
      
      centroid<- betadisper(dis, group = grouping)
      centroid$distances 
      mat[i,j] <- mean(centroid$distances)
    }
    
    print(j) # progress bar
  }
  return(mat)
}

exclvec<- seq(0,0.5, 0.01)
centroids <- PCRcentroid_bySample_dup(dup.proportion.reads, exclvec, unique(dup.proportion.reads$bio))



