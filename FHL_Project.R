###############
# Modeling & Map Figure for FHL 446 Paper
# RStudio Version 1.3.1056
# Started: 2 July 2021
# Last Edited: 4 July 2021

#####==Dependencies=====

library(maps)
library(mapdata)
library(maptools) 
library(scales)
library(rgdal)
library(ggmap)
library(ggsn)
library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(ggplot2)
library(lme4)
library(dplyr)

#####==MAP=====

bounds<-c(left=-123.055, bottom=48.5 , right=-122.94 , top=48.6)
SiteMap <- get_stamenmap(bounds, zoom=13, maptype = "toner-lite") %>% ggmap() + 
  theme(legend.position = "none", axis.title.x = element_blank(),axis.title.y = element_blank()) + 
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  geom_point(aes(x=-122.966, y=48.535), colour="darkolivegreen", size=1) +
  annotate("text", x=-122.967, y=48.54, label= "TR", size=5, colour="darkgreen") +
  geom_point(aes(x=-123.038, y=48.565), colour="blue4", size=1) +
  annotate("text", x=-123.037, y=48.57, label= "PC", size=5,colour="blue4")

bounds<-c(left=-123.2, bottom=48.4 , right=-122.8 , top=48.73)
SJIMap <- get_stamenmap(bounds, zoom=12, maptype = "toner-lite") %>% ggmap() + 
  theme(legend.position = "none", axis.title.x = element_blank(),axis.title.y = element_blank()) + 
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  geom_point(aes(x=-122.9645, y=48.535), colour="darkolivegreen", size=3) +
  annotate("text", x=-123.06, y=48.51, label= "Turn Rock (TR)", size=4, colour="darkgreen") +
 geom_point(aes(x=-123.022, y=48.563), colour="blue4", size=3) +
  annotate("text", x=-123.022, y=48.585, label= "Point Caution (PC)", size=4,colour="blue4")

bounds<-c(left=-125 , bottom=46.8 , right=-122 , top=49.5)
SalishMap <- get_stamenmap(bounds, zoom=9, maptype = "toner-lite") %>% ggmap()+
  geom_rect(xmin=-123.2 , ymin=48.4 , xmax=-122.8 , ymax=48.75, alpha=0.1, col="blue4")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10)) + 
  annotate("text", x=-123.5, y=47.75, label= "Washington", size=4)


# Panel plot

multipanel <- plot_grid(SalishMap, SJIMap, SiteMap, labels = c('a', 'b','c'), label_size = 16, nrow = 1)
y.lab <- textGrob("Latitude (dec)", 
                  gp=gpar( fontsize=13), rot=90)
x.lab <- textGrob("Longitude (dec)", 
                  gp=gpar( fontsize=13))

# Combining panels
add_x<- plot_grid(multipanel, x.lab, ncol = 1,  rel_heights = c(1, .1))
add_y <- plot_grid(y.lab, add_x, nrow = 1, rel_widths = c(0.1, 1))
complete_plot<- add_y+ theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"))+ theme(plot.background = element_rect(color = "grey", size = 1))
complete_plot <- complete_plot+ north(x.min = .9,x.max=1, y.min = .75, y.max=0.85, scale=1)
complete_plot
# save plot
ggsave("map.png", complete_plot, 
       width = 5.97,
       height = 4,
       dpi = 300, scale=1.5)

######=========Thickness Data========

# Read in Data
rawthickness <- read.csv("FHL_thickness.csv")

# Cleaning Thickness Data
colnames(rawthickness) <- rawthickness[2,]
thickness <- rawthickness[-c(1,2),]
thickness[,1:3] <- sapply(thickness[,1:3], as.numeric)
thickness[,1:3] <- sapply(thickness[,1:3], as.numeric)

# Convert thickness units to mm (on 4x compound scope, 35.5 units = 1 mm)
thickness[,1:3] <- (thickness[,1:3])/35.5
thickness[,1:3] <- (thickness[,1:3])*1000 #convert to micrometers

# Test relationships 
plot(thickness$margin_1, thickness$margin_2, xlim=c(0, 1100),ylim=c(0, 1100)) # looks like positive correlation, good
par(mfrow=c(1,3))
hist(thickness$margin_1, xlim=c(0,1100))
hist(thickness$margin_2, xlim=c(0,1100))
hist(thickness$center, xlim=c(0,1100))

# margin_1 and margin_2 have similar means and ranges, lets combine them by taking an average margin score for each blade
thickness[,6] <- apply(thickness[,1:2], MARGIN=1, FUN=mean)
thickness <- thickness %>% rename("mean_margin" = "V6") %>% select(-c(margin_1, margin_2))

thickness <- thickness %>% pivot_longer(!c(Site, Blade_position), names_to = "center_margin", values_to = "thick")
thickness[67:70,2] <- "M" # we'll ignore that these blades were length outliers for now
thickness$Site <- as.factor(thickness$Site)
thickness$Blade_position <- as.factor(thickness$Blade_position)
thickness$center_margin <- as.factor(thickness$center_margin)

# Center and Margin thickness measurements had similar standard deviations 
thickness %>% filter(center_margin == "mean_margin") %>% summarise(var = sd(thick))
thickness %>% filter(center_margin == "center") %>% summarise(var = sd(thick))

## What is the difference in thickness means between two sites? models didn't find Site to be a detemining factor of thickness. 
thickness %>% group_by(Site,center_margin) %>% summarise(var= mean(thick)) # means for center and margin measurements are within ~20 micrometers between sites

#####=====Thickness Models======
m1 <- lm(thick ~ center_margin, data= thickness) # center or margin explains variation in thickness (p<0.0001)

m2 <- lm(thick ~ center_margin + Blade_position, data= thickness) 
# both center/margin (p<0.0001) and medial/lateral (0.0387) explain variation in thickness. 
# Medial blades are thicker, center of blades is thicker 

m3 <- lm(thick ~ center_margin + Blade_position + Site, data= thickness) 
# Add site as a fixed effect, shouldn't be correlated with center_margin or Blade_position. No significant effect of site

# m2 had lowest AIC, following models explore whether different sites had different slopes for blade position and center/margin
m4 <- lm(thick ~ center_margin:Site + Blade_position, data= thickness) 
m5 <- lm(thick ~ center_margin + Blade_position:Site, data= thickness) 
m6 <- lm(thick ~ center_margin:Site + Blade_position:Site, data= thickness) 

deltaAICfunc <- function(AICvec){
  
  delta_AICvec <- rep(NA, times=length(AICvec))
  for(i in 1:length(AICvec)){
    delta_AICvec[i] <- AICvec[i] - min(AICvec)
  }
  
  weightsvec <- rep(NA, times=length(AICvec))
  for(i in 1:length(AICvec)){
    weightsvec[i] <- exp(-0.5*delta_AICvec[i])/(sum(exp(-0.5*delta_AICvec)))
  }
  
  output <- data.frame(AICc = AICvec, delta_AIC = delta_AICvec, weights = weightsvec)
  return(output)
  
} 


# all looks good 
sim<-DHARMa::simulateResiduals(m2,plot=T)
sim<-DHARMa::simulateResiduals(m5,plot=T)
deltaAICfunc(c(AIC(m1),AIC(m2),AIC(m3),AIC(m4),AIC(m5),AIC(m6)))

#####===Width and Ruffle Data=======

rawwidthLength <- read.csv("FHL_widthlength.csv")

# Cleaning Width and Length Data
colnames(rawwidthLength) <- rawwidthLength[2,]
widthLength <- rawwidthLength[-c(1:4),]
widthLength <- widthLength %>% separate(Site.Indiv, c("Site","Indiv"))
widthLength <-widthLength %>% drop_na()

# get means of 2 measurements per individual
widthMeans <- widthLength %>% pivot_wider(names_from = c(Indiv, Site,blade.position), values_from = "width ") %>% 
  select(-c("Total Length ", "Ruffle Length "))
widthMeans[,] <- sapply(widthMeans[,], as.numeric)
w <- apply(widthMeans, MARGIN=2, FUN= mean, na.rm=T)

ruffleMeans <- widthLength %>% pivot_wider(names_from = c(Indiv, Site,blade.position), values_from = "Ruffle Length ") %>% 
  select(-c("Total Length ", "width "))
ruffleMeans[,] <- sapply(ruffleMeans[,], as.numeric)
r<- apply(ruffleMeans, MARGIN=2, FUN= mean, na.rm=T)

totalMeans <- widthLength %>% pivot_wider(names_from = c(Indiv, Site,blade.position), values_from = "Total Length ") %>% 
  select(-c("Ruffle Length ", "width "))
totalMeans[,] <- sapply(totalMeans[,], as.numeric)
t <- apply(totalMeans, MARGIN=2, FUN= mean, na.rm=T)

# UNITE For each individual (medial/lateral, TR/PC)
merged <- as.data.frame(cbind(w,t,r))
merged[,4] <- rownames(merged)
merged <- merged %>% separate(V4, c("Indiv","Site","blade.position"), sep="_") %>% rename(Blade_position = blade.position)
# Divide by 25 to get ruffle score
merged$r <- (merged$r)/25

#######=====WidthLength Models =====

# Looking at ruffle 
summary(m7 <- lm(r ~ Site, data= merged)) # TR has lower ruffle score! 
summary(m7.1 <- lm(r ~ Site + w, data= merged))
summary(m7.2 <- lm(r ~ Site + w:Site, data= merged))
summary(m7.3 <- lm(r ~ Site + t, data= merged))
summary(m7.4 <- lm(r ~ Site + t:Site, data= merged)) 
summary(m7.5 <- lm(r ~ Site + t:Site + Blade_position:Site, data= merged)) 
deltaAICfunc(c(AIC(m7), AIC(m7.1),AIC(m7.2),AIC(m7.3),AIC(m7.4), AIC(m7.5)))
# m7.4 Most supported. TR has lower ruffle score (by around 6.2 cm)! 
# B/w 2 sites, total length was more important in determining ruffle at Point Caution rather than Turn Rock.
#Blade Position doesn't explain variation in ruffle at either site
sim<-DHARMa::simulateResiduals(m7.4,plot=T) # good 


summary(m8 <- lm(w ~ Site, data= merged)) 
summary(m8.1 <- lm(w ~ Site + r, data= merged))
summary(m8.2 <- lm(w ~ Site + r:Site, data= merged))
summary(m8.3 <- lm(w ~ Site + t, data= merged))
summary(m8.4 <- lm(w ~ Site + t:Site, data= merged)) 
summary(m8.5 <- lm(w ~ Site + t:Site + Blade_position:Site, data= merged)) 
deltaAICfunc(c(AIC(m8), AIC(m8.1),AIC(m8.2),AIC(m8.3),AIC(m8.4), AIC(m8.5)))
# m8.5: TR is narrower! Supports modelling that total length has different effects on width in PC that it does in TR (sig).
# Supports interaction between blade position and width (M blades wider at PC, narrower at TR, NOT SIGNIFICANT)
sim<-DHARMa::simulateResiduals(m8.5,plot=T) # bad! Use m8.4 


summary(m9 <- lm(t ~ Site, data= merged)) 
summary(m9.1 <- lm(t ~ Site + r, data= merged))
summary(m9.2 <- lm(t ~ Site + r:Site, data= merged))
summary(m9.3 <- lm(t ~ Site + w, data= merged))
summary(m9.4 <- lm(t ~ Site + w:Site, data= merged)) 
summary(m9.5 <- lm(t ~ Site + w:Site + Blade_position:Site, data= merged)) 
deltaAICfunc(c(AIC(m9), AIC(m9.1),AIC(m9.2),AIC(m9.3),AIC(m9.4), AIC(m9.5)))
# m9.3. Total length is unaffected by site (TR or PC). Also unexplained by width. (??)
sim<-DHARMa::simulateResiduals(m9.3,plot=T) # good


######======Ruffle and Thickness?#========

full <- left_join(merged, thickness, by=c("Site", "Blade_position"))

#Use best model from earlier thickness modelling 
summary(m10 <- lm(thick ~ r:center_margin + center_margin + Blade_position, data= full) )
summary(m10.1 <- lm(thick ~ r + center_margin + Blade_position, data= full) )
deltaAICfunc(c(AIC(m10), AIC(m10.1)))
#ruffle does affect thickness (more ruffle = lower thickness), just not differentially on margin or center
# results on blade position and center/margin agree with m2
sim<-DHARMa::simulateResiduals(m10.1,plot=T) # good

 
####=====AIC Table=====
tab1<- deltaAICfunc(c(AIC(m1), AIC(m2),AIC(m3),AIC(m4),AIC(m5),AIC(m6)))
rownames(tab1)<- c("thickness ~ blade area",
                   "thickness ~ blade area + blade position",
                   "thickness ~ blade area + blade position + site",
                   "thickness ~ blade area*site + blade position",
                   "thickness ~ blade area + blade position*site",
                   "thickness ~ blade area*site + blade position*site")

tab2<- deltaAICfunc(c(AIC(m7), AIC(m7.1),AIC(m7.2),AIC(m7.3),AIC(m7.4),AIC(m7.5), c(AIC(m8), AIC(m8.1),AIC(m8.2),AIC(m8.3),AIC(m8.4), AIC(m8.5), AIC(m9), AIC(m9.1),AIC(m9.2),AIC(m9.3),AIC(m9.4), AIC(m9.5))))
rownames(tab2)<- c("undulation ~ site",
                   "undulation ~ site + width",
                   "undulation ~ site + width*site",
                   "undulation ~ site + total length",
                   "undulation ~ site + total length*site",
                   "undulation ~ site + total length*site + blade position*site",
                   "width ~ site",
                   "width ~ site + undulation",
                   "width ~ site + undulation*site",
                   "width ~ site + total length",
                   "width ~ site + total length*site",
                   "width ~ site + total length*site + blade position*site" ,
                   "total length ~ site",
                   "total length ~ site + undulation",
                   "total length  ~ site + undulation*site",
                   "total length  ~ site + width",
                   "total length  ~ site + width*site",
                   "total length  ~ site + width*site + blade position*site" )

###====Crab data T test======

# weight change 
crab_df <- data.frame(low.flow = c(2.68,5.7,8.51,2.25,2.97,9.06,16.91,1.508,4.38,3.18,3.47),
      hi.flow = c(19.22,7.43,30.66, 7.47, 13.29, 23.8,  3.15, 9.14, 26.22,3.29,11.6))
crab_df$diff <- low.flow-hi.flow

# are differences between pairs normally dist?
qqnorm(crab_df$diff)
qqline(crab_df$diff) # Yes! 

t.test(low.flow, hi.flow, paired = T)
# t = -2.728, df = 10, p-value = 0.02127

boxplot(data.frame("PC" = low.flow, "TR" = hi.flow), ylab="Mass Change (grams)", xlab="Site", 
        col=viridis::magma(2, begin=0.9, end=1))
 

####=====Plots=======

# Fig 2. Thickness (blade area blade length)
par(mfrow=c(1,2))
boxplot(thick ~ center_margin, data=thickness, ylab="Thickness (micrometers)", xlab="Blade Area", 
         col=viridis::viridis(2, begin=0.5, end=0.8), names=c("Center","Margin"))
boxplot(thick ~ Blade_position, data=thickness, ylab="Thickness (micrometers)", xlab="Blade Position", 
        col=viridis::magma(2, begin=0.5, end=0.7), names=c("Lateral","Medial"))


# Fig 3. Undulation between site
par(mfrow=c(1,2))
boxplot(r ~ Site, data=merged, xlab="Undulation Score", ylab="Site", 
        col=viridis::magma(2, begin=0.6, end=0.8), horizontal=TRUE )

# Fig 4. width between site
boxplot(w ~ Site, data=merged, xlab="Width (centimeters)", ylab="", 
        col=viridis::magma(2, begin=0.6, end=0.8), ylim=c(0,10), 
        horizontal=TRUE)






#####====T Tests Below! =======

####===== Q1: is ruffle correlated with site?======= 

# Ruffles distribution:  Pretty normal
qqnorm(merged[which(merged$Site == "PC"),3])# PC
qqline(merged[which(merged$Site == "PC"),3])
qqnorm(merged[which(merged$Site == "TR"),3]) # TR
qqline(merged[which(merged$Site == "TR"),3])

# variances are  significantly different.. yikes 
bartlett.test(r ~ Site, data = merged)

t.test(merged[which(merged$Site == "PC"),3], merged[which(merged$Site == "TR"),3], paired = F)
# t = 1.6108, df = 4.3223, p-value = 0.1772, ruffle not determined by site
 
####==== Q2: Is ruffle correlated with Blade position?====

qqnorm(merged[which(merged$Blade_position == "M"),3])
qqline(merged[which(merged$Blade_position == "M"),3])
qqnorm(merged[which(merged$Blade_position == "L"),3]) 
qqline(merged[which(merged$Blade_position == "L"),3])

bartlett.test(r ~ Blade_position, data = merged)
# variance is not significantly different, good 

t.test(merged[which(merged$Blade_position == "M"),3], merged[which(merged$Blade_position == "L"),3], paired = F)
# t = -0.47634, df = 9.9832, p-value = 0.6441

####====Q3: is width correlated with site? ======

qqnorm(merged[which(merged$Site == "PC"),2]) # pretty normal
qqline(merged[which(merged$Site == "PC"),2])
qqnorm(merged[which(merged$Site == "TR"),2]) 
qqline(merged[which(merged$Site == "TR"),2])

bartlett.test(w ~ Site, data = merged) # variance is not significantly different, good 
t.test(merged[which(merged$Site == "PC"),3], merged[which(merged$Site == "TR"),3], paired = F)
# t = 1.6108, df = 4.3223, p-value = 0.1772, insignificant


####=======Q4: is width correlated with blade position?======

qqnorm(merged[which(merged$Blade_position == "M"),2]) # pretty normal
qqline(merged[which(merged$Blade_position == "M"),2])
qqnorm(merged[which(merged$Blade_position == "L"),2]) 
qqline(merged[which(merged$Blade_position == "L"),2])

bartlett.test(w ~ Blade_position, data = merged) # variance is not significantly different, good 
t.test(merged[which(merged$Blade_position == "M"),3], merged[which(merged$Blade_position == "L"),3], paired = F)
# t = -0.47634, df = 9.9832, p-value = 0.6441, insignificant


####=====Q5: is thickness correlated with site?=====

qqnorm(full[which(full$Site == "PC"),8]) # pretty normal
qqline(full[which(full$Site == "PC"),8])
qqnorm(full[which(full$Site == "TR"),8]) 
qqline(full[which(full$Site == "TR"),8])

bartlett.test(thick ~ Site, data = full) # variance is not significantly different, good 
t.test(full[which(full$Site == "PC"),8], full[which(full$Site == "TR"),8], paired = F)
# t = -1.587, df = 137.26, p-value = 0.1148, insignificant



####=====Q6: is thickness correlated with blade position? SIG but unequal variance==== 

qqnorm(full[which(full$Blade_position == "M"),8]) # pretty normal
qqline(full[which(full$Blade_position == "M"),8])
qqnorm(full[which(full$Blade_position == "L"),8]) 
qqline(full[which(full$Blade_position == "L"),8])

bartlett.test(thick ~ Blade_position, data = full) # variance is  significantly different, BAD 
t.test(full[which(full$Blade_position == "L"),8], full[which(full$Blade_position == "M"),8],paired = F)
# t = -2.9033, df = 265.99, p-value = 0.004003
####======Q7: is thickness correlated with blade area?SIG but unequal variance======
 
 qqnorm(full[which(full$center_margin == "center"),8]) # pretty normal
 qqline(full[which(full$center_margin == "center"),8])
 qqnorm(full[which(full$center_margin == "mean_margin"),8]) 
 qqline(full[which(full$center_margin == "mean_margin"),8])
 
 bartlett.test(thick ~ center_margin, data = full) # variance is  significantly different, BAD 
 t.test(full[which(full$center_margin == "mean_margin"),8], full[which(full$center_margin == "center"),8],paired = F)
 # t = -9.6399, df = 252.9, p-value < 2.2e-16
 
 
####====Q8: is thickness correlated with ruffle?======
 summary(lm(thick ~ r , full))
 # Adjusted R-squared:  0.002393 
 # Estimate Std. Error t value Pr(>|t|)    
   #r             -132.8      103.5  -1.283    0.201   


 
 
 
 
 
 
 
 
 