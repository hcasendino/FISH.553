#how to do things

## Make final table
formattable(avg_abundance, list('Species' = formatter(
  "span",style = ~ style(color = "grey",font.style = "italic"))))


# Making Port Madison Map with embedded WA map (NOT USING)

# edit zoomed out map 
bounds<-c(left=-125 , bottom=45 , right=-116 , top=49.4)
embedMap <- get_stamenmap(bounds, zoom=6, maptype = "toner-lite") %>% ggmap()+
  geom_point(aes(x=-122.5472, y=47.73836), colour="red", size=2.5) + 
  xlab("Longitude (dec)")+
  ylab("Latitude (dec)")+
  geom_label(aes(label="Port Madison", fontface = "bold", x=-120.5, y=48), size=3, colour="red") + 
  theme(legend.position = "none") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"))


combinedMap <- PortMap + 
  inset(ggplotGrob(embedMap), xmin = -122.538, xmax = -122.52, ymin = 47.731, ymax = 47.739)
### Melt all species columns into one column with corresponding densities for modelling. 
#data<- data %>% pivot_longer(-c(year,depth,time,distance,trawlarea,total_catch,total_density,prepost), names_to = "species_ID", values_to = "sp_number")
#funct <- function(x, na.rm=FALSE) (x/data$trawlarea)
#data <- data %>% mutate_at(c("sp_number"), funct) %>% rename(sp_density = sp_number)




# How to tell if rows are identical 
identicalRows <- function(data){
  for(i in 1:length(data$col)){
    if(identical(data[[i,3]],data[[i,4]]) == TRUE && identical(data[[i,4]],data[[i,5]]) == TRUE){
      print(c(i,"IDENTICAL"))
    }
  }
}


# Gamma NLL
alpha_vec <- seq(0.14,0.25,0.01)
theta_vec <- seq(890,900,1)

GammaNLL <- function(pars, data){
  alpha <- pars[[1]]
  theta <- pars[[2]]
  return (-sum(dgamma(data, shape = alpha, scale = theta, log = TRUE)))
}

Vec<- matrix(NA,nrow=length(alpha_vec), ncol = length(theta_vec))

for(j in 1:length(theta_vec)){
  theta<- theta_vec[j]
  
  for(i in 1:length(alpha_vec)){
    alpha <- alpha_vec[i]
    fit<- GammaNLL(c(alpha,theta), unnested_trip_c.t$"1_2")
    Vec[i,j] <- fit
  }
  print(j)
}

MLEnll <- min(Vec)
which(Vec ==MLEnll, arr.ind = T) 



