####=================
# Assignment: Principles and Debugging Quiz
# Date Created: November 8, 2020
# Name: Helen Casendino; hcas1024@uw.edu
# Note: This explains a way to dynamically place labels onto your plot.
# Especially useful in multipanel plots where you want to place a bunch of labels quick. 
####=================


graph_function<-function(label,axis_percentile,...){
  
  # This lines gives extreme x values (x1, x2) and extreme y values
  plot_extremes<-par("usr")
  
  x_loc<- plot_extremes[1] + axis_percentile[1] * diff(plot_extremes[1:2])
  y_loc<- plot_extremes[3] + axis_percentile[2] * diff(plot_extremes[3:4])
  text(x = x_loc, y = y_loc, l = label,...)
  }

plot(1,1)
graph_function(label="(a)", axis_percentile=c(.05,.95))
 
