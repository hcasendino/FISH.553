#### The first section of your R script: Meta info ####
# Keep track of your code and its
# intended purpose.

### Example ###
# This is code to replicate the analyses and figures from my 2014 Science
# paper. Code developed by Sarah Supp, Tracy Teal, and Jon Borelli

#### 2nd section of R Script: Data, dependencies, requirements ####
# Be explicit about the requirements and dependencies of your code

### Example of dependencies ###
library(ggplot2)
library(reshape)
library(vegan)

### Example of other requirements/improving reproducibility ###

## BEST PRACTICE EXAMPLE ##

input_file <- "data/data.csv" 
output_file <- "data/results.csv"

# read input
input_data <- read.csv(input_file)
# get number of samples in data
sample_number <- nrow(input_data)
# generate results
results <- some_other_function(input_file, sample_number)
# write results
write.table(results, output_file)


## LESS IDEAL EXAMPLE ##
# check
input_data <- read.csv("data/data.csv")
# get number of samples in data
sample_number <- nrow(input_data)
# generate results
results <- some_other_function("data/data.csv", sample_number)
# write results
write.table("data/results.csv", output_file)

### If you are going to write your own functions, put them up here, too! ###
### Basically, any global variables you want defined in your workspace go here ###


#### Overview of best practices ####
# Start each program with a description of what it does.
# 
# Then load all required packages.
# 
# Consider what working directory you are in when sourcing a script.
# 
# Use comments to mark off sections of code.
# 
# Put function definitions at the top of your file, or in a separate file if there are many.
# 
# Name and style code consistently.
# 
# Break code into small, discrete pieces.
# 
# Factor out common operations rather than repeating them.
# 
# Keep all of the source files for a project in one directory and use relative paths to access them.
# 
# Keep track of the memory used by your program.
# 
# Always start with a clean environment instead of saving the workspace.
# 
# Keep track of session information in your project folder.
# 
# Have someone else review your code.
# 
# Use version control.