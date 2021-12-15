### Final Project ###
# this section includes the packages used, the working directory and the workflow 

#Scripts should be looked at in this order 
# 1) Main
# 2) Cleaning Data 
# 3) Figures
# 4) Statistics 




# Packages ----------------------------------------------------------------

library(dplyr)
#working directory 
wd <- getwd()
setwd("~/Desktop/Data Analysis in R/Final R Project")

# workflow ----------------------------------------------------------------

folder.names <- c("a.fp.data.raw","b.fp.data.clean", "c.fp.results","d.fp.figures", "e.fp.rscripts", "f.fp.csv")

# create folders if they don't exit yet. 
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# paths to the folders. The 'p.' indicates the variable is a path.
# make sure the variable names describe the folder.names
p.fp.data.raw <- paste(wd, "/", folder.names[1], "/", sep = "")
p.fp.data.clean <- paste(wd, "/", folder.names[2], "/", sep = "")
p.fp.results <- paste(wd, "/", folder.names[3], "/", sep = "")
p.fp.figures <- paste(wd, "/", folder.names[4], "/", sep = "")
p.fp.rscripts <- paste(wd, "/", folder.names[5], "/", sep = "")
p.fp.csv <- paste(wd, "/", folder.names[6], "/", sep = "")



# The project includes 

# data cleaning 
# Figures 
# statistical analysis 

wd
