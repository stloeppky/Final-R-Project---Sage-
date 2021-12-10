### Final Project ###

# Packages ----------------------------------------------------------------

library(ggplot2)
library(dplyr)
#working directory 
wd <- getwd()

# workflow ----------------------------------------------------------------

folder.names <- c("a.fp.data.raw","b.fp.data.clean", "c.fp.results","d.fp.figures", "e.fp.rscripts", "f.fp.cvs")

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

# Data exploration  -------------------------------------------------------

# reading in the data 

# Cultus lake average abundance through time
AA.Cultus <- read.csv("Average_Abundance:Run_Size_Cultus_Lake.csv")

# Cultus Lake climate data 
Climate.Data.Cultus <- read.csv("Monthly.Climate.Data.Cultus.Lake.csv")



# Data Cleaning -----------------------------------------------------------

# making a dataset with just the variables I want for the climate data
climate.data.cultus <- Climate.Data.Cultus %>% 
  #arrange(-year) %>% 
  select(Year, Total.Precip..mm., Mean.Temp...C., Mean.Max.Temp...C.)

# renaming variables in climate dataset 
names(climate.data.cultus)[1] <- "year"
names(climate.data.cultus)[2] <- "total_monthly_precip"
names(climate.data.cultus)[3] <- "mean_monthly_temp"
names(climate.data.cultus)[4] <- "mean.max.temp"

# making a dataset with just the variables I want for the salmon data 
abundance.data.cultus <- AA.Cultus %>% 
  #arrange(-year) %>% 
  select(species, year, Average_Abundance_Run_Size)

# creating the yearly mean with the monthly data 
annual.climate.data <- aggregate(climate.data.cultus, list(climate.data.cultus$year), mean, na.rm = TRUE)

# I have two rows of years but that's okay for now --- 



# filter out years that do not match 
filtered.data.climate <- annual.climate.data %>%
  filter(annual.climate.data$year > 1949 & annual.climate.data$year < 1995)

filtered.data.abundance <- abundance.data.cultus %>%
  filter(abundance.data.cultus$year > 1949 & abundance.data.cultus$year < 1995)

# merging data (1950-1994) 
# using the merge function to create a data set that kicks out the values that do not match up 
merged.data <- merge(filtered.data.climate, filtered.data.abundance, by = c("year", "year"))

# delete extraneous extra year column 
merged.data <- subset (merged.data, select = -Group.1)

# write to CSV 
write.csv(merged.data, paste(p.fp.data.clean,  "Merged_Data.csv"), row.names = FALSE)



# Making Graphs  ----------------------------------------------------------


# plot of abundance over time 
plot(merged.data$year, merged.data$Average_Abundance_Run_Size,
     type = "l",
     col ="red",
     xlab = "Year",
     ylab = "Average Abundance & Run Size",
     main = "Cultus Abundance through time")

# Why isn't this function work? (write pdf)
# write.pdf(merged.data, paste(p.fp.figures,  "Cultus_Abundace_Through_Time"), row.names = FALSE)

# making abundance it in ggplot 
ggplot(data = AA.Cultus, aes(x = year, y = Average_Abundance_Run_Size)) +
geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE, 
              colour = "red",
              ylab('Average Abundance'))

# 





