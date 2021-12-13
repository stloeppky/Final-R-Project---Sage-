### Final Project ###
# packages 
library(ggplot2)
library(dplyr)
#working directory 
wd <- getwd()

# create workflow 
folder.names <- c("a.fp.data.raw","b.fp.data.clean", "c.fp.results","d.fp.figures", "e.fp.rscripts", "f.fp.cvs")

# workflow? ----------------------------------------------------------------


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


# reading in the first of the Salmon Data - Cultus lake average abundance through time
AA.Cultus <- read.csv("Average_Abundance:Run_Size_Cultus_Lake.csv")

#find range in years 
range(AA.Cultus$year) # 1950 - 2019

#rename Variables 
names(AA.Cultus)[5] <- "Average_Abundance_Run_Size"
names(AA.Cultus)[4] <- "year"

# reading in the climate data - Cultus Lake 
Climate.Data.Cultus <- read.csv("Monthly.Climate.Data.Cultus.Lake.csv")

# find range in years 
range(Climate.Data.Cultus$Year) # 1931 - 1994 

# making a dataset with just the variables I want for the climate data
climate.data.cultus <- Climate.Data.Cultus %>% 
  #arrange(-year) %>% 
  select(Station.Name, Year, Total.Precip..mm., Mean.Temp...C.)




# Making a plot with Salmon pop values through time 
plot(AA.Cultus$year, AA.Cultus$Average_Abundance_Run_Size,
     ylab = "Average Spawner Abundace/Run Size",
     xlab = "Year",
     main = "Cultus Lake",
     type = "l",
     col = "red",
     xlim = c(1950, 1994),
     abline(lm(AA.Cultus$year ~ AA.Cultus$Average_Abundance_Run_Size), col = "blue"))
     # WHYYYY is this line straight across the bottom 
     

# trying in GGPlot 

library(ggplot2)


ggplot(data = AA.Cultus, aes(x = year, y = Average_Abundance_Run_Size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red")
       y = "Average Abundance"

# okay that worked. Cant figure out how to make a rolling average or change the colour of the trendline but those 
# are small potatoes 
# now the labels aren't working for some reason. UGHHHHHH

# now to sort through some data 
# omitting na from total precip. maybe?
na.omitted <- na.omit(climate.data.cultus$Total.Precip..mm.)

# trying to make the columns that I want? JUST precip 
cultus.precip.data <- climate.data.cultus %>%
  select(Year, Total.Precip..mm.)


# omitting na from precip data 
na.omit(cultus.precip.data)
# plotting the rainfall 
plot(cultus.precip.data$Year, cultus.precip.data$Total.Precip..mm.) #hmm not super useful. And missing data in the middle? 
 
# Now I want to combine the months in the year to create an annual average
str(cultus.precip.data) # just looking at the data 
 
# creating the yearly mean with the monthly data 
mean.annual.precip <- aggregate(cultus.precip.data$Total.Precip..mm., list(cultus.precip.data$Year), mean, na.rm = TRUE)

# renaming the variables 
names(mean.annual.precip)[1] <- "year"
names(mean.annual.precip)[2] <- "mean_yearly_precip"

#creating a yearly mean with the monthly data for temperature 
mean.annual.temp <- aggregate(climate.data.cultus$Mean.Temp...C., list(climate.data.cultus$Year), mean, na.rm = TRUE)

# renaming the variables 
names(mean.annual.temp)[1] <- "Year"
names(mean.annual.temp)[2] <- "Mean.Temp"


 
# also, is max more relevant, and is standard deviation super relevant? Things to look at later?

# write to CSV file 
write.csv(mean.annual.precip, paste(p.fp.csv, "c.mean.annual.precip.csv"), row.names = FALSE)
write.csv(mean.annual.temp, paste(p.fp.csv,  "c.mean.annual.temp.csv"), row.names = FALSE)


# make a plot with 

# trying to make a plot in base R 
plot(mean.annual.precip$year, mean.annual.precip$mean_yearly_precip,
  abline(lm(mean.annual.precip$year ~ mean.annual.precip$mean_yearly_precip)),
  type = "l",
  col ="blue",
  xlim = c(1950, 1994))
 

# Trying to make a plot in GG plot DID NOT WORK 
ggplot(data = mean.annual.precip, aes(x = year, y = x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "blue")


# creating a dataframe with two different sets of data? 

# year <- mean.annual.precip$Group.1
# precip <- mean.annual.precip$x
# average_abundace <- AA.Cultus$Average_Abundance_Run_Size

# data.frame(year, precip, average_abundace)
# okay I need to figure out how to cut off some of the years and also deal with the fact that 
# the axis are so different 



library(dplyr)

# merging data (1950-1994) 


filtered.data.precip <- mean.annual.precip %>%
  filter(mean.annual.precip$year > 1949 & mean.annual.precip$year < 1995)

filtered.data.abundance <- AA.Cultus %>%
  filter(AA.Cultus$year > 1949 & AA.Cultus$year < 1995)

# using the merge function to create a data set that kicks out the values that do not match up 
merged.data <- merge(filtered.data.precip, filtered.data.abundance, by = c("year", "year"))

# graph merged data 

ggplot(merged.data, aes(x = year, y = c(log(mean_yearly_precip), log(Average_Abundance_Run_Size)), type = 'l'))
# this one ^ doesnt work?

# ggplot both variables 
merged.data.plot <- ggplot(merged.data, aes(x = year)) + 
  geom_line(aes(y = log(mean_yearly_precip), color = 'average precip')) + 
  geom_line(aes(y = log(log(Average_Abundance_Run_Size)), color = 'average abundace'))


ggplot(merged.data, aes(x = year, y = Average_Abundance_Run_Size, size = mean_yearly_precip)) +
         geom_point(alpha = 0.5)

ggplot(merged.data, aes(x = year, y = Average_Abundance_Run_Size, size = mean_yearly_precip)) +
  geom_point(alpha = 0.5)



# Statistical Analysis ----------------------------------------------------

# correlation test between precip and abundance 

cor(filtered.data.precip, filtered.data.abundance)

# one way ANOVA 
one.way.anova <- aov(Average_Abundance_Run_Size ~ mean_yearly_precip, data = merged.data)
summary(one.way.anova)


