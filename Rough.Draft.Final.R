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
names(climate.data.cultus)[2] <- "mean_annual_precip"
names(climate.data.cultus)[3] <- "mean_annual_temp"
names(climate.data.cultus)[4] <- "mean_max_temp"

# renaming variables in salmon dataset 
names(AA.Cultus)[5] <- "Average_Abundance_Run_Size"

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
pdf(file = "d.fp.figures/Abundace Through Time.pdf")

plot(merged.data$year, merged.data$Average_Abundance_Run_Size,
     col ="red",
     xlab = "Year",
     ylab = "Average Abundance & Run Size",
     main = "Cultus Abundance through time",
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$year), col = "red"))
dev.off()

# Why isn't this function work? (write pdf)
# write_pdf(merged.data, paste(p.fp.figures,  "Cultus_Abundace_Through_Time"), row.names = FALSE)
 


# making abundance it in ggplot 
ggplot(data = merged.data, aes(x = year, y = average_abundace)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, colour = "red", ylab('Average Abundance'))
# why isnt this workin? 



# plot of temp against precip 

plot(merged.data$mean_annual_temp, merged.data$mean_annual_precip,
     col ="blue",
     xlab = "Average Annual temp (C)",)


# plot of precip through time 
plot(merged.data$year, merged.data$mean_annual_precip,
     type = "l",
     col ="blue",
     xlab = "Year",
     ylab = "Average precipitation (mm)",
     main = "Precipitation Through Time",
    abline(lm(merged.data$mean_annual_precip ~ merged.data$year)))


# plot of abundance against precip 
plot(merged.data$mean_annual_precip, merged.data$Average_Abundance_Run_Size,
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$mean_annual_precip)),
     col ="blue",
     xlab = "Average Annual Precip (mm)",
     ylab = "Average Abundance")


# plot of abundance against temperature  
plot(merged.data$mean_annual_temp, merged.data$Average_Abundance_Run_Size,
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$mean_annual_temp)),
     col ="blue",
     xlab = "Average Annual temp (C)",
     ylab = "Average Abundance")

# creating a merged plot 

# precip and abundance 
ggplot(merged.data, aes(x = year)) + 
geom_line(aes(y = log(mean_annual_precip), color = 'average precip')) + 
  geom_line(aes(y = log(log(Average_Abundance_Run_Size)), color = 'average abundace'))

#temp and precip 
ggplot(merged.data, aes(x = year)) + 
  geom_line(aes(y = log(mean_annual_precip), color = 'average precip')) + 
  geom_line(aes(y = log(mean_annual_temp), color = 'average temp'))

#temp and abundance 

pdf(file = "d.fp.figures/Temp and Abundance.pdf")

ggplot(merged.data, aes(x = year)) + 
geom_line(aes(y = (mean_annual_temp), color = 'average temp')) + 
  geom_line(aes(y = log(Average_Abundance_Run_Size), color = 'average abundance'))

dev.off()



# Statistical Analysis  ---------------------------------------------------

# test a correlation between temp and abundance 

#H0: No correlation between temp and abundance 
#HA: Correlation between temp and abundance 

cor.test(merged.data$mean_annual_temp, merged.data$Average_Abundance_Run_Size)

# correlation test between temp and precip
cor.test(merged.data$mean_annual_temp, merged.data$mean_annual_precip)

# correlation test between precip and abundance
cor.test(merged.data$mean_annual_temp, merged.data$Average_Abundance_Run_Size)






# one way ANOVA for abundance and precip  
one.way.anova <- aov(Average_Abundance_Run_Size ~ mean.annual.precip, data = merged.data)
summary(one.way.anova)

#                       Df   Sum Sq   Mean Sq F value Pr(>F)
# mean_yearly_precip  1 1.24e+08 124045731   1.123  0.296
# Residuals          39 4.31e+09 110505209               
# 4 observations deleted due to missingness

# there is not a statistically significant relationship between average yearly precipitation and abundance in
# Cultus Lake Sockeye 


# one way ANOVA with abundance and temp 
one.way.anova <- aov(Average_Abundance_Run_Size ~ mean_annual_temp, data = merged.data)
summary(one.way.anova)

#                    Df    Sum Sq   Mean Sq F value Pr(>F)
# mean_annual_temp  1 6.388e+06   6387537   0.056  0.814
# Residuals        39 4.427e+09 113522086               
# 4 observations deleted due to missingness

# there is not a statistically significant relationship between average yearly temperature and abundance in
# Cultus Lake Sockeye 

