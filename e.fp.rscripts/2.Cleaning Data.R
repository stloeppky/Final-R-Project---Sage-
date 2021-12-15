# reading in the data 

# in this script the data is read from CSV files downloaded from the databases, cleaned, and merged into 
# manageable datasets
# the code is separated into the Fraser watershed, and Skeena watershed data cleaning 
# final datasets are written to CSV file 
# I also  wanted to load the initial datasets into the "raw data" folder but when I did that I was 
# not able to read them into the script so I left them in the general folder 

# Fraser Watershed Data ---------------------------------------------------


# Cultus lake average abundance through time
AA.Cultus <- read.csv("Average_Abundance:Run_Size_Cultus_Lake.csv")

# Cultus Lake climate data 
Climate.Data.Cultus <- read.csv("Monthly.Climate.Data.Cultus.Lake.csv")


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

# write merged data to CSV
write.csv(merged.data,"f.fp.cvs\\merged.data.fraser.csv", row.names = FALSE)


# Morice Lake/Wistaria Data - Skeener Watershed ---------------------------


# Reading in Wistaria climate data 
climate.data.wistaria <- read.csv("climate.data.wistaria.csv")

# reading in morice abundance data 
AA.Morice <- read.csv("AA.Morice.csv")


# making a data set with just the variables I want for the climate data
climate.wistaria <- climate.data.wistaria %>% 
  #arrange(-year) %>% 
  select(Year, Total.Precip..mm., Mean.Temp...C., Mean.Max.Temp...C.)


# creating the yearly mean with the monthly data 
annual.climate.wistaria <- aggregate(climate.wistaria[, 2:4], list(climate.wistaria$Year), mean, na.rm = TRUE)
# renaming 
names(annual.climate.wistaria)[1] <- "year"

#finding range for Wistaria Climate 
range(annual.climate.wistaria$year)
# 1926 - 2004 

#finding range for Morice lake data 
range(AA.Morice$year)
# 1950 - 2020


# filtering years out so that the years match up 

filtered.data.wistaria <- annual.climate.wistaria %>%
  filter(annual.climate.wistaria$year > 1949 & annual.climate.wistaria$year < 2003)

filtered.data.morice <- AA.Morice %>%
  filter(AA.Morice$year > 1949 & AA.Morice$year < 2003)


merged.data.skeena <- merge(filtered.data.wistaria, filtered.data.morice, by = c("year", "year"))

# write CSV

write.csv(merged.data.skeena, paste(p.fp.data.clean,  "Merged Data Skeena.csv"), row.names = FALSE)





