### statistical analysis ###
# this provides statistical analysis of the data. 
# the correlations are not used (commented out)
# was unable to figure out how to save these to the results folder but would have done that if I had figured 
# it out

# Descriptive Statistics Cultus   -------------------------------------------------


# descrptive statistics for Mean cultus climate data 
mean.cultus.precip <- mean(merged.data$mean_annual_precip, na.rm=TRUE) # = 123.2929
mean.cultus.temp <- mean(merged.data$mean_annual_temp, na.rm=TRUE) # = 9.8936 
sd.cultus.precip <- sd(merged.data$mean_annual_precip, na.rm=TRUE) # = 22.33
sd.cultus.temp <- sd(merged.data$mean_annual_temp, na.rm=TRUE) # = 0.85656 

# descrptive statistics for Mean cultus abundance data 
mean.cultus.abundance <- mean(merged.data$Average_Abundance_Run_Size, na.rm=TRUE) # = 13559.44
sd.cultus.abundance <- sd(merged.data$Average_Abundance_Run_Size, na.rm=TRUE) # = 11396.35 

mean.skeena.temp <- mean(merged.data.skeena$Mean.Temp...C., na.rm=TRUE) # 2.494365


# Linear Regression  ------------------------------------------------------

# linear regression of abundance due to temperature (CULTUS)


lm.cultus.abundance.temp <- lm(Average_Abundance_Run_Size ~ mean_annual_temp, data = merged.data)
summary(lm.cultus.abundance.temp)


# linear regression of abundance due to precip (CULTUS)
lm.cultus.abundance.precip <- lm(Average_Abundance_Run_Size ~ mean_annual_precip, data = merged.data)
summary(lm.cultus.abundance.precip)

# linear regression of abundance due to precip (Skeena)
lm.skeena.abundance.precip <- lm(Spawner.Abundace...Run.Size ~ Total.Precip..mm., data = merged.data.skeena)

summary(lm.skeena.abundance.precip)

# linear regression of abundance due to temperature (Skeena)
lm.skeena.abundance.temp <- lm(Spawner.Abundace...Run.Size ~ Mean.Temp...C., data = merged.data.skeena)

summary(lm.skeena.abundance.temp)











# below are tests that I ultimately did not use


# Correlation between temperature and precip Cultus -----------------------
# cor(merged.data$mean_annual_precip, merged.data$mean_annual_temp, method="pearson",  use = "complete.obs")
# cor.test(merged.data$mean_annual_precip, merged.data$mean_annual_temp, method="pearson",  use = "complete.obs")
# t = -0.41973, 
# df = 39, 
# p-value = 0.677
# 95 percent confidence interval:
#  -0.3671357  0.2456591
# cor  = -0.06705955 

# Correlation Mean Temp and Abundance Cultus   -------------------------------------


# cor(merged.data$Average_Abundance_Run_Size, merged.data$mean_annual_temp, method="pearson",  use = "complete.obs")
# cor.test(merged.data$Average_Abundance_Run_Size, merged.data$mean_annual_temp, method="pearson",  use = "complete.obs")

# correlation coefficient = -0.03795606 
# t = -0.23721 
# P- Value = 0.8137 
# 95 percent confidence interval:
# -0.3416174  0.2728809



# Correlation Precip and Abundance Cultus  ----------------------------------------


# correlation test 
# cor(merged.data$Average_Abundance_Run_Size, merged.data$mean_annual_precip, method="pearson",  use = "complete.obs")
# cor.test(merged.data$Average_Abundance_Run_Size, merged.data$mean_annual_precip, method="pearson",  use = "complete.obs")

# correlation coefficient = -1.0595
# t = -1.0595
# P- Value = 0.8137 
# 95 percent confidence interval:
# -0.4516728  0.1480014
