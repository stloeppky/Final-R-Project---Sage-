### Figures  ### 
# In this section I create figures with data from the data cleaning script
# This scrpt is also seperated into graphs made from Fraser data, Skeena data, and both

# Fraser Data  -----------------------------------------------------------------

Data

# plot of abundance over time Cultus 

pdf(file = "d.fp.figures/plot.abundance.fraser1.pdf")

plot(merged.data$year, merged.data$Average_Abundance_Run_Size,
     col = "black",
     xlab = "Year",
     ylab = "Average Abundance & Run Size",
     main = "Cultus Abundance through time",
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$year), col = "red"))
dev.off()


# making abundance it in ggplot 
# ggplot(data = merged.data, aes(x = year, y = average_abundace)) +
# geom_point() + 
#  geom_smooth(method = "lm", se = FALSE, colour = "red", ylab('Average Abundance'))
# why isnt this workin? 



# plot of temp against precip 

pdf(file = "d.fp.figures/temp.precip.fraser.pdf")
pdf(file = "d.fp.figures/pl")
plot(merged.data$mean_annual_temp, merged.data$mean_annual_precip,
     col ="blue",
     xlab = "Average Annual temp (C)",)
dev.off()


# plot of precip through time 
pdf(file = "d.fp.figures/precip.fraser.pdf")
plot(merged.data$year, merged.data$mean_annual_precip,
     col ="blue",
     xlab = "Year",
     ylab = "Mean precipitation (mm)",
     abline(lm(merged.data$mean_annual_precip ~ merged.data$year)))
dev.off()

# plot of abundance against precip 
pdf(file = "d.fp.figures/precip.abundance.fraser.pdf")
plot(merged.data$mean_annual_precip, merged.data$Average_Abundance_Run_Size,
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$mean_annual_precip)),
     col ="blue",
     xlab = "Average Annual Precip (mm)",
     ylab = "Average Abundance")
dev.off()

# plot of abundance against temperature 
pdf(file = "d.fp.figures/abundance.temp.fraser.pdf")
plot(merged.data$mean_annual_temp, merged.data$Average_Abundance_Run_Size,
     abline(lm(merged.data$Average_Abundance_Run_Size ~ merged.data$mean_annual_temp)),
     col ="blue",
     ylim = c(0, 40000),
     xlab = "Average Annual temp (C)",
     ylab = "Average Abundance")
dev.off()


# average temp through time 
pdf(file = "d.fp.figures/temp.fraser.pdf")
plot(merged.data$year, merged.data$mean_annual_temp,
     abline(lm(merged.data$mean_annual_temp ~ merged.data$year), col = "blue"),
     xlab = "year",
     ylab = "Mean annual temp")
legend(x= 1950, y= 12.1,c("Fraser"),cex= 1,col=c("blue"),pch=c(1))
dev.off()

# below are figure attempts that I ended up not using 


# creating a merged plot 
# precip and abundance 
# ggplot(merged.data, aes(x = year)) + 
# geom_line(aes(y = log(mean_annual_precip), color = 'average precip')) + 
#  geom_line(aes(y = log(log(Average_Abundance_Run_Size)), color = 'average abundace'))

#temp and precip 
# ggplot(merged.data, aes(x = year)) + 
#  geom_line(aes(y = log(mean_annual_precip), color = 'average precip')) + 
#  geom_line(aes(y = log(mean_annual_temp), color = 'average temp'))

#temp and abundance 

# ggplot(merged.data, aes(x = year)) + 
#  geom_line(aes(y = (mean_annual_temp), color = 'average temp')) + 
#  geom_line(aes(y = log(Average_Abundance_Run_Size), color = 'average abundance'))





# mean max temp through time - 

# plot(merged.data$year, merged.data$mean_max_temp,
#      abline(lm(merged.data$mean_max_temp ~ merged.data$year)),
#      xlab = "year",
#     ylab = "Mean Max temp",
#     main = 'Mean Max Temp Through Time')





# Skeena Data  ------------------------------------------------------------

# abundance through time 
pdf(file = "d.fp.figures/abundance.skeena.pdf")
plot(merged.data.skeena$year, merged.data.skeena$Spawner.Abundace...Run.Size,
     col = "black",
     xlab = "Year",
     ylab = "Average Abundance & Run Size",
     main = "Morice Abundance through time",
     abline(lm(merged.data.skeena$Spawner.Abundace...Run.Size ~ merged.data.skeena$year), col = "red"))
dev.off()

# boxplot for abundance 
pdf(file = "d.fp.figures/abundance.skeea.boxplot.pdf")
boxplot(merged.data.skeena$Spawner.Abundace...Run.Size)
hist(merged.data.skeena$Spawner.Abundace...Run.Size)
dev.off()

#precip through time 

pdf(file = "d.fp.figures/precip.skeena.pdf")
plot(merged.data.skeena$year, merged.data.skeena$Total.Precip..mm.,
     col = "red",
     xlab = "Year",
     ylab = "Mean Precipitation (mm)",
     main = "Morice Precip through time",
     abline(lm(merged.data.skeena$Total.Precip..mm. ~ merged.data.skeena$year)))
dev.off()


# temp through time 
pdf(file = "d.fp.figures/temp.skeena.pdf")
plot(merged.data.skeena$year, merged.data.skeena$Mean.Temp...C.,
     col = "black",
     xlab = "Year",
     ylab = "Mean Temp",
     abline(lm(merged.data.skeena$Mean.Temp...C. ~ merged.data.skeena$year), col = "red"))

legend(x= 1950, y= 4.25,c("Skeena"),cex= 1,col=c("red"),pch=c(1))

dev.off()



# Plots with data from both data series  ----------------------------------

# abundance through time 

pdf(file = "d.fp.figures/merged.abundance.pdf")
Plot.Skeena.Fraser.A <- plot(merged.data.skeena$year, merged.data.skeena$Spawner.Abundace...Run.Size,
     col = "red",
     xlab = "Year",
     ylab = "Spawners",
     points(merged.data$year, merged.data$Average_Abundance_Run_Size, 
            col = "blue",
     abline(lm(merged.data.skeena$Spawner.Abundace...Run.Size ~ merged.data.skeena$year), 
            col = "red"),
     abline(lm(merged.data$Average_Abundance_Run_Size~ merged.data$year), col = "blue")))
legend(x= 1990, y= 56000,c("Fraser","Skeena"),cex= 1,col=c("blue","red"),pch=c(1,1))
dev.off()
  

# Temperature through time for both places 

pdf(file = "d.fp.figures/merged.temp.pdf")
plot(merged.data.skeena$year, merged.data.skeena$Mean.Temp...C.,
     col = "red",
     xlab = "Year",
     ylim = c(0, 14),
     xlim = c(1950, 2004),
     ylab = "Mean Temperature (C)",
     points(merged.data$year, merged.data$mean_annual_temp, 
            col = "blue"))
legend(x= 1950, y= 14,c("Fraser","Skeena"),cex= 1,col=c("blue","red"),pch=c(1,1))
dev.off()


# precip through time 
pdf(file = "d.fp.figures/merged.precip.pdf")
plot(merged.data.skeena$year, merged.data.skeena$Total.Precip..mm.,
     col = "red",
     xlab = "Year",
     ylim = c(0, 200),
     xlim = c(1950, 1994),
     ylab = "Mean Precip (mm)",
     points(merged.data$year, merged.data$mean_annual_precip, 
            col = "blue"))
legend(x= 1950, y= 200,c("Fraser","Skeena"),cex= 1,col=c("blue","red"),pch=c(1,1))
dev.off()


  