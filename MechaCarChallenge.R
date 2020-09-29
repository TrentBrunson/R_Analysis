# Objectives
# Design and interpret a multiple linear regression analysis to identify variables of interest.
# Calculate summary statistics for quantitative variables.
# Perform a t-test in R and provide interpretation of results.
# Design your own statistical study to compare vehicle performance of two vehicles.

# Assumptions
# Multiple metrics such as vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance 
# were collected for each vehicle.
# suspension coil test results from multiple production lots; weight capacity of multiple suspension coils were tested 
# to determine if the manufacturing process is consistent across lots.
library(tidyverse)

MechaCar_MPG_Table <- read.csv('MechaCar_mpg.csv',check.names=T)

head(MechaCar_MPG_Table) #check out data in table

# use multilinear regression model to check out probability values
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCar_MPG_Table)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data = MechaCar_MPG_Table))

# Import suspension coil data
Suspension_Coil_Table <- read.csv('Suspension_Coil.csv')
head(Suspension_Coil_Table)

# summarize suspension coil stat data
Suspension_Summary <- Suspension_Coil_Table %>% summarize(Mean= mean(PSI), Median= median(PSI), Variance= var(PSI), Std_Dev= sd(PSI))
Suspension_Summary

# determine if the suspension coil's pound-per-inch results are 
# statistically different from the mean population results of 1,500 pounds per inch
# group suspension coil data by lot
Suspension_by_Lot <- Suspension_Coil_Table %>% group_by(Manufacturing_Lot) %>% 
  summarise(Mean= mean(PSI), Median= median(PSI), Variance= var(PSI), Std_Dev= sd(PSI))
Suspension_by_Lot

# Run t-test to see if this data set differs from the populations mean of 1500 ppi
t.test(Suspension_Coil_Table$PSI, mu = 1500)
