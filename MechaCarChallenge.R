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

MechaCar_MPG_Table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
View(MechaCar_Suspension_Table)
