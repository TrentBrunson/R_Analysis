library(jsonlite)
library(tidyverse)
demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
demo_table2 <- fromJSON(txt = 'demo.json')
filter_table <- demo_table2[demo_table2$price > 10000,]
filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) #filter by price, drivetrain and clean title

# three steps:

# Create a numerical vector that is the same length as the number of rows in the data frame using the colon (:) operator.
# Use the sample() function to sample a list of in dices from our first vector.
# Use bracket notation to retrieve data frame rows from sample list.

demo_table[sample(1:nrow(demo_table), 3),]

demo_table <- demo_table %>% mutate(Mileage_per_Year = Total_Miles/(2020-Year), IsActive = TRUE) #add columns to original data frame
                                    
                                    