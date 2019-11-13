#############################################################################
# An R-script for reformatting and plotting our data sets from Slusher
# 
# Author:  Sean Calida
# Version: 11/13/2019
#############################################################################

# Load necessary packages 
library(ggplot2)   # For plotting
library(tidyverse) # For formatting
library(lubridate) # For datetime issues

# Read in CSV files
Floor1 <- read_csv("Floor1.csv", skip=2)
Floor4 <- read_csv("Floor4.csv", skip=2)
Floor7 <- read_csv("Floor7Logger.csv", skip=2)
Floor12 <- read_csv("Floor12Logger.csv", skip=2)

# Change column names for each data set (Date, Temperature, Light Intensity)
colnames(Floor1)[1:3] <- c("Date", "Temp_F", "Int_lmft2")
colnames(Floor4)[1:3] <- c("Date", "Temp_F", "Int_lmft2")
colnames(Floor7)[1:3] <- c("Date", "Temp_F", "Int_lmft2")
colnames(Floor12)[1:3] <- c("Date", "Temp_F", "Int_lmft2")

# Format Date column to Year-Month-Day format for each data set
Floor1$Date <- ymd_hms(Floor1$Date)
Floor4$Date <- ymd_hms(Floor4$Date)
Floor7$Date <- ymd_hms(Floor7$Date)
Floor12$Date <- ymd_hms(Floor12$Date)

# Remove all unnecessary columns for each data set
Floor1 <- Floor1[,1:3]
Floor4 <- Floor4[,1:3]
Floor7 <- Floor7[,1:3]
Floor12 <- Floor12[,1:3]

# Remove all rows where Temp is NA for each data set
Floor1 <- drop_na(Floor1, Temp_F)
Floor4 <- drop_na(Floor4, Temp_F)
Floor7 <- drop_na(Floor7, Temp_F)
Floor12 <- drop_na(Floor12, Temp_F)

# Match the datetime range for each data set (Oct 14 - Nov 4, 1pm)
Floor1 <- Floor1 %>% filter(Date >= "2019-10-14 00:00:00")
Floor1 <- Floor1 %>% filter(Date <= "2019-11-04 13:00:00")
Floor4 <- Floor4 %>% filter(Date >= "2019-10-14 00:00:00")
Floor4 <- Floor4 %>% filter(Date <= "2019-11-04 13:00:00")
Floor7 <- Floor7 %>% filter(Date >= "2019-10-14 00:00:00")
Floor7 <- Floor7 %>% filter(Date <= "2019-11-04 13:00:00")
Floor12 <- Floor12 %>% filter(Date >= "2019-10-14 00:00:00")
Floor12 <- Floor12 %>% filter(Date <= "2019-11-04 13:00:00")

# Combine all the data sets into one large data set
SlusherData <- bind_rows("Floor1" = Floor1, 
                    "Floor4" = Floor4, 
                    "Floor7" = Floor7, 
                    "Floor12" = Floor12, .id ="Level")

#############################################################################
# Plot Name:   Stacked_Plot
# Description: A plot showing a timeseries (Temperature vs Time) with each
#              floor on a different line.
#
# Author:  Sean Calida
# Version: 11/13/2019
#############################################################################
Stacked_Plot <- ggplot(data = SlusherData, aes(x = Date, y = Temp_F, 
                                              color = Level)) + 
  geom_line() + theme_minimal()

#############################################################################
# Plot Name:   Average_Plot
# Description: A plot showing the average Temp of each floor for each day
#
# Author:  Sean Calida
# Version: 11/13/2019
#############################################################################
SlusherDataDay <- mutate(SlusherData, day = day(Date))
SlusherData_daily <- SlusherDataDay %>% group_by(day, Level) %>% 
  summarise(mean_Temp = mean(Temp_F), max_Temp = max(Temp_F), 
            min_Temp = min(Temp_F))

Average_Plot <- ggplot(SlusherData_daily, aes(x = Level, y = mean_Temp, 
                          fill = as.character(day))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() + ylab("Mean Temp (F)") + 
  scale_fill_discrete(name = "Day", labels = c("Oct 14", "Oct 15", 
                                               "Oct 16", "Oct 17", 
                                               "Oct 18", "Oct 19",
                                               "Oct 20", "Oct 21",
                                               "Oct 22", "Oct 23", 
                                               "Oct 24", "Oct 25", 
                                               "Oct 26", "Oct 27",
                                               "Oct 28", "Oct 29",
                                               "Oct 30", "Oct 31", 
                                               "Nov 1", "Nov 2",
                                               "Nov 3", "Nov 4"))

#############################################################################
# Plot Name:   Box_Plot
# Description: A box plot showing the mean, min, and max of Temp for each 
#              floor
#
# Author:  Sean Calida
# Version: 11/13/2019
#############################################################################
Box_Plot <- ggplot(data = SlusherData, aes(x = Level, y = Temp_F)) + 
  geom_boxplot() + theme_minimal()