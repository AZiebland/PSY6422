Happiness.csv <- '/Users/millieziebland/Documents/Uni/Semester 2/
  PSY6422 Data Analysis and Visualisation/Coding Project/PSY6422 Project'

#Loading the data
library(readxl)
Happiness <- read_xls("Data/WorldHappinessReport.xls")

#Display the data
head(Happiness)

#Cleaning the data
library(tidyverse)
HappinessReport <- select(Happiness, 'Country name', 'year', 'Life Ladder')
head(HappinessReport)
HappinessReport <- rename(HappinessReport, Country = 'Country name', Year = year, Happiness_Score =
                            'Life Ladder')

#Changing Year variable from character to date
Year <- HappinessReport %>% 
  mutate(Year = as.Date(ISOdate(Year, 1, 1)), Year = format(Year, '%Y'))


#Creating heat map 
library(maps)
library(ggplot2)
library(dplyr)
library(stringr)
world_map <- map_data("world")

#Recoding country names to match data
as.factor(HappinessReport$Country) %>% levels()
HappinessReport$Country <- recode(HappinessReport$Country, 
                                  'United States' = 'USA', 
                                  'United Kingdom' = 'UK')
world_map <- subset(world_map, region != "Antarctica")

name <- c("Average Happiness Score")
name <- str_wrap(name, width = 5)
#Joining the two datasets

#Simple plot
p <- ggplot(HappinessReport) + geom_map(dat = World_Map_Year, map = World_Map_Year, aes(map_id = region),
                                        fill = "white", color = '#7f7f7f', linewidth = 0.25) + 
  ggtitle("World Happiness Report")
p_out <- p + geom_map(map = World_Map_Year, aes(map_id = "region", fill = Happiness_Score), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = name) +
  expand_limits(x = World_Map_Year$long, y = World_Map_Year$lat) + coord_fixed(1.5)




my_plots = list() 
for(i in 0:16) {
  Year1 = 2005 + i
  HappinessYear <- filter(HappinessReport, Year == Year1)
  World_Map_Joined <- left_join(world_map, HappinessYear, by = c('region' = 'Country'))
  p <- ggplot() + geom_map(dat = world_map, map = world_map, aes(long, lat, map_id = region),
                           fill = "white", color = 'grey', linewidth = 0.25) +
    ggtitle("World Happiness Report")
  p_out <- p + geom_map(map = World_Map_Joined, aes(map_id = World_Map_Joined$region, fill = World_Map_Joined$Happiness_Score), linewidth = 0.25) +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = name) +
    expand_limits(x = World_Map_Joined$long, y = World_Map_Joined$lat) + coord_fixed(1.5)
  print(p_out)
 
}



Year1=2009
HappinessYear <- filter(HappinessReport, Year == Year1)
World_Map_Joined <- left_join(world_map, HappinessYear, by = c('region' = 'Country'))

p <- ggplot() + geom_map(dat = world_map, map = world_map, aes(long, lat, map_id = region),
                                        fill = "white", color = 'white', linewidth = 0.25) +
                                        ggtitle("World Happiness Report")
p_out <- p + geom_map(map = World_Map_Joined, aes(map_id = World_Map_Joined$region, fill = World_Map_Joined$Happiness_Score), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = name) +
  expand_limits(x = World_Map_Joined$long, y = World_Map_Joined$lat) + coord_fixed(1.5)


#Wrapping legend title
