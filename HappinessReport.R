#-------------------Installing and loading libraries-------------------------
#install.packages(readxl)
#install.packages("countrycode")
#install.packages("imputeTS")
#install.packages("padr")

library(readxl)
library(tidyverse)
library(dplyr)
library(grid)
library(ggplot2)
library(shadowtext)
library(here)
library(maps)
library(stringr)
library(gganimate)
library(gifski)
library(countrycode)
library(imputeTS)
library(padr)

#--------------------Loading the data----------------------------------------
Happiness <- read_xls("Data/WorldHappinessReport.xls")

#Display the data
head(Happiness)

#--------------------Cleaning the data----------------------------------------
HappinessReport <- Happiness %>% 
                   select('Country name', year, 'Life Ladder') %>% 
                   rename(Country = 'Country name', Year = year, Happiness_Score = 'Life Ladder')

HappinessReport = as.data.frame(HappinessReport)

#Changing class of year variable to date
HappinessReport$Year <- as.Date(ISOdate(HappinessReport$Year, 1, 1))

#Filtering years to 2011-2021
HappinessReport <- HappinessReport[HappinessReport$Year >= "2011-01-01" & 
                                     HappinessReport$Year <= "2021-01-01",]

#Creating average for missing years
#First add row for missing years with NA
HappinessReport <- HappinessReport %>%
  pad(group = "Country", start_val = as.Date('2011-01-01'),
      end_val = as.Date('2021-01-01'))

HappinessReport %>% 
  group_by(Country) %>% 
    na_ma(Happiness_Score, k = 1, weighting = "simple")




#Calculating weighted moving average


#Show first few rows of processed data
head(HappinessReport)




#--------------------Summary Statistics---------------------------------------
#Finding average happiness score of each country
Average <- HappinessReport %>% 
  group_by(Country) %>% 
  summarise_at(vars(Happiness_Score),
               list(Happiness_Score = mean))

#Select top 10 happiest countries on average
Top10 <- Average %>% 
            arrange(desc(Happiness_Score)) %>% 
            slice(1:10) %>% 
            arrange(Happiness_Score) %>% 
            mutate(Country = factor(Country, levels = Country))

#Plot top 10 happiest countries
p_top <- ggplot(Top10, aes(Country, Happiness_Score, fill = Happiness_Score)) +
    geom_col() +
    scale_fill_gradient2(low = "#99CCFF", high = "#0033FF", mid = "#6699FF", 
    midpoint = median(Top10$Happiness_Score)) +
    coord_flip() +
    geom_text(aes(label = round(Happiness_Score, 3)), 
    hjust = 1, nudge_y = -.25, color = "white") +
    labs(title = "Top 10 Happiest Countries on Average Between 2005-2021", 
    subtitle = "Life Evaluation based on the Cantril Ladder where 0 = worst possible life, 10 = best possible life", 
    y = "Average Happiness Score", caption = "Source: World Happiness Report, Gallup World Poll")
  
#Setting the plot theme
p_top1 <- p_top +
  theme_bw() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title.position = "plot") +
  theme(panel.border = element_blank()) +
  theme(legend.position="none")

#Saving the plot
ggsave(here("Figures", "Top 10 Happiest Countries.pdf"), plot = p_top1)


#Finding top 10 least happy countries
Least10 <- Average %>% 
  arrange(Happiness_Score) %>% 
  slice(1:10) %>% 
  arrange(Happiness_Score) %>% 
  mutate(Country = factor(Country, levels = Country))

#Plotting top 10 least happy countries
p_bottom <- ggplot(Least10, aes(Country, Happiness_Score, fill = Happiness_Score)) +
  geom_col() +
  ylim(0, 8) +
  scale_fill_gradient2(low = "#FF9999", high = "#FF0033", mid = "#FF6666", 
  midpoint = median(Least10$Happiness_Score)) +
  coord_flip() +
  geom_text(aes(label = round(Happiness_Score, 3)), 
  hjust = 1, nudge_y = -.25, color = "white") +
  labs(title = "Top 10 Least Happy Countries on Average Between 2005-2021", 
  subtitle = "Life Evaluation based on the Cantril Ladder where 0 = worst possible life, 10 = best possible life", y = "Average Happiness Score", 
  caption = "Source: World Happiness Report, Gallup World Poll")

#Setting plot theme
p_bottom1 <- p_bottom +
  theme_bw() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title.position = "plot") +
  theme(panel.border = element_blank()) +
  theme(legend.position="none")
p_bottom1
#Saving the plot
ggsave(here("Figures", "Top 10 Least Happy Countries.pdf"), plot = p_bottom1)


#----------------Creating choropleth map of world happiness over 10 years---------------------
#Loading world map
world_map <- map_data("world")

#Remove Antarctica 
world_map <- subset(world_map, region != "Antarctica")

#Recoding country names to match data
as.factor(HappinessReport$Country) %>% levels()
HappinessReport$Country <- recode(HappinessReport$Country, 
                                  'United States' = 'USA', 
                                  'United Kingdom' = 'UK')




#Wrapping legend title
name <- c("Average Happiness Score")
name <- str_wrap(name, width = 5)

#For loop to create chlorpleth map for each year
Year <- list("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
my_plots = list() 
for(i in 0:13) {
  Year1 = 2008 + i
  HappinessYear <- filter(HappinessReport, Year == Year1)
  World_Map_Joined <- left_join(world_map, HappinessYear, by = c('region' = 'Country'))
  p <- ggplot(World_Map_Joined, aes(long, lat)) + geom_map(dat = world_map, map = world_map, 
       aes(map_id = region), fill = "white", color = 'grey', linewidth = 0.25) +
       labs(title = sprintf("World Happiness Report: %s", Year[i]), 
       subtitle = "Life Evaluation based on the Cantril Ladder where 0 = worst possible life, 10 = best possible life",
       caption = "Source: World Happiness Report, Gallup World Poll") +
       theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  p_out <- p + geom_map(map = World_Map_Joined, aes(map_id = region, fill = Happiness_Score), 
           linewidth = 0.25) +
           scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = name) +
           expand_limits(x = World_Map_Joined$long, y = World_Map_Joined$lat) + 
           coord_fixed(1.5)
  print(p_out)
  #ggsave(here("Figures", file=paste0("plot_", 2008 + i,".png")))
  
}

#-----------------------Loading average income dataset----------------------------------------------
Average_Income <- read.csv("Data/HumanDevelopmentReport.csv")

#Removing unwanted data
Average_Income <- Average_Income %>% 
                  filter(!row_number() %in% c(1:7, 202:276)) %>% 
                  select(Table.1..Human.Development.Index.and.its.components, X.8) %>% 
                  rename(Country = Table.1..Human.Development.Index.and.its.components, 
                         Average.Income = X.8) %>% 
                  mutate_all(na_if, "") %>% 
                  na.omit()
      
#Remove comma in average income to make class numeric
Average_Income$Average.Income <- as.numeric(gsub(",", "", Average_Income$Average.Income))

#-------------------Plotting top 10 richest countries in the world---------------------------
Happiness2021 <- HappinessReport %>% 
  filter(Year == "2021-01-01")

Joined_Data <- inner_join(Happiness2021, Average_Income, by = "Country") %>% 
  select(Country, Happiness_Score, Average.Income)

library(scales)
MostRich <- Joined_Data %>% 
            arrange(desc(Average.Income)) %>% 
            slice(1:10) %>% 
            mutate(Country = factor(Country, levels = Country))

p_rich <- ggplot(MostRich, aes(x = reorder(Country, Average.Income) , y = Average.Income)) +
          geom_col(aes(fill=Average.Income)) +
          coord_flip() +
          scale_fill_gradient2(low = "#99CCFF", high = "#0033FF" , mid = "#6699FF", 
                               midpoint = median(MostRich$Average.Income)) +
          geom_text(aes(label = dollar(Average.Income)), hjust = 1, nudge_y = -0.5, color = "white") +
          labs(title = "Top 10 Richest Countries based on average income per capita ($) in 2021", 
          x= "Country", y = "Average Income Per Capita ($)", 
          caption = "Source: United Nations Development Programme")

#Setting the plot theme
p_rich1 <- p_rich +
  theme_bw() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title.position = "plot") +
  theme(panel.border = element_blank()) +
  theme(legend.position="none")

p_rich1

#Plotting top 10 poorest countries
MostPoor <- Joined_Data %>% 
  arrange(Average.Income) %>% 
  slice(1:10) %>% 
  mutate(Country = factor(Country, levels = Country))


p_poor <- ggplot(MostPoor, aes(x = Country, y = Average.Income)) +
          geom_col(aes(fill=Average.Income)) +
          coord_flip() +
          scale_fill_gradient2(low = "#FF9999", high = "#FF0033", mid = "#FF6666", 
          midpoint = median(MostPoor$Average.Income)) + 
          geom_text(aes(label = dollar(Average.Income)), hjust = 1.25, nudge_y = -.25, color = "white") +
          labs(title = "Top 10 Poorest Countries based on average income per capita ($) in 2021", 
          x= "Country", y = "Average Income Per Capita ($)", 
          caption = "Source: United Nations Development Programme")

#Setting the plot theme
p_poor1 <- p_poor +
  theme_bw() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title.position = "plot") +
  theme(panel.border = element_blank()) +
  theme(legend.position="none")

p_poor1

#Merging happiness data set with average income data set for 2020
Happiness2021 <- HappinessReport %>% 
                 filter(Year == "2021-01-01")

Joined_Data <- inner_join(Happiness2021, Average_Income, by = "Country") %>% 
               select(Country, Happiness_Score, Average.Income)


#Adding column with continent
Joined_Data$continent <- countrycode(sourcevar = Joined_Data$Country, origin = "country.name",
                         destination = "continent")









#----------------Plotting average income against happiness score in 2020--------------------

#Saving the plot
ggsave(here("Figures", "Average Income and Happiness Score.pdf"), plot = p_out1)



p_out1


library(plotly)
install.packages("ggiraph")
library(ggiraph)

gg_point = ggplot(Joined_Data) + geom_point_interactive(aes(x = Average.Income, 
                                  y = Happiness_Score, col = continent, tooltip = Country, 
                                  data_id = Happiness_Score)) +
  labs(title = "The Relationship Between Average Income and Happiness Score",
       subtitle = "Data from 104 countries in 2021",
       x = "Average Net National Income Per Capita, US($)", 
       y = "Happiness Score") +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot")
              
girafe(ggobj = gg_point)

p_out1 <- p1 +
  geom_point_interactive(aes(col = continent, text = (paste("Country:", Country,
  "Average Income:", dollar(Average.Income), "Happiness Score:", round(Happiness_Score, 3))))) +
  stat_smooth(method="lm", formula= y ~ log(x), se = FALSE, color = "black", size = 0.5) +
  labs(x = "Average Net National Income Per Capita, US($)", y = "Happiness Score", color = "Continent") 


  

p_both <- ggplot(Joined_Data, aes(x = Average.Income, y = Happiness_Score)) #Adding mappings to ggplot

p_both <- p_both +
  geom_point(aes(col = continent, text = paste("Country:", Country,
                                               "<br>Average Income:", dollar(Average.Income), 
                                               "<br>Happiness Score:", round(Happiness_Score, 3)))) +
  stat_smooth(method="lm", formula= y ~ log(x), se = FALSE, color = "black", size = 0.5) +
  labs(x = "Average Net National Income Per Capita, US($)", 
       y = "Happiness Score",
       color = "Continent")

#Creating interactive plot with hover text
plot_ly(p_both, tooltip = "text") %>% 
  layout(title = list(text = paste0('<b> The Relationship Between Average Income 
and Happiness Score <b>', font = 1)),
         legend = list(title=list(text='<b> Continent </b>')))



Joined_Data$continent <- countrycode(sourcevar = Joined_Data$Country, 
                                     origin = "country.name", 
                                     destination = "continent")
Joined_Data <- Joined_Data %>% 
  mutate(tooltip_text = paste0(Country, "\n",
                               "Happiness Score: ", round(Happiness_Score, 3), "\n",
                               "Average Income: ", dollar(Average.Income)))
#Creating scatterplot
gg_point = ggplot(Joined_Data, aes(Average.Income, Happiness_Score)) + 
  geom_point_interactive(aes(col = continent, 
                             tooltip = tooltip_text, 
                             data_id = Happiness_Score)) +
  geom_smooth(method="lm", formula= y ~ log(x), se = FALSE, color = "black", size = 0.5) +
  labs(title = "The Relationship Between Average Income and Happiness Score",
       subtitle = "Data from 104 countries in 2021",
       x = "Average Net National Income Per Capita, US($)", 
       y = "Happiness Score",
       caption = "Source: World Happiness Report and United Nations Development Programme",
       color = "Continent") +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

#Making the scatterplot interactive              
girafe(ggobj = gg_point)