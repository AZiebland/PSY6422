#-------------------LOADING LIBRARIES--------------------------------------------
#install.packages(readxl)
#install.packages("countrycode")
#install.packages("scales")
#install.packages("ggiraph")
library(readxl) #Read excel file
library(tidyverse) #Transform and better present data
library(dplyr) #Makes data manipulation easier
library(ggplot2) #Create plots from a data frame
library(here) #Finds projects file based on current working directory
library(maps) #Source of geospatial data 
library(stringr) #Used for wrapping legend titles
library(gganimate) #Used to create animated GIF
library(gifski) #Used to create animated GIF
library(countrycode) #Converts country names into continent
library(scales) #Used to add dollar sign to labels
library(ggiraph) #Used to create interactive scatterplot with hover text

#-----------------------------LOADING THE DATA----------------------------------------------------
Happiness <- read_xls("Data/WorldHappinessReport.xls")

#Display the data
head(Happiness)

#-------------------------------DATA WRANGLING----------------------------------------------------
#Selecting and renaming the variables
HappinessReport <- Happiness %>% 
                    select('Country name', year, 'Life Ladder') %>% 
                    rename(Country = 'Country name', Year = year, Happiness_Score = 'Life Ladder')

#Creating data frame
HappinessReport = as.data.frame(HappinessReport)

#Filtering years to 2011-2021
HappinessReport <- HappinessReport[HappinessReport$Year >= "2011" & 
                                     HappinessReport$Year <= "2021",]

#Show first few rows of processed data
head(HappinessReport)

#-------------------------------------SUMMARY STATISTICS---------------------------------------
#Finding average happiness score of each country
Average <- HappinessReport %>% 
              group_by(Country) %>% 
                 summarise_at(vars(Happiness_Score),
                 list(Happiness_Score = mean))

#--------------------------------PLOTTING HAPPIEST COUNTRIES-----------------------------------
#Select top 10 happiest countries on average
Top10 <- Average %>% 
          arrange(desc(Happiness_Score)) %>% #Arrange happiness score in descending order
            slice(1:10) %>% #Select the first 10 data points
              arrange(Happiness_Score) %>% #Rearranging happiness score so highest score is plotted first
                mutate(Country = factor(Country, levels = Country)) #Mutate country column to display correct order

#Plot top 10 happiest countries
p_top10 <- ggplot(Top10, aes(Country, Happiness_Score, fill = Happiness_Score)) #Adding mappings to ggplot

p_top10 <- p_top10 +           #Adding geom layer to ggplot 
           geom_col() +        #Plotting x and y as a bar chart
           coord_flip() +      #Making the bars horizontal
           scale_fill_gradient2(low = "#99CCFF", high = "#0033FF", mid = "#6699FF", 
                                midpoint = median(Top10$Happiness_Score)) +  #Filling the bars based on happiness score
           geom_text(aes(label = round(Happiness_Score, 3)), nudge_y = -.55, color = "white") + #Add score on bar
           labs(title = "Top 10 Happiest Countries on Average Between 2011-2021", 
           subtitle = "0 = Worst Possible Life, 10 = Best Possible Life", 
           y = "Average Happiness Score", 
           caption = "Source: World Happiness Report, Gallup World Poll") + 
           theme_bw() + #Setting plot theme: remove legend, move title to left, remove background
           theme(plot.title = element_text(face = "bold"), 
                 plot.title.position = "plot",
                 panel.border = element_blank(), 
                 legend.position="none") 

#Saving the plot
ggsave(here("Figures", "Top 10 Happiest Countries.pdf"), plot = p_top10)

#----------------------------PLOTTING LEAST HAPPY COUNTRIES-------------------------------------
#Finding top 10 least happy countries
Least10 <- Average %>% 
            arrange(Happiness_Score) %>%  #Arrange happiness score in ascending order
             slice(1:10) %>% #Select the first 10 data points
              arrange(desc(Happiness_Score)) %>% #Rearranging scores so they are plotted from lowest to highest
                mutate(Country = factor(Country, levels = Country)) #Mutate country column to display correct order

#Plotting top 10 least happy countries
p_least10 <- ggplot(Least10, aes(Country, Happiness_Score, fill = Happiness_Score)) #Adding mappings to ggplot

p_least10 <- p_least10 +  #Adding geom layer to ggplot
             geom_col() +  #Plotting x and y as bar chart
             coord_flip() +  #Making the bars horizontal
             ylim(0, 8) +  #Setting y axis scale to match top 10 happiest countries
             scale_fill_gradient2(low = "#FF9999", high = "#FF0033", mid = "#FF6666", 
                                  midpoint = median(Least10$Happiness_Score)) +  #Filling bars based on happiness score
             geom_text(aes(label = round(Happiness_Score, 3)), nudge_y = -.55, color = "white") + #Add score on bar
             labs(title = "Top 10 Least Happy Countries on Average Between 2011-2021", #Adding labels
                  subtitle = "0 = Worst Possible Life, 10 = Best Possible Life", 
                  y = "Average Happiness Score", 
                  caption = "Source: World Happiness Report, Gallup World Poll") + 
             theme_bw() +  #Setting plot theme: remove legend, move title to left, remove background
             theme(plot.title = element_text(face = "bold"),
                   plot.title.position = "plot",
                   panel.border = element_blank(),
                   legend.position="none")

#Saving the plot
ggsave(here("Figures", "Top 10 Least Happy Countries.pdf"), plot = p_least10)

#----------------Creating choropleth map of world happiness over 10 years---------------------
#Loading world map
world_map <- map_data("world")

#Remove Antarctica 
world_map <- subset(world_map, region != "Antarctica")

#Recoding country names to match data
HappinessReport$Country <- recode(HappinessReport$Country, 
                                  'United States' = 'USA', 
                                  'United Kingdom' = 'UK')

#Wrapping legend title
name <- c("Average Happiness Score")
name <- str_wrap(name, width = 5)

#The following line of code was included in the rmarkdown file to combine the plots into a GIF {r, animation.hook='gifski', interval=1, fig.align='center', out.width="110%"}

#For loop to create choropleth map for each year
Year <- list("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

my_plots = list() 
for(i in 1:11) {
  
  #Year index starting from 2011 and increasing to 2021 
  Year1 = 2010 + i  
  
  #Selecting specific data using the index
  HappinessYear <- filter(HappinessReport, Year == Year1)  
  
  #Joining country in happiness data to region in world map data
  World_Map_Joined <- left_join(world_map, HappinessYear, by = c('region' = 'Country')) 
  
  #Creating the choropleth plot
  p <- ggplot(World_Map_Joined, aes(long, lat)) + #Adding mappings to ggplot
        geom_map(dat = world_map, map = world_map, #Plotting countries on map
                 aes(map_id = region), fill = "white", color = 'grey', linewidth = 0.25) + #Filling in the countries
        labs(title = sprintf("World Happiness Report: %s", Year[i]), #Adding labels
             subtitle = "0 = Worst Possible Life, 10 = Best Possible Life",
             caption = "Source: World Happiness Report, Gallup World Poll") +
        theme(plot.title = element_text(face = "bold"),
              axis.title = element_blank(), #Removing x and y labels
              axis.text.x = element_blank(), 
              axis.text.y = element_blank())
  
  #Plotting happiness scores onto the map
  p_out <- p + geom_map(map = World_Map_Joined, aes(map_id = region, fill = Happiness_Score), 
                        linewidth = 0.25) + #Plotting happiness score onto map
                scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = name, limits = c(2, 8)) + #Filling countries based on happiness score
                coord_fixed(1.5) #Fixing aspect ratio
  
  print(p_out)
  
  #Saving each plot
  ggsave(here("Figures", file=paste0("plot_", 2010 + i,".png")))
}

#-----------------------Loading average income dataset----------------------------------------------
#Loading the data
Average_Income <- read.csv("Data/HumanDevelopmentReport.csv")

#Showing first few rows of raw data
head(Average_Income)

#Removing unwanted data
Average_Income <- Average_Income %>% 
                    filter(!row_number() %in% c(1:7, 202:276)) %>% #Remove columns with unwanted data
                      select(Table.1..Human.Development.Index.and.its.components, X.8) %>% 
                        rename(Country = Table.1..Human.Development.Index.and.its.components, 
                               Average.Income = X.8) %>% #Renaming long variable
                          mutate_all(na_if, "") %>% #Entering NA for missing data
                            na.omit() #Removing rows containing NA

#Remove comma in average income to make class numeric
Average_Income$Average.Income <- as.numeric(gsub(",", "", Average_Income$Average.Income))

#Selecting data from 2021 in happiness report
Happiness2021 <- HappinessReport %>% 
                  filter(Year == "2021") %>% 
                   na.omit()
  
#Joining the happiness data in 2021 to average income data so only countries that also have a happiness rating are plotted
Joined_Data <- left_join(Happiness2021, Average_Income, by = "Country") %>% 
                select(Country, Happiness_Score, Average.Income) %>% 
                  na.omit()

#-------------------PLOTTING RICHEST COUNTRIES IN THE WORLD----------------------------------
#Arranging average income in descending order and selecting first 10
MostRich <- Joined_Data %>% 
              arrange(desc(Average.Income)) %>% 
              slice(1:10) %>% 
              mutate(Country = factor(Country, levels = Country)) #Mutate country column to display correct order

#Plotting top 10 richest countries
p_rich <- ggplot(MostRich, aes(x = reorder(Country, Average.Income) , y = Average.Income)) #Addings mappings to ggplot

p_rich <- p_rich +
            geom_col(aes(fill=Average.Income)) + #Adding geom layer to ggplot
            coord_flip() + #Making bars horizontal
            scale_fill_gradient2(low = "#99CCFF", high = "#0033FF" , mid = "#6699FF", 
                                 midpoint = median(MostRich$Average.Income)) + #Filling bars based on average income
            geom_text(aes(label = dollar(Average.Income)), hjust = 1, nudge_y = -0.35, 
                      color = "white") + #Adding income on top of bar
            labs(title = "Top 10 Richest Countries based on average income per capita ($) in 2021", #Adding labels
                 x= "Country", y = "Average Income Per Capita ($)", 
                 caption = "Source: United Nations Development Programme") +
            theme_bw() + #Setting plot theme
            theme(plot.title = element_text(face = "bold"), 
                  plot.title.position = "plot", 
                  panel.border = element_blank(), 
                  legend.position="none")

#Saving the plot
ggsave(here("Figures", "Top 10 Richest Countries.pdf"), plot = p_rich)

#--------------------------PLOTTING POOREST COUNTRIES------------------------------------------
#Arranging data in ascending order and selecting first 10
MostPoor <- Joined_Data %>% 
              arrange(Average.Income) %>% 
              slice(1:10) %>% 
              arrange(desc(Average.Income)) %>% 
              mutate(Country = factor(Country, levels = Country)) #Mutate country column to display correct order

#Plotting top 10 poorest countries
p_poor <- ggplot(MostPoor, aes(x = Country, y = Average.Income)) #Adding mappings to ggplot

p_poor <- p_poor +
          geom_col(aes(fill=Average.Income)) +
          coord_flip() +
          scale_fill_gradient2(low = "#FF9999", high = "#FF0033", mid = "#FF6666", 
                               midpoint = median(MostPoor$Average.Income)) + 
          geom_text(aes(label = dollar(Average.Income)), hjust = -0.5, color = "black") + #Adding income next to bar
          ylim(0, 90000) + #Setting scale to match top 10 rich countries bar chart
          labs(title = "Top 10 Poorest Countries based on average income per capita ($) in 2021", 
               x= "Country", y = "Average Income Per Capita ($)", 
               caption = "Source: United Nations Development Programme") +
          theme_bw() +
          theme(plot.title = element_text(face = "bold"), #Setting the plot theme
                plot.title.position = "plot",
                panel.border = element_blank(),
                legend.position="none")

#Saving the plot
ggsave(here("Figures", "Top 10 Poorest Countries.pdf"), plot = p_poor)

#-------------------------SCATTERPLOT OF HAPPINESS AND INCOME------------------------------------
#Adding column with continent
Joined_Data$continent <- countrycode(sourcevar = Joined_Data$Country, 
                                     origin = "country.name", 
                                     destination = "continent")

#Creating tooltip column for hover text
Joined_Data <- Joined_Data %>% 
                mutate(tooltip_text = paste0(Country, "\n",
                                             "Happiness Score: ", round(Happiness_Score, 3), "\n",
                                             "Average Income: ", dollar(Average.Income)))

#Creating scatterplot - ggiraph package is used to make scatterplot interactive - when mouse hovers over points on the plot, country name, happiness score and average income will be visible
p_both <- ggplot(Joined_Data, aes(Average.Income, Happiness_Score)) + 
          geom_point_interactive(aes(col = continent, #Make scatterplot with hover text
                                     tooltip = tooltip_text)) +
          geom_smooth(method="lm", formula= y ~ log(x), se = FALSE, color = "black", size = 0.5) + #Add logarithmic regression line
          labs(title = "The Relationship Between Average Income and Happiness Score", #Add labels
               subtitle = "Data from 104 countries in 2021",
               x = "Average Net National Income Per Capita, US($)", 
               y = "Happiness Score",
               caption = "Source: World Happiness Report, United Nations Development Programme",
               color = "Continent") +
          theme(plot.title = element_text(face = "bold"), #Make title bold
                plot.title.position = "plot") #Move title position

#Display interactive graph with girafe() function             
girafe(ggobj = p_both)

#Saving the plot
ggsave(here("Figures", "Average Income and Happiness Score.pdf"), plot = p_both)
