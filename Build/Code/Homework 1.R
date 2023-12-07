#Our Script for ECON 691 Home Work 1
#Created by :Friday B. Agala
#Created on :Dec 08, 2023


library(tidycensus)
library(tidyverse)
library(ggplot2)

var <- load_variables(2021, "acs5", cache = TRUE)

temp <- var %>%
  filter(grepl("MEDIAN ", concept)) %>%
  filter(grepl("MEDIAN GROSS CONTRACT RENT", concept))


#The temp variable created about 46 observations for the 4 variables of interest

vars <- c("B06011_001", "B25058_001")

acs <- get_acs(geography = "county",           #Defines geography level of data
               variables = vars,               #Specifies the data we want
               state=c(18,39,21, 47),          #Denotes the specific states with their code
               year=2021,                      #Represents the year
               geometry = TRUE)                #Downloads the TIGER shapefile data


#state code for Indiana, Ohio, Kentucky, and Tennessee is 18,39,21, and 47,respectively.

core <- acs %>%                                                #This line of codes helps to clean the data
  select(-moe) %>%
  mutate(variable = case_when(variable == "B06011_001"~"Med_Inc",
                              variable == "B25058_001"~"Med_Rent",
                              TRUE ~ variable))  %>%               #use to modify or create a column
  
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), 
              names_from = "variable",
              values_from = "estimate") %>%
  
  mutate(med_inc2 = Med_Inc/12,
         Afford = Med_Rent/med_inc2,
         Affordable = 0.33 - Afford)




#This first code visualizes the afford variable for the four states: Indiana, Kentucky, Ohio, and Tennessee.

ggplot(core) +
  geom_sf(aes(fill = Afford)) 

#The second code visualizes the affordable variable for Indiana, Kentucky, Ohio, and Tennessee.

ggplot(core) +
  geom_sf(aes(fill=Affordable)) + 
  scale_fill_gradient2()+
  theme_bw()  #This is the one I wanted to see.

#The third code visualizes the affordable variable by filling with Median Income for the four states.

ggplot(core) +
  geom_sf(aes(fill=Med_Inc))  

#This last code visualizes the affordable variable by filling with Median Rent for the four states.
ggplot(core) +
  geom_sf(aes(fill=Med_Rent))  

#for summary, use the command: summary(core)