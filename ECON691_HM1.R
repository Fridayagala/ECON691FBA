#Our Script for ECON 691 Home Work 1
#Created by :Friday B. Agala
#Created on :Dec 08, 2023


library(tidycensus)
library(tidyverse)
library(ggplot2)
var <- load_variables(2021, "acs5", cache = TRUE)

temp <- var %>%
  filter(grepl("MEDIAN ", concept)) %>%
  filter(grepl("GROSS CONTRACT RENT", concept))

#control+enter to run the last code
#now we see that temp variable created about 46 observations for the 4 variables

vars <- c("B06011_001", "B25058_001")

acs <- get_acs(geography = "county", #Defines geography level of data
               variables = vars,     #specifies the data we want
               state=c(18,39,21, 47),          #denotes the specific state
               year=2021,         #denotes the year
               geometry = TRUE)     #downloads the TIGER shapefile data


#state code for Idiana,Ohio,Kentucky, and Tennessee is 18,39,21, and 47,respectively.

core <- acs %>% #this helps to clean the data
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
  
  
  
  
#for summary, use the command: summary(core)

#ggplot(core) +
 # geom_sf(aes(fill = Afford)) #visualizes the afford variable


#ggplot(core) +
 # geom_sf(aes(fill=Affordable)) + #visualizes the affordable variable
#scale_fill_gradient2()+
 # theme_bw()

ggplot(core) +
 geom_sf(aes(fill=Med_Inc + Med_Rent))  #visualizes the affordable variable by filling with Median Income and Median Rent.
 
#ggplot(core) +
 # geom_sf(aes(fill=Med_Rent))
