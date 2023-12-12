#Scripts for ECON 691 Homework 2 
#Created by: Friday B. Agala
#Created on: December 08, 2023

rm(list=ls())


library(tidycensus)
library(tidyverse)
library(sf)
library(stargazer)
library(dplyr)


#This helps us to download the data from 5acs Census API for 2021

vars <- load_variables(2021, "acs5", cache = TRUE)


#This code is designed to help filter the list of variables.
data<-vars  %>%
  filter(grepl("CENSUS DATA API", concept)) 


#This line helps to get access to the data from ACS 5-Year API
vars<-c("B01001_001", "B01001_003", "B01001_004", "B01001_005", "B01001_006",
        "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
        "B01001_027", "B01001_028", "B01001_029", "B01001_030",
        "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
        "B02001_002", "B02001_003", "B02001_005",
        "B25087_001", "B25087_002", "B25088_001",
        "B99084_001", "B99084_005",
        "B06011_001", "B25058_001")

#This command pulls the data from ACS
years <- c(2018, 2019, 2020, 2021)

#This is the second part: programing a loop to pull down the data

for(i in years){
  acs <- get_acs(geography = "county",	                 #defines geography level of data 
                 variables = vars,    	                 #specifics the data we want 
                 state=c(18,39,21, 47),                  #denotes the specific states 
                 year = i,	                             #denotes the year
                 geometry = TRUE)	                       #downloads the TIGER shapefile data 
  
 
  
  core <- acs %>%                                        #Clean and Pivot the data
    
    mutate(variable = case_when(variable=="B01001_001" ~ "Population",
                                variable=="B06011_001" ~ "Med_Inc",
                                variable=="B25058_001" ~ "Med_Rent",
                                variable=="B25088_001" ~ "Med_Cost",
                                TRUE ~ variable)) %>%
    select(-"moe") %>%
    pivot_wider(id_cols = c("GEOID", "NAME", "geometry"),  names_from = "variable", values_from = "estimate") 
  
  core <- core %>%
    group_by(GEOID) %>%
    
    mutate(per_und18 = (sum(c_across(B01001_003:B01001_006))+sum(c_across(B01001_027:B01001_030)))/Population,
           per_ovr64 = (sum(c_across(B01001_020:B01001_025))+sum(c_across(B01001_044:B01001_049)))/Population,
           per_blk   = (B02001_003)/Population,
           per_wht   = (B02001_002)/Population,
           per_asn   = (B02001_005)/Population,
           per_oth   = 1 - per_wht - per_blk - per_asn,
           per_mort  = (B25087_002)/B25087_001,
           per_wfh   = (B99084_005)/B99084_001,
           m_monthly = Med_Inc/12,
           Rent_Share = Med_Rent/m_monthly,
           Affordable = 0.33 - Rent_Share,
           Population = Population/10000000,
           Med_Cost = Med_Cost/10000,
           Year = i) 
  ifelse(i==years[1], CORE <- core, CORE <- rbind(CORE, core))
  
}


#In the third part of the assignment, we are expected to estimate 6 models:


#Models 1, 3.1, and 4.1 regress affordability on all characteristic variables for the four asigned states (Indiana, Kentucky, Ohio, and Tennessee) capturing year and county fixed effects.
#Models 2, 3.2, and 4.2 regress Rent Share on all characteristic variables for the four asigned states (Indiana, Kentucky, Ohio, and Tennessee) capturing year and county fixed effects.

mod1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost,
           data = core)

mod2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost,
           data = core)

mod3.1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost +
              factor(Year),
            data = CORE)

mod3.2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost + 
              factor(Year), 
            data = CORE)

mod4.1 <- lm(Affordable ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost +
               factor(Year)+factor(GEOID),
             data = CORE)

mod4.2 <- lm(Rent_Share ~ Population + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + Med_Cost + 
               factor(Year)+factor(GEOID),
             data = CORE)

  
  #Visualize the Data

temp <- CORE %>%
  group_by(GEOID) %>%
  reframe(x = mean(per_blk, .by=GEOID))

vis <- acs %>%
  select(GEOID, geometry) %>%
  distinct() %>%
  left_join(., temp, by="GEOID") %>%
  mutate(est = x * mod2$coefficients[5])   #I suspect this is a remnent from the original code?

ggplot(vis) + 
  geom_sf(aes(fill=est)) +
  scale_fill_gradient2()+
  theme_bw()

ggplot(vis) + 
  geom_sf(aes(fill=x)) +
  scale_fill_gradient2()+
  theme_bw()

ggplot(core) + 
  geom_sf(aes(fill=Affordable)) +
  scale_fill_gradient2()+
  theme_bw()


#The last Part of the Homework 2 involves using stargazer package to create an output table in both LaTex and HTML formats.


#LaTeX Format: 

model_list <- list(mod1, mod2, mod3.1, mod3.2, mod4.1, mod4.2)

latex_table <- stargazer(model_list, title = "Regression Models", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_models_table.latex")

browseURL("regression_models_table.latex")




#HTML Format:

model_list <- list(mod1, mod2, mod3.1, mod3.2, mod4.1, mod4.2)

html_table <- stargazer(model_list, title = "Regression Models", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_models_table.html")

browseURL("regression_models_table.html")





#For the Last Part, I want to also create 6 tables; one for each model.

#LATEX FORMAT

#LaTeX format for Model 1
latex_table <- stargazer(mod1, title = "Regression Model 1", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model1_table.latex")

browseURL("regression_model1_table.latex")

#LaTeX format for Model 2
latex_table <- stargazer(mod2, title = "Regression Model 2", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model2_table.latex")

browseURL("regression_model2_table.latex")

#LaTeX format for Model 3.1
latex_table <- stargazer(mod3.1, title = "Regression Model 3", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model3_table.latex")

browseURL("regression_model3_table.latex")

#LaTeX format for Model 3.2
latex_table <- stargazer(mod3.2, title = "Regression Model 4", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model4_table.latex")

browseURL("regression_model4_table.latex")

#LaTeX format for Model 4.1
latex_table <- stargazer(mod4.1, title = "Regression Model 5", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model5_table.latex")

browseURL("regression_model5_table.latex")

#LaTeX format for Model 4.2
latex_table <- stargazer(mod4.2, title = "Regression Model 6", align = TRUE, type = "latex")

cat(latex_table, sep = "\n")

writeLines(latex_table, "regression_model6_table.latex")

browseURL("regression_model6_table.latex")


#HTML FORMAT

#Html Format for Model 1
html_table <- stargazer(mod1, title = "Regression Model 1", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model1_table.html")

browseURL("regression_model1_table.html")

#Html format for Model 2
html_table <- stargazer(mod2, title = "Regression Model 2", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model2_table.html")

browseURL("regression_model2_table.html")

#Html format for Model 3.1
html_table <- stargazer(mod3.1, title = "Regression Model 3", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model3_table.html")

browseURL("regression_model3_table.html")

#Html format for Model 3.2
html_table <- stargazer(mod3.2, title = "Regression Model 4", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model4_table.html")

browseURL("regression_model4_table.html")

#Html format for Model 4.1
html_table <- stargazer(mod4.1, title = "Regression Model 5", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model5_table.html")

browseURL("regression_model5_table.html")

#Html format for Model 4.2
html_table <- stargazer(mod4.2, title = "Regression Model 6", align = TRUE, type = "html")

cat(html_table, sep = "\n")

writeLines(html_table, "regression_model6_table.html")

browseURL("regression_model6_table.html")
