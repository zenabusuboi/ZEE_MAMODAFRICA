library(tidyverse) #load tidyverse library
library(ggplot2)

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

clean_routine_data <- read.csv('./clean_routine_data.csv') 

clean_routine_data_Admin1 <- clean_routine_data %>%
  group_by(Admin1,month) %>%
  summarise(across(c(test_u5:conf_ov5),sum))

### repartition of cases by age

##### at the municipality level

cases_age <- clean_routine_data_Admin1 %>%
  group_by(Admin1) %>%
  summarise(across(c(conf_u5,conf_ov5),sum)) %>%
  pivot_longer(cols=c(conf_u5,conf_ov5),
               names_to = "age_group",values_to = "conf",names_prefix = "conf_")

ggplot(cases_age,
       aes(x=Admin1,y=conf,fill=age_group))+
  geom_col()

##### at the national level

### test positivity rate

### seasonality of incidence

##### load population data
pop <- read.csv('./population.csv',sep=";")

##### by month

##### in 2018

