library(tidyverse) #load tidyverse library

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

routine_data <- read.csv('./routine_data.csv',sep=";") #load routine dataset

#example of tidyverse 
clean_routine_data <- routine_data %>% #pipe operator
  mutate(test_total = test_u5+test_ov5)
  
write_csv(clean_routine_data, './clean_routine_data.csv') #save cleaned dataset