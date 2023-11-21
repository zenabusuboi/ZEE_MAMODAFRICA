library(tidyverse) #load tidyverse library

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

routine_data <- read.csv('./routine_data.csv',sep=";") #load routine dataset

clean_routine_data <- routine_data %>% 
  mutate(Admin1 = case_when(Admin1 =='Norteast Greenland National Park'~'Northeast Greenland National Park',
                            Admin1 =="sermersooq"~"Sermersooq",
                            Admin1 == 'Kujaleq'~'Kujalleq',
                            TRUE~Admin1),
         year = case_when(year == 3018 ~ 2018,
                          year == 18 ~ 2018,
                          TRUE ~ 2018), 
         across(c(test_u5, test_ov5, conf_u5, conf_ov5),~as.integer(gsub("O",0,.)))) %>%
  drop_na() %>% 
  filter(test_u5>=conf_u5,
         test_ov5>=conf_ov5,
         if_any(c(test_u5, test_ov5, conf_u5, conf_ov5),~.!=-9999)) 

write_csv(clean_routine_data, './clean_routine_data.csv') #save cleaned dataset
