library(tidyverse) #load tidyverse library
library(lubridate)

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

routine_data <- read.csv('./routine_data.csv',sep=";",na.strings = c("", "NA", -9999)) #load routine dataset
#----------------------------------------------------------

# MY DATA CLEANING
my_routine_data <- routine_data 

 #type of each column of dataframe
str(my_routine_data$Admin1)
str(my_routine_data$hf)
str(my_routine_data$month)
str(my_routine_data$year)
str(my_routine_data$test_u5)
str(my_routine_data$conf_u5)
str(my_routine_data$test_ov5)
str(my_routine_data$conf_ov5)
#----------------------------------------------
clean_routine_data <- my_routine_data %>%
  select(Admin1, month, year, test_u5, test_ov5, conf_u5, conf_ov5) %>% # drop unwanted cols
  drop_na(test_u5, conf_u5, test_ov5, conf_ov5) %>% #drop NAs
  filter(test_u5>conf_u5,
         test_ov5>conf_ov5) %>% #filter only rows where test cases are more than confirmed cases
 mutate(Admin1 = case_when(Admin1 == 'Norteast Greenland National Park'~'Northeast Greenland National Park',
            Admin1 == 'Kujaleq'~'Kujalleq',
            Admin1 == 'sermersooq'~'Sermersooq',
            TRUE~Admin1),
  month = factor(month,
                 levels = month.abb),
year = case_when(year == 3018 ~ 2018,
                 year == 18 ~ 2018,
                 TRUE ~ 2018),
date_tested = make_date(year = year, month = month),
across(c(test_u5, test_ov5, conf_u5, conf_ov5),~as.integer(gsub("O",0,.))),
test_total = test_u5+test_ov5,
conf_total = conf_u5+conf_ov5) %>%
  group_by(Admin1, date_tested)%>% # specify cols to keep and do modification on the rest
  summarise(across(c(test_u5:conf_ov5,test_total,conf_total),sum)) # aggregate 
# all monthly data per admin1 

write_csv(clean_routine_data, './clean_routine_data.csv') #save cleaned dataset

  
  
  
  
  
  
  