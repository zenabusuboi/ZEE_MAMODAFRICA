library(tidyverse) #load tidyverse library
library(ggplot2)
library(gridExtra)

rm(list=ls()) #clean the environment

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets the folder where
#'this file is saved as current directory;
#'all paths are now expressed relative to the current directory

my_clean_routine_data <- read.csv('./clean_routine_data_1.csv') 

clean_routine_data_Admin1 <- my_clean_routine_data %>%
  group_by(Admin1,month) %>% # specify cols to keep and do modification on the rest
  # if you don't group you loose info (lumps all of them)
  summarise(across(c(test_u5:conf_ov5),sum))

### repartition of cases by age

##### at the municipality level

cases_age_admin1 <- clean_routine_data_Admin1 %>%
  group_by(Admin1) %>%
  summarise(across(c(conf_u5,conf_ov5),sum)) %>%
  pivot_longer(cols=c(conf_u5,conf_ov5), # put a col on top of the other
               names_to = "age_group",values_to = "conf",names_prefix = "conf_")

ggplot(cases_age_admin1,
       aes(x=Admin1,y=conf,fill=age_group))+
  geom_col()

##### at the national level
cases_age_national <- clean_routine_data_Admin1 %>%
  ungroup() %>%
  summarise(across(c(conf_u5,conf_ov5),sum)) %>%
  pivot_longer(cols=c(conf_u5,conf_ov5), # put a col on top of the other
               names_to = "age_group",values_to = "confirmed",
               names_prefix = "conf_")

ggplot(cases_age_national,
       aes(x=Admin1,y=confirmed,fill=age_group))+
  geom_col()


### test positivity rate
# confirmed cases are always less than the tested cases
TPR <- clean_routine_data_Admin1 %>%
  mutate(TPR_u5 = conf_u5/test_u5) %>%
  mutate(TPR_ov5 = conf_ov5/test_ov5)

##### load population data
pop <- read.csv('./population.csv',sep=";")

# replace Qaasuitsup with Qasuitsup
pop$Admin1[pop$Admin1 == "Qaasuitsup"] <- "Qasuitsup"

my_pop <- pop #%>%
  #mutate(pop_total = pop_u5+pop_ov5) # find total pop by admin1


### seasonality of incidence

##### in 2018, incidence by municipality
Inc_2018 <- clean_routine_data_Admin1 %>%
  group_by(Admin1)%>% #sum all by admin1
  summarise(across(c(conf_u5,conf_ov5),sum)) %>%
  left_join(my_pop) %>%
  mutate(Inc_u5 = round(conf_u5/pop_u5*1000), 
         Inc_ov5 = round(conf_ov5/pop_ov5*1000)) %>%
  pivot_longer(cols=c(Inc_u5,Inc_ov5), # put a col on top of the other
               names_to = "age_group",values_to = "Incidence",
               names_prefix = "Inc_")

#Plot 2018
#factor() orders your x and y axis
ggplot(Inc_2018,
       aes(x=Admin1,y=Incidence,fill=age_group))+
  geom_col()+
  labs(title = "Incidence for 2018")


##### incidence by month  
Inc_month <- clean_routine_data_Admin1 %>%
  group_by(Admin1,month)%>%
  select(conf_u5,conf_ov5) %>%
  left_join(my_pop) %>%
  mutate(Inc_u5 = round(conf_u5/pop_u5*1000), 
         Inc_ov = round(conf_ov5/pop_ov5*1000))
 # pivot_longer(cols=c(Inc_u5,Inc_ov5), # put a col on top of the other
  #             names_to = "age_group",values_to = "Incidence",
   #            names_prefix = "Inc_")


#Plot by month
#factor() orders your x and y axis
#my_data <- Inc_month %>%
plots <- list()
  subs <- unique(Inc_month$Admin1)


for (i in subs) {
p <- ggplot(Inc_month,
       aes(x=month,y=Inc_u5,fill=age_group))+
  geom_col()+
  labs(title = paste(i),
       x = "Month",
       y = "Incidence") +
  theme_minimal()
plots[[i]] <- p

}

  # Arrange the sub-region plots in a grid
  grid.arrange(grobs = plots, ncol = 2)  # You can adjust ncol for the desired number of columns
  

