library(tidyverse)
library(ggplot2)
library(sf)

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./dataanalysis_onesolution.R")

### Improve plot of incidence by municipality
ggplot(incidence,
       aes(x=month,y=inc_conf*1000,group=Admin1,color=Admin1,
           text=inc_conf))+
  geom_line(size=2)+
  theme_bw()+
  scale_color_manual(values=c("red","purple","green","forestgreen","deeppink"))+
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        legend.position = "bottom",
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title = "Seasonality of Incidence by Municipality",
       y = "Incidence per 1000", color = "Municipality")
 # facet_wrap(Admin1~.) # puts each municipality on a separate plot
  

ggsave("./plot_incidence_Admin1.png",
       width = 10,height=8)

### Map of incidence by municipality
library(sf)

#shapefile: shape of each municipality
map.sf = file.path("./shapefile/GRL_Admin1.shp") %>%
  sf::st_read(quiet = TRUE)
map.sf$Admin1[map.sf$Admin1=="Qaasuitsup"]<-"Qasuitsup"

map.inc <- left_join(incidence2018,map.sf)

ggplot(map.inc)+
  geom_sf(aes(geometry = geometry, fill = inc_conf*1000))+
  labs(fill="Incidence per 1000")+
  xlim(-75, -10)+
  labs(title = "Malaria incidence in Greenland ")+
  theme(plot.title = element_text(hjust = 0.5))+
        scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

########################
### This part is facultative and requires additional packages ###
########################

### Interactive plot: plotly
library(plotly)
library(htmlwidgets) #to save the interactive plots

incidence_Admin1_plot <- ggplot(incidence,
                                aes(x=month,y=inc_conf,group=Admin1,color=Admin1,
                                    text=inc_conf))+
  geom_line()

#transforms ggplot in plotly
incidence_Admin1_plotly <-ggplotly(incidence_Admin1_plot,
                                   tooltip = c("text"))
incidence_Admin1_plotly

#save plot
saveWidget(incidence_Admin1_plotly,"./monthly_incidence_Admin1.html")

### Interactive map of incidence
library(leaflet)

#leaflet requires a specific sf format
map.inc_sf <- sp::merge(map.sf,incidence2018)

#specific color palette: function
palette_leaflet <- colorNumeric(palette="YlOrRd",
                                domain = map.inc_sf$inc_conf)

leaflet(data = map.inc_sf)%>%
  addTiles() %>%
  addPolygons(label = ~Admin1
              , fillColor = ~palette_leaflet(inc_conf)
              , weight = 1
              , opacity = 1.0
              , fillOpacity = 1
              , color = "white") %>%
  addLegend("bottomright"
            , pal = palette_leaflet, values = ~inc_conf
            , title = "Incidence"
            , opacity = 1
  )

