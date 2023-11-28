# Libraries and set up
library(tidyverse)
library(here)

here::i_am("Predictive_Models/Maumelle/contour-plot.R")

source(here("Predictive_Models/Maumelle/retrieve_maum_data.R")) # read in data retrieval function

#Desired dates
Begin_date= "2023-10-01"
End_date = "2023-11-27"
# Get data
maum_profiles <- retrieve_maum_data(Begin_date = Begin_date, End_date = End_date)
#these data are read in US/Central

#set color palette function and plotting theme
color_ramp <- grDevices::colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb")
depth_breaks <- c(0,2,4,6,8,9)
date_limits <- c(Begin_date, End_date)|>as_datetime()
plot_theme <- theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 12, face = "bold"),
        legend.key.height = unit(1.75, 'cm'), legend.key.width = unit(0.75, "cm"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

#temp contour plot
temp.p<-ggplot(data = maum_profiles|>filter(!is.na(Temp_C)), aes(x = datetime, y = Depth_m))+
  geom_raster(aes(fill = Temp_C), interpolate = TRUE)+
  scale_fill_gradientn(name = "Temp (C)", colors = color_ramp(20))+
  ylab("Depth (m)")+
  scale_x_datetime(expand=c(0,0), date_breaks = '10 days', 
                   limits = date_limits, timezone = "US/Central", date_labels = "%b-%d") + 
  scale_y_continuous(expand=c(0,0), breaks = depth_breaks, trans = "reverse")+
  guides(title = "blah")+
  #ggtitle("Temperature (C) May 1 - Dec 1, 2022")+
  plot_theme
temp.p
