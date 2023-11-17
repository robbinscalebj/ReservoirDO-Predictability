library(tidyverse)
library(lubridate)
library(here)
library(hms)

here::i_am("Predictive_Models/Fayetteville/fayetteville_collate_datasets.R")
#read weather data
df_meteo <- read_csv(here("Predictive_Models/Fayetteville/noaa_isd_fayetteville.csv"))|>
  rename(datetime = "time")

#read water data
df_water <- read_csv(here("Predictive_Models/Fayetteville/fayetteville_tidied_data-2021-03-22_2021-09-26.csv"))


#join data
df <- left_join(df_water, df_meteo)|>
  relocate(Depth_m, .after = datetime)|>
  mutate(doy = yday(datetime),
         hour_of_day = hour(datetime))


#set training/validation split variable
validation_set_range_days <- list(seq(80,89), seq(100,109), seq(120,129), seq(140,149), seq(160,169), seq(180,189), seq(200,209), seq(220,229), seq(240,249), seq(260,269))

df2 <- df|>
  mutate(val_period = case_when(doy %in% validation_set_range_days[[1]] ~ 1,
                                doy %in% validation_set_range_days[[2]] ~ 2,
                                doy %in% validation_set_range_days[[3]] ~ 3,
                                doy %in% validation_set_range_days[[4]] ~ 4,
                                doy %in% validation_set_range_days[[5]] ~ 5,
                                doy %in% validation_set_range_days[[6]] ~ 6,
                                doy %in% validation_set_range_days[[7]] ~ 7,
                                doy %in% validation_set_range_days[[8]] ~ 8,
                                doy %in% validation_set_range_days[[9]] ~ 9,
                                doy %in% validation_set_range_days[[10]] ~ 10,
                                #doy %in% validation_set_range_days[[11]] ~ 11,
                                #doy %in% validation_set_range_days[[12]] ~ 12,
                                #doy %in% validation_set_range_days[[13]] ~ 13
                                ))|>
  select(-WindSpeed_m.s, -WindDirection_deg.clwise, -RelHum_per)|>
  rename(Datetime = "datetime", WaterTemp_C = "Temp_C")

df2|>filter(is.na(val_period))|>nrow() #9792
df2|>filter(!is.na(val_period))|>nrow() #9738
#write data set
df2 |> write_csv(here("Predictive_Models/Fayetteville/fayetteville_data_for_predmodels.csv"))
