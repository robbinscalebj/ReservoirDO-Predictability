library(tidyverse)
library(lubridate)
library(here)
library(hms)

here::i_am("Predictive_Models/Maumelle/maumelle_collate_datasets.R")
#read weather data
df_meteo <- read_csv(here("Predictive_Models/Maumelle/noaa_isd_maumelle.csv"))|>
  rename(datetime = "time")

#read water data
df_water <- read_csv(here("Predictive_Models/Maumelle/maumelle_tidied_data-2021-04-20_2021-12-31.csv"))|>
  filter(datetime == round_date(datetime, unit = "hours"))|>
  filter(datetime >= as_datetime("2021-04-20 05:00:00") & datetime <= as_datetime("2021-12-31 23:00:00"))


#join data
df <- left_join(df_water, df_meteo)|>
  relocate(Depth_m, .after = datetime)|>
  mutate(doy = yday(datetime),
         hour_of_day = hour(datetime))


#set training/validation split variable
validation_set_range_days <- list(seq(120,129), seq(140,149), seq(160,169), seq(180,189), seq(200,209), seq(220,229), seq(240,249), seq(260,269), 
                               seq(280,289), seq(300,309), seq(320,329), seq(340,349), seq(360,365))

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
                                doy %in% validation_set_range_days[[11]] ~ 11,
                                doy %in% validation_set_range_days[[12]] ~ 12,
                                doy %in% validation_set_range_days[[13]] ~ 13))|>
  select(-WindSpeed_m.s, -WindDirection_deg.clwise, -RelHum_per)|>
  rename(Datetime = "datetime", WaterTemp_C = "Temp_C")


#write data set
df2 |> write_csv(here("Predictive_Models/Maumelle/maumelle_data_for_predmodels.csv"))
