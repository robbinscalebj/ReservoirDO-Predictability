
#This script combines the Richland-Chambers "long-term" (2000-2020) data sources in proper formatting to be used for predictive modeling.
#Steps
#- Read data sources
#- Combine data sources
#- Trim data to correct time
#- Write data to external file


library(tidyverse)
library(lubridate)
library(hms)


#Pull in RC weather data
# Pull in inflow data
# Pull in RC observation data
# Pare down to T-10 dates from observation dates

#setwd("C:/Users/Caleb_Robbins/Documents/Richland_Chambers_Modelling/Predictive_Models")


meteo <- read_csv("./RC_UTCmeteo_for_WET.csv", col_names = c("Datetime", "WWind_m.s", "SWind_m.s", "AtmPress_hPa", "AirTemp_C", "DewTemp_C", "CloudCover_prop"))|>
  filter(Datetime > as_datetime("1990-01-01"))


obs <- read_csv("./RC_tidied_observations.csv")%>%
  mutate(TN_mg.L = TKN_mg.L + Nox_mg.L)%>%
  select(Datetime, Depth_m, Depth_TMB, chla_ug.L, Temp_C, NH4_mg.L, Nox_mg.L, TP_mg.L, TN_mg.L, DO_mg.L)%>%
  mutate(Datetime = round_hms(Datetime, 3600)) #round times to nearest hour

#Do data with all meteo times
meteo_full_data <- obs|>select(Datetime, DO_mg.L, Depth_m, Temp_C)|>
  mutate(Depth_m = -1*Depth_m)|>
  filter(Depth_m<=10)|>
  right_join(meteo)|>
  rename(WaterTemp_C = "Temp_C", DewPoint_C = "DewTemp_C", Pressure_hPa = "AtmPress_hPa", Cloud_cov_fraction = "CloudCover_prop")|>
  #select(-chla_ug.L,-NH4_mg.L,-Nox_mg.L,-TP_mg.L, -TN_mg.L, -Depth_TMB, -datetime_obs)|>
  mutate(doy = yday(Datetime),
         time_of_day = as_hms(Datetime)) |>
  filter(year(Datetime)>=1990)|> #reservoir full
  relocate(doy,time_of_day)

#DO without all meteo time lags
tidy_obs <- obs%>%
  mutate(datetime_obs = Datetime)%>%
  #filter(Depth_m >-0.9)%>%
  #select(Datetime, datetime_obs, WaterTemp_obs)%>%
  right_join(meteo)|>
  filter(!is.na(DO_mg.L))|>
    rename(WaterTemp_C = "Temp_C", DewPoint_C = "DewTemp_C", Pressure_hPa = "AtmPress_hPa", Cloud_cov_fraction = "CloudCover_prop")|>
  mutate(Depth_m = -1*Depth_m)|>
  filter(Depth_m<=10, is.na(Depth_TMB))|>
  select(-chla_ug.L,-NH4_mg.L,-Nox_mg.L,-TP_mg.L, -TN_mg.L, -Depth_TMB, -datetime_obs)|>
  mutate(doy = yday(Datetime),
         time_of_day = as_hms(Datetime)) |>
  filter(year(Datetime)>=1990)|> #reservoir full
  relocate(doy,time_of_day)


#write .csv's
tidy_obs |> mutate(Datetime = as.character(Datetime))|> write_csv("RC_pred_longterm_training.csv")

meteo_full_data |> mutate(Datetime = as.character(Datetime))|> write_csv("RC_pred_longterm_training_fullmeteo.csv")


 