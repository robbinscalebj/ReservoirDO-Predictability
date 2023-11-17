#This script 
# 1) Combines the Richland-Chambers "short-term" (2021-2022) data sources in proper formatting to be used for predictive modeling. 
# 2) Splits the 2021 data into training and validation periods
# 2a) this must respect the stratification of sampling efforts - Depth, important time periods

library(lubridate)
library(hms)

setwd("C:/Users/Caleb_Robbins/Documents/Richland_Chambers_Modelling/Predictive_Models")
#read meteorological data
cors_isd <- read_csv("Corsicana_ISD_Data_for_preds.csv")%>%
  rename(datetime_isd_reading = "date") |>
  mutate(Cloud_cov_fraction = cl_1/8, .keep = "unused")%>%
  #cols must be named: time, Clouds,AirTemp,RelHum,WindSpeed, Rainfall
  select(datetime_isd_reading,dew_point,Cloud_cov_fraction,ws,wd, air_temp, atmos_pres,RH)%>%
  rename(AirTemp_C = "air_temp", DewPoint_C = "dew_point", RelHum_per = "RH", 
         Pressure_hPa = "atmos_pres", WindSpeed_m.s = "ws", 
         WindDirection_deg.clwise = "wd")%>%
  mutate(WWind_m.s = WindSpeed_m.s*-sin(WindDirection_deg.clwise*(pi/180)),
         SWind_m.s = WindSpeed_m.s*-cos(WindDirection_deg.clwise*(pi/180)))

#read profiler data
profiler_dat <- read_csv("RC_Live_Dataframe_for_preds.csv",col_types = cols("Datetime" = col_datetime(), profileId = 'c', "Attribute" = 'c', .default = 'n'))|>
  mutate(datetime_isd_reading = round_date(Datetime, "hour"))|> #datetime is rounded to nearest hour to match up with isd data
  #join isd data 
  left_join(cors_isd)|>
 # select(-datetime_isd_reading)|>
  relocate(datetime_isd_reading)|>
  select(-Depth_m, -SpCond_uS.cm, -pH, -DO_per.sat, -ORP_mv, -Turbidity_NTU, -Chla, -`Air Temp C`, -`Precip mm`, -`Windspeed M/s`, -`Wind Dir`, 
         -`Humidity %`,-`Abs Press kPa`,-`Attribute`,-`Global Radiation`)|>
  #trim down data to 2021 and get day of year to use for indexing to create training/validation datasets
  group_by(profileId)|> #ensure whole profiles still stick together
  mutate(doy = yday(first(Datetime)),
         time_of_day = as_hms(Datetime))|>
  filter(year(Datetime)==2021)|>
  relocate(doy,time_of_day)|>
  rename(Depth_m = "Payout_m", WaterTemp_C = "Temp_C")|>
  select(-datetime_isd_reading, -WindSpeed_m.s, -WindDirection_deg.clwise, -RelHum_per)


############################################################################
  #  
  
#Training set vs validation set = set DO to NA every other 10 days - want 10 days because that's max 'near-term' timeframe where we want consistent data 
#create grouping variable - want to be able to count 10 days

validation_set_range_days <- c(seq(120,129), seq(140,149), seq(160,169), seq(180,189), seq(200,209), seq(220,229), seq(240,249), seq(260,269), 
                             seq(280,289), seq(300,309), seq(320,329), seq(340,349), seq(360,369))

validation_data <- profiler_dat|>
  mutate(DO_mg.L = ifelse(doy %in% validation_set_range_days, DO_mg.L, NA))

training_data <- profiler_dat|>
  mutate(DO_mg.L = ifelse(!(doy %in% validation_set_range_days), DO_mg.L, NA))
  
  

training_data |> write_csv("RC_pred_highfreq_training.csv")
  
validation_data|> write_csv("RC_pred_validation.csv")

p1<-ggplot(data = validation_data, aes(x = Datetime, y = DO_mg.L))+
  geom_point()+xlab("Datetime")+ylab("DO (mg/L)")+ggtitle("Validation")+scale_y_continuous(limits = c(0,17))
p2<-ggplot(data = training_data, aes(x = Datetime, y = DO_mg.L))+
  geom_point()+xlab("Datetime")+ylab("DO (mg/L)")+ggtitle("Training")+scale_y_continuous(limits = c(0,17))

plot_grid(p2,p1)

