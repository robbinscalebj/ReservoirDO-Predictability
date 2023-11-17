library(tidyverse)
library(here)
library(tidymodels)

here::i_am("Predictive_Models/Fayetteville/fayetteville_randfor_recipes.R")

# read data
fayetteville_df <- read_csv(here("Predictive_Models/Fayetteville/fayetteville_data_for_predmodels.csv"))


#created lagged predictors - there is no good way to do this (basically,grouped lagging) within tidymodels recipes
rec_lags <- fayetteville_df|>
  group_by(Depth_m)|>
  arrange(Datetime)|>
  mutate(across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 1L), .names = "{.col}_lag2hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 3L), .names = "{.col}_lag6hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 6L), .names = "{.col}_lag12hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 12L), .names = "{.col}_lag24hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 18L), .names = "{.col}_lag36hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 24L), .names = "{.col}_lag48hr"),
         across(c(WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s), ~lag(., n = 36L), .names = "{.col}_lag72hr"))|>
  mutate(validation_period = as_factor(if_else(is.na(val_period), "training", "testing")))
###########
### Random forest recipes
## Additive structure (no interactions)
# No lag

lag0_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s)|>
  drop_na(DO_mg.L,WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s)

lag0_split <- initial_split(lag0_df) 
lag0_split$in_id <- which(lag0_df$validation_period == "training")
lag0_training <- training(lag0_split)
lag0_testing <- testing(lag0_split)

rec_lag0<-recipe(DO_mg.L ~ ., data = lag0_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 2 hours
lag2_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag2hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag2_split <- initial_split(lag2_df) 
lag2_split$in_id <- which(lag2_df$validation_period == "training")
lag2_training <- training(lag2_split)
lag2_testing <- testing(lag2_split)

rec_lag2<-recipe(DO_mg.L ~ ., data = lag2_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 6 hours
lag6_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag6hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag6_split <- initial_split(lag6_df) 
lag6_split$in_id <- which(lag6_df$validation_period == "training")
lag6_training <- training(lag6_split)
lag6_testing <- testing(lag6_split)

rec_lag6<-recipe(DO_mg.L ~ ., data = lag6_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 12 hours
lag12_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag12hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag12_split <- initial_split(lag12_df) 
lag12_split$in_id <- which(lag12_df$validation_period == "training")
lag12_training <- training(lag12_split)
lag12_testing <- testing(lag12_split)

rec_lag12<-recipe(DO_mg.L ~ ., data = lag12_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 24 hours
lag24_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag24hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag24_split <- initial_split(lag24_df) 
lag24_split$in_id <- which(lag24_df$validation_period == "training")
lag24_training <- training(lag24_split)
lag24_testing <- testing(lag24_split)

rec_lag24<-recipe(DO_mg.L ~ ., data = lag24_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 48 hours
lag48_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag48hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag48_split <- initial_split(lag48_df) 
lag48_split$in_id <- which(lag48_df$validation_period == "training")
lag48_training <- training(lag48_split)
lag48_testing <- testing(lag48_split)

rec_lag48<-recipe(DO_mg.L ~ ., data = lag48_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())
# Lag 72 hours
lag72_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag72hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag72_split <- initial_split(lag72_df) 
lag72_split$in_id <- which(lag72_df$validation_period == "training")
lag72_training <- training(lag72_split)
lag72_testing <- testing(lag72_split)

rec_lag72<-recipe(DO_mg.L ~ ., data = lag72_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

##########

## Interaction recipes

#no lag

lag0_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s)|>
  drop_na(DO_mg.L,WaterTemp_C,Cloud_cov_fraction,DewPoint_C,AirTemp_C,Pressure_hPa, WWind_m.s,SWind_m.s)

lag0_int_split <- initial_split(lag0_int_df) 
lag0_int_split$in_id <- which(lag0_int_df$validation_period == "training")
lag0_int_training <- training(lag0_int_split)
lag0_int_testing <- testing(lag0_int_split)

rec_int_lag0<-recipe(DO_mg.L ~ ., data = lag0_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(WaterTemp_C+Pressure_hPa+Cloud_cov_fraction+AirTemp_C+WWind_m.s+SWind_m.s+DewPoint_C))|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 2 hours
lag2_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag2hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag2_int_split <- initial_split(lag2_int_df) 
lag2_int_split$in_id <- which(lag2_int_df$validation_period == "training")
lag2_int_training <- training(lag2_int_split)
lag2_int_testing <- testing(lag2_int_split)

rec_int_lag2<-recipe(DO_mg.L ~ ., data = lag2_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 6 hours
lag6_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag6hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag6_int_split <- initial_split(lag6_int_df) 
lag6_int_split$in_id <- which(lag6_int_df$validation_period == "training")
lag6_int_training <- training(lag6_int_split)
lag6_int_testing <- testing(lag6_int_split)

rec_int_lag6<-recipe(DO_mg.L ~ ., data = lag6_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 12 hours
lag12_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag12hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag12_int_split <- initial_split(lag12_int_df) 
lag12_int_split$in_id <- which(lag12_int_df$validation_period == "training")
lag12_int_training <- training(lag12_int_split)
lag12_int_testing <- testing(lag12_int_split)

rec_int_lag12<-recipe(DO_mg.L ~ ., data = lag12_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 24 hours
lag24_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag24hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag24_int_split <- initial_split(lag24_int_df) 
lag24_int_split$in_id <- which(lag24_int_df$validation_period == "training")
lag24_int_training <- training(lag24_int_split)
lag24_int_testing <- testing(lag24_int_split)

rec_int_lag24<-recipe(DO_mg.L ~ ., data = lag24_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())

# Lag 48 hours
lag48_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag48hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag48_int_split <- initial_split(lag48_int_df) 
lag48_int_split$in_id <- which(lag48_int_df$validation_period == "training")
lag48_int_training <- training(lag48_int_split)
lag48_int_testing <- testing(lag48_int_split)

rec_int_lag48<-recipe(DO_mg.L ~ ., data = lag48_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())
# Lag 72 hours
lag72_int_df<- rec_lags|>
  select(doy, hour_of_day, Depth_m, DO_mg.L, val_period, validation_period,Datetime,contains("lag72hr"))|>
  drop_na(DO_mg.L,contains("lag"))

lag72_int_split <- initial_split(lag72_int_df) 
lag72_int_split$in_id <- which(lag72_int_df$validation_period == "training")
lag72_int_training <- training(lag72_int_split)
lag72_int_testing <- testing(lag72_int_split)

rec_int_lag72<-recipe(DO_mg.L ~ ., data = lag72_int_training)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_interact(terms = ~Depth_m*hour_of_day*(contains("lag")))|>
  step_normalize(all_numeric(), -all_outcomes())