#This script provides the code to build linear (and extension) predictive models on long-term Richland Chambers data. 

#Data were created in Collate_LongTerm_Data.R - read XXX.csv
#Validation data
library(tidymodels)
library(tidyverse)

#load data
RC <- read_csv("Predictive_Models/RC_pred_longterm_training_fullmeteo.csv")



#data preprocessing - "feature engineering"

rc_lm_rec <- recipe(DO_mg.L ~ ., data = RC)|>
  step_rm("Datetime","WaterTemp_C")|>
  step_lag(-doy, -time_of_day, -Depth_m, -DO_mg.L, lag = c(1,6,12,24,36,48,72))|>
  prep()
df <- juice(rc_lm_rec)


#set engine - very easy in this case
lm_spec <- linear_reg() |> 
  set_engine("lm")


lm_fit <- lm_spec |>
  fit(DO_mg.L ~.,
      data = juice(rc_lm_rec))

lm_fit

mc_cv(juice(rc_lm_rec))
