#Original depth in feet
#Do is mg/L
#Temp is C
#Datetime is CDT
library(tidyverse)
library(lubridate)
library(here)

here::i_am("Predictive_Models/Fayetteville/fayetteville_df_retrieve-tidy.R")


temp_df <- read_delim(here("Predictive_Models/Fayetteville/fayetteville_raw_temp.txt"))|>
  rename(DateTime = "...1")|>
  pivot_longer(cols = -DateTime, names_to = "Depth_m", values_to = "Temp_C")|>
  mutate(datetime = as_datetime(DateTime, format = "%m/%d/%Y %H:%M"), .keep = "unused")

do_df <- read_delim(here("Predictive_Models/Fayetteville/fayetteville_raw_do.txt"))|>
  pivot_longer(cols = -datetime, names_to = "Depth_m", values_to = "DO_mg.L")|>
  mutate(datetime = as_datetime(datetime, format = "%m/%d/%Y %H:%M"),
         Depth_m = str_remove(Depth_m,"doobs_"))

df <- full_join(temp_df,do_df, by = c("datetime", "Depth_m"))|> #good, same size as entries
  mutate(datetime = force_tz(datetime, tzone = "UTC")) #define tz for good practice...


Begin_date = df|>arrange(datetime)|>first()|>pull(datetime)|>as_date()|>str_remove(" UTC")
End_date = df|>arrange(desc(datetime))|>first()|>pull(datetime)|>as_date()|>str_remove(" UTC")


#write data to file

df|>write_csv(here(paste0("Predictive_Models/Fayetteville/fayetteville_tidied_data-", Begin_date,"_", End_date,".csv")))
