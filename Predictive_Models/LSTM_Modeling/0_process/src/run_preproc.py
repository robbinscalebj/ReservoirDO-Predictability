# coding: utf-8
from preprocess_data import *
x_vars = ['doy', 'time_of_day', 'Depth_m', 'WaterTemp_C', 'WWind_m.s', 'SWind_m.s', 'Pressure_hPa', 'AirTemp_C', 'DewPoint_C', 'Cloud_cov_fraction']
df = preprocess_data("../in/RC_pred_highfreq_training.csv", x_vars, 'Datetime', 'Depth_m', '')
