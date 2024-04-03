# Generate sensitivity analysis figures

# Retrieve data
## RC
size_rc <-read_csv(here("Predictive_Models/RC_training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Richland-Chambers")
read_csv(here("Predictive_Models/RC_split_switch_sensitivity_results.csv"))
read_csv(here("Predictive_Models/RC_downsampling_sensitivity_results.csv"))
## EM
size_em<-read_csv(here("Predictive_Models/EM/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Eagle Mountain")

## Fay
size_fay<-read_csv(here("Predictive_Models/Fayetteville/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Fayetteville")

## Maumelle

size_maum<-read_csv(here("Predictive_Models/Maumelle/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Maumelle")
read_csv(here("Predictive_Models/Maumelle/split_switch_sensitivity_results.csv"))
read_csv(here("Predictive_Models/Maumelle/downsampling_sensitivity_results.csv"))

## join data
test1.j <- bind_rows(size_rc,size_maum, size_fay, size_em)

# Test 1

ggplot()+
  geom_point(data = test1.j, aes(x = split_percent, y = rmse_on_test_set))+
  geom_point(data = test1.j|>group_by(split_percent,lake)|>summarize(mean_rmse = mean(rmse_on_test_set)),
             aes(x = split_percent, y = mean_rmse), size = 6, alpha = 0.5, color = "red")+
  scale_y_continuous(trans = "reverse")+
  xlab("Training Data Used for Fitting (%)")+
  ylab("RMSE on Test Set")+
  theme_bw()+
  facet_wrap(.~lake)
