# Generate sensitivity analysis figures

library(tidyverse)
library(tidymodels)
library(here)
# Retrieve data
## RC
size_rc <-read_csv(here("Predictive_Models/RC_training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Richland-Chambers")
sp_preds_rc <- read_csv(here("Predictive_Models/RC_split_switch_sensitivity_preds.csv"))|>
  mutate(lake = "Richland-Chambers")
sp_rc <- read_csv(here("Predictive_Models/RC_split_switch_sensitivity_results.csv"))|>
  mutate(lake = "Richland-Chambers")
ds_rc<-read_csv(here("Predictive_Models/RC_downsampling_sensitivity_results.csv"))

## EM
size_em<-read_csv(here("Predictive_Models/EM/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Eagle Mountain")
sp_preds_em <- read_csv(here("Predictive_Models/EM/split_switch_sensitivity_preds.csv"))|>
  mutate(lake = "Eagle Mountain")
sp_em <- read_csv(here("Predictive_Models/EM/split_switch_sensitivity_results.csv"))|>
  mutate(lake = "Eagle Mountain")
ds_em <- read_csv(here("Predictive_Models/EM/downsampling_sensitivity_results.csv"))

## Fay
size_fay<-read_csv(here("Predictive_Models/Fayetteville/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Fayetteville")
sp_preds_fay <- read_csv(here("Predictive_Models/Fayetteville/split_switch_sensitivity_preds.csv"))|>
  mutate(lake = "Fayetteville")
sp_fay <- read_csv(here("Predictive_Models/Fayetteville/split_switch_sensitivity_results.csv"))|>
  mutate(lake = "Fayetteville")
### no downsampling for fayetteville, because its data is the target for downsampling

## Maumelle
size_maum <- read_csv(here("Predictive_Models/Maumelle/training_data_size_sensitivity_results.csv"))|>
  mutate(lake = "Maumelle")
sp_preds_maum <- read_csv(here("Predictive_Models/Maumelle/split_switch_sensitivity_preds.csv"))|>
  mutate(lake = "Maumelle")
sp_maum <- read_csv(here("Predictive_Models/Maumelle/split_switch_sensitivity_results.csv"))|>
  mutate(lake = "Maumelle")
ds_maum <- read_csv(here("Predictive_Models/Maumelle/downsampling_sensitivity_results.csv"))

## join data
test1.j <- bind_rows(size_rc,size_maum, size_fay, size_em)

# Test 1 - How does training data size influence model accuracy on the original test data set? 

ggplot()+
  geom_point(data = test1.j, aes(x = split_percent, y = rmse_on_test_set))+
  geom_point(data = test1.j|>group_by(split_percent,lake)|>summarize(mean_rmse = mean(rmse_on_test_set)),
             aes(x = split_percent, y = mean_rmse), size = 6, alpha = 0.5, color = "red")+
  scale_y_continuous(trans = "reverse")+
  xlab("Training Data Used for Fitting (%)")+
  ylab("RMSE on Test Set")+
  theme_bw()+
  facet_wrap(.~lake)


# Test 2 - How does choice of testing/training split influence model accuracy? 
## By depth overall
test2_dep.j <- bind_rows(sp_maum, sp_rc, sp_fay,sp_em)|>
  mutate(split_diff = rmse_switched_split - rmse_original_split)


ggplot(data = test2_dep.j)+
  geom_point(aes(x = Depth_m, y = split_diff))+
  facet_wrap(.~lake)

ggplot(data = test2_dep.j)+
  geom_hline(aes(yintercept = 0), linewidth = 2, color = "red")+
  geom_boxplot(aes(x = lake, y = split_diff), width = 0.25, linewidth = 1)+
  geom_point(aes(x = lake, y = split_diff, color = Depth_m), size = 2)+
  theme_bw()+
  ylab("Switched split RMSE - Original RMSE")+
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12, face = "bold"))

# recapitulate key figure (Fig 4) for comparison
test2.j <- bind_rows(sp_preds_maum, sp_preds_rc, sp_preds_fay,sp_preds_em)|>
  mutate(val_period = as_factor(val_period),
         lake = as_factor(lake),
         season = case_when(lake == "Eagle Mountain" & doy %in% c(170:250) ~ "Warm",
                            lake == "Richland-Chambers" & doy %in% c(170:250) ~ "Warm",
                            lake == "Fayetteville" & doy %in% c(110:250) ~ "Warm",
                            lake == "Maumelle" & doy %in% c(150:290) ~ "Warm",
                            .default = "Cool"),
         depth_cat = case_when(lake == "Eagle Mountain" & Depth_m == 10 ~ "Bottom",
                               lake == "Richland-Chambers" & Depth_m == 10 ~ "Bottom",
                               lake == "Fayetteville" & Depth_m == 8.5 ~ "Bottom",
                               lake == "Maumelle" & Depth_m == 9 ~ "Bottom",
                               Depth_m == 0 ~ "Surface",
                               Depth_m == 5 ~ "Middle"),
         depth_cat = as_factor(depth_cat),
         depth_cat = fct_relevel(depth_cat, "Surface", "Middle", "Bottom"))

test2.j_rmse <- test2.j |>
  filter(!is.na(depth_cat))|>
  group_by(lake, doy, season, depth_cat)|>
  rmse(truth = DO_mg.L, estimate = .pred)#|>
  #rename(rmse = ".estimate")

res_rmse_df <- test2.j_rmse|>rename(rmse = ".estimate")|>filter(!is.na(depth_cat))|>
  mutate(corr_grouping = as_factor(str_c(lake,season,depth_cat,sep = "_")),
         doy = yday(Datetime),
         corr_group2 = str_c(lake,depth_cat,val_period, sep="_"))|>
  filter(doy !=1)|> #gets rid of a couple RC time points that happened to be just after the New Year - artefact of converting to UTC I suppose
  group_by(lake,season,val_period, depth_cat, doy, corr_grouping, corr_group2)|>
  summarize(rmse = sqrt(sum(rmse^2)/n()))|>
  drop_na()

m1 <- nlme::gls(data = test2.j_rmse,rmse ~  season*lake*depth_cat,
                correlation = corAR1(form = ~doy|corr_group2),
                weights = varIdent(form = ~1|lake*season*depth_cat), 
                control = lmeControl(opt = "optim", optimMethod = "L-BFGS-B"))


theme_fig4 <- theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14,face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = c(0.85,0.8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())  

p <- ggplot()+
  geom_jitter(data = test2.j_rmse, aes(x = season, y = rmse, color = depth_cat), alpha = 0.3)+
 # geom_point(data = m1_em_tidy, aes(x = season, y = estimate, color = depth_cat),color = "black",size = 2.5, alpha = 0.9)+
  #geom_errorbar(data = m1_em_tidy,aes(x = season, ymin = conf.low, ymax = conf.high, group = depth_cat),  color = "black", width = 0.3)+
  scale_color_jco()+
  ylab("Swithced Split RMSE")+
  facet_grid(factor(depth_cat, levels= c("Surface", "Middle", "Bottom"))~factor(lake, levels= c("Eagle Mountain", "Richland-Chambers", "Fayetteville", "Maumelle")), scales = "free")+
  #scale_y_discrete(breaks = levels(res_rmse_df$lake),labels = levels(res_rmse_df$lake))+
  theme_fig4+
  guides(color = "none")+
  xlab("Season")
p  
  
