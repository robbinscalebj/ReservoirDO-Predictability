library(tidyverse)
library(tidymodels)
library(lubridate)
library(here)
library(vip)
library(ranger)
library(doParallel)


here::i_am("Predictive_Models/EM/em_randfor_train.R")

source(here("Predictive_Models/EM/em_randfor_recipes.R")) #reads data and recipes

################

# Train Random Forest models 

tune_randfor <- rand_forest(
  mtry = tune(),
  trees = 500, 
  min_n = tune()) |>
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity", seed = 123) 
#setting seed ensures parallel workers working from same seed, basically makes collect_predictions() match predict() on backend


########################### Workflow Sets
randfor_lag0_models <- workflow_set(
  preproc = list(lag0 = rec_lag0),
  models = list(randfor = tune_randfor))

randfor_lag2_models <- workflow_set(
  preproc = list(lag2 = rec_lag2),
  models = list(randfor = tune_randfor))

randfor_lag6_models <- workflow_set(
  preproc = list(lag6 = rec_lag6),
  models = list(randfor = tune_randfor))

randfor_lag12_models <- workflow_set(
  preproc = list(lag12 = rec_lag12),
  models = list(randfor = tune_randfor))

randfor_lag24_models <- workflow_set(
  preproc = list(lag24 = rec_lag24),
  models = list(randfor = tune_randfor))


randfor_lag48_models <- workflow_set(
  preproc = list(lag48 = rec_lag48),
  models = list(randfor = tune_randfor))

randfor_lag72_models <- workflow_set(
  preproc = list(lag72 = rec_lag72),
  models = list(randfor = tune_randfor))


randfor_intlag0_models <- workflow_set(
  preproc = list(intlag0 = rec_int_lag0),
  models = list(randfor = tune_randfor))

randfor_intlag2_models <- workflow_set(
  preproc = list(intlag2 = rec_int_lag2),
  models = list(randfor = tune_randfor))

randfor_intlag6_models <- workflow_set(
  preproc = list(intlag6 = rec_int_lag6),
  models = list(randfor = tune_randfor))

randfor_intlag12_models <- workflow_set(
  preproc = list(intlag12 = rec_int_lag12),
  models = list(randfor = tune_randfor))

randfor_intlag24_models <- workflow_set(
  preproc = list(intlag24 = rec_int_lag24),
  models = list(randfor = tune_randfor))


randfor_intlag48_models <- workflow_set(
  preproc = list(intlag48 = rec_int_lag48),
  models = list(randfor = tune_randfor))

randfor_intlag72_models <- workflow_set(
  preproc = list(intlag72 = rec_int_lag72),
  models = list(randfor = tune_randfor))



######################## Tune models
set.seed(123)
cl <- makePSOCKcluster(16)
doParallel::registerDoParallel(cl)
randfor_lag0_tuned <- randfor_lag0_models|>
  workflow_map(verbose = TRUE, seed=123, "tune_grid", control = control_grid(save_pred= TRUE), 
               resamples = vfold_cv(lag0_training, v = 10, repeats = 5, strata = Depth_m), grid = 20) 
doParallel::registerDoParallel(cl)
randfor_lag2_tuned <- randfor_lag2_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag2_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_lag6_tuned <- randfor_lag6_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag6_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_lag12_tuned <- randfor_lag12_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag12_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_lag24_tuned <- randfor_lag24_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag24_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_lag48_tuned <- randfor_lag48_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag48_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_lag72_tuned <- randfor_lag72_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag72_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)


doParallel::registerDoParallel(cl)
randfor_intlag0_tuned <- randfor_intlag0_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag0_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag2_tuned <- randfor_intlag2_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag2_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag6_tuned <- randfor_intlag6_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag6_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag12_tuned <- randfor_intlag12_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag12_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag24_tuned <- randfor_intlag24_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag24_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag48_tuned <- randfor_intlag48_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag48_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)
doParallel::registerDoParallel(cl)
randfor_intlag72_tuned <- randfor_intlag72_models|>
  workflow_map(verbose = TRUE, "tune_grid", resamples = vfold_cv(lag72_int_training, v = 10, repeats = 5, strata = Depth_m), grid = 20)

#### Save tuning sets
randfor_mods <- list(randfor_lag0_tuned,randfor_lag2_tuned,randfor_lag6_tuned,randfor_lag12_tuned,
                     randfor_lag24_tuned,randfor_lag48_tuned,randfor_lag72_tuned,randfor_intlag0_tuned,
                     randfor_intlag2_tuned,randfor_intlag6_tuned,randfor_intlag12_tuned,
                     randfor_intlag24_tuned,randfor_intlag48_tuned,randfor_intlag72_tuned)

saveRDS(randfor_mods, file = here("Predictive_Models/EM/randfor_mods.Rds"))

## to read back in, 
#randfor_mods <- readRDS(here("Predictive_Models/EM/randfor_mods.Rds"))
#randfor_names <- map(randfor_mods, pluck(1))|>unlist()|>str_extract("[^_]+")
#randfor_names <- paste0("randfor_", randfor_names, "_tuned")
#names(randfor_mods) <- randfor_names
#list2env(randfor_mods, envir = globalenv())

########################### Extract workflows
#combine all the *_tuned workflow_map objects and select best


randfor_lag0_wf_best <- randfor_lag0_tuned|>extract_workflow_set_result("lag0_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag0_randfor")
randfor_lag2_wf_best <- randfor_lag2_tuned|>extract_workflow_set_result("lag2_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag2_randfor")
randfor_lag6_wf_best <- randfor_lag6_tuned|>extract_workflow_set_result("lag6_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag6_randfor")
randfor_lag12_wf_best <- randfor_lag12_tuned|>extract_workflow_set_result("lag12_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag12_randfor")
randfor_lag24_wf_best <- randfor_lag24_tuned|>extract_workflow_set_result("lag24_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag24_randfor")
randfor_lag48_wf_best <- randfor_lag48_tuned|>extract_workflow_set_result("lag48_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag48_randfor")
randfor_lag72_wf_best <- randfor_lag72_tuned|>extract_workflow_set_result("lag72_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "lag72_randfor")


randfor_intlag0_wf_best <- randfor_intlag0_tuned|>extract_workflow_set_result("intlag0_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag0_randfor")
randfor_intlag2_wf_best <- randfor_intlag2_tuned|>extract_workflow_set_result("intlag2_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag2_randfor")
randfor_intlag6_wf_best <- randfor_intlag6_tuned|>extract_workflow_set_result("intlag6_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag6_randfor")
randfor_intlag12_wf_best <- randfor_intlag12_tuned|>extract_workflow_set_result("intlag12_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag12_randfor")
randfor_intlag24_wf_best <- randfor_intlag24_tuned|>extract_workflow_set_result("intlag24_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag24_randfor")
randfor_intlag48_wf_best <- randfor_intlag48_tuned|>extract_workflow_set_result("intlag48_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag48_randfor")
randfor_intlag72_wf_best <- randfor_intlag72_tuned|>extract_workflow_set_result("intlag72_randfor")|>collect_metrics()|>
  group_by(.metric)|>arrange(mean, .by_group = TRUE)|>ungroup()|>filter(.config == pluck(.config[1]))|>mutate(wflow_id = "intlag72_randfor")





randfor_best_results <- bind_rows(randfor_lag0_wf_best,randfor_lag2_wf_best,randfor_lag6_wf_best,randfor_lag12_wf_best,
                                  randfor_lag24_wf_best,randfor_lag48_wf_best,randfor_lag72_wf_best,
                                  randfor_intlag0_wf_best, randfor_intlag2_wf_best,randfor_intlag6_wf_best,
                                  randfor_intlag12_wf_best, randfor_intlag24_wf_best, randfor_intlag48_wf_best)


randfor_predselect_plot <- randfor_best_results|>
  mutate(.metric = fct_recode(.metric, `R-squared` = "rsq", RMSE = "rmse"),
         wflow_id = as_factor(wflow_id),
         wflow_id = fct_reorder2(wflow_id, desc(.metric), desc(mean)))|>
  ggplot(aes(x = wflow_id, y = mean))+
  geom_point()+
  geom_errorbar(aes(ymin = mean-(1.96*std_err), ymax = mean+(1.96*std_err)))+
  facet_wrap(.~.metric, scales = "free")+
  xlab("Model ID")+
  ylab("Metric Score")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("eagle mountain hyperparameter tuning")

randfor_predselect_plot
#best is lag0 with no interaction

saveRDS(randfor_lag0_tuned, file = here("Predictive_Models/EM/randfor_tuneresults_lag0.Rds"))
randfor_lag0_tuned <- readRDS(here("Predictive_Models/EM/randfor_tuneresults_lag0.Rds"))
randfor_best <- randfor_lag0_tuned |> #MAKE SELECTION BASED ON ABOVE PROCEDURE
  extract_workflow_set_result("lag0_randfor")|>
  select_best(metric = "rmse")
# mtry = 5, min_n = 4

randfor_final <- randfor_lag0_tuned|>
  extract_workflow("lag0_randfor")|>
  finalize_workflow(randfor_best)


final_randfor <- last_fit(randfor_final, lag0_split)
final_vi<-final_randfor|>extract_fit_parsnip()|>vip(num_features = 80)|>pluck("data")
write_csv(final_vi, here("Predictive_Models/EM/randfor_lag0_vip_scores.csv"))
saveRDS(final_randfor, here("Predictive_Models/EM/randfor_fit.Rds"))
final_randfor <- readRDS(here("Predictive_Models/EM/randfor_fit.Rds"))


#For testing only
randfor_data <- final_randfor[[1]][[1]][["data"]]

final_randfor_metrics <- final_randfor |> 
  collect_metrics()

final_randfor_preds <- final_randfor |>
  collect_predictions()|>
  bind_cols(randfor_data|>select(-DO_mg.L)|>filter(!is.na(val_period))) #filters to observations where val_period is given


final_randfor_preds |> 
  ggplot()+
  geom_point(aes(x = .pred, y = DO_mg.L))+
  ylab("Observed DO (mg/L)")+
  xlab("Predicted DO (mg/L)")+
  coord_obs_pred()

final_randfor_preds|>
  filter(Depth_m == 0)|>
  ggplot()+
  geom_point(aes(x = Datetime, y = .pred))+
  geom_line(aes(x = Datetime, y = DO_mg.L))+
  ylab("DO (mg/L)")+
  xlab("Datetime")+
  facet_wrap(.~val_period, scales = "free")

rmse(final_randfor_preds, truth = DO_mg.L, estimate = .pred)
