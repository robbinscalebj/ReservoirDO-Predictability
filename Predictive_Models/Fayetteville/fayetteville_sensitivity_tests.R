

library(here)
library(tidyverse)
library(tidymodels)
library(doSNOW)
library(doParallel)
library(tictoc)

####################### SETUP ######################################
# load data and recipes
source(here("Predictive_Models/Fayetteville/fayetteville_randfor_recipes.R"))
# clean up to only necessary objects - best workflow was no lag (lag0) - see [sitename]_randfor_train.R
rm(list = setdiff(ls(), ls(pattern = "lag0_df"))) 

## Set model hyperparameters
### Code from [sitename]_randfor_train.R - load tuning results to extract random forest hyperparameters  
randfor_best <- readRDS(here("Predictive_Models/Fayetteville/randfor_tuneresults_lag0.Rds")) |> 
  extract_workflow_set_result("lag0_randfor")|>
  select_best(metric = "rmse")

mtry <- randfor_best$mtry
min_n <- randfor_best$min_n

#set model conditions
rf_model <- rand_forest(mtry = mtry, min_n = min_n, trees = 500, mode = "regression")|>set_engine("ranger")


################################# TEST 1 #######################################
# Test importance of training data N on test set efficacy



### Randomly split training data - Nested dataframes - testing data is... original testing data to keep the testing set consistent for different data sizes 

split_percents <- c(1,5,10,25,50,75,90)/100
#split_percents <- c(1,5)/100
splits_df <- tibble(split_percent = c(rep(split_percents, 10),1))|> # doing 10 year to get a more precise sense of the variation
  mutate(data = map(split_percent, ~slice_sample(lag0_df|>ungroup()|>filter(validation_period == "training"), by = Depth_m, prop = .x)))|>
  #mutate(length = map(data, nrow))|>
  arrange(split_percent)|>
  mutate(batch_number = row_number())
#slice_head(n = 3) #for testing

testing_data<-lag0_df|>ungroup()|>filter(validation_period == "testing")

# -- LOOP -- 
## setup
#cl <- makePSOCKcluster(5) 
#registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(splits_df), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
batch_names <- splits_df|>pull(split_percent)

## run 
tic()
rf_rmse_results <- foreach(i = 1:nrow(splits_df), .options.snow = opts, 
                           .packages = c("tidyverse", "tidymodels"), .final = function(x) setNames(x, batch_names), .inorder = TRUE) %do% {
                             
                             #subset data 
                             training_data <- splits_df|>filter(batch_number == i)|>select(data)|>unnest(data)
                             
                             #recipe
                             training_recipe<-recipe(DO_mg.L ~ ., data = training_data)|>
                               step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
                               step_normalize(all_numeric(), -all_outcomes())
                             
                             #set up workflow
                             rf_wflow <- workflow()|>
                               add_recipe(training_recipe)|>
                               add_model(rf_model)
                             
                             #fit, predict, and evaluate
                             rf_fit <- parsnip::fit(rf_wflow, training_data)
                             
                             rf_predictions <- predict(rf_fit, testing_data)|>bind_cols(testing_data|>select(DO_mg.L))|>
                               rmse(truth = DO_mg.L, estimate = .pred)
                             
                             list(rf_predictions$.estimate)
                             
                           }

toc() # ~ 1.2 min



rf_rmse_results_trans<-rf_rmse_results|>unlist()|>enframe(name = "split_prop", value = "rmse_on_test_set")|>
  mutate(split_percent = as.numeric(split_prop)*100)

rf_rmse_results_trans|>write_csv(here("Predictive_Models/Fayetteville/training_data_size_sensitivity_results.csv"))
rf_rmse_results_trans<-read_csv(here("Predictive_Models/Fayetteville/training_data_size_sensitivity_results.csv"))

ggplot()+
  geom_point(data = rf_rmse_results_trans, aes(x = split_percent, y = rmse_on_test_set))+
  geom_point(data = rf_rmse_results_trans|>group_by(split_percent)|>summarize(mean_rmse = mean(rmse_on_test_set)),
             aes(x = split_percent, y = mean_rmse), size = 6, alpha = 0.5, color = "red")+
  scale_y_continuous(trans = "reverse")+
  xlab("Training Data Used for Fitting (%)")+
  ylab("RMSE on Test Set")+
  theme_bw()






################################# TEST 2 #######################################

# This test will test how sensitive our results are to switching which split of the data is test vs train.

# Generate original train/test evaluation
training_data_og <- lag0_df|>ungroup()|>filter(validation_period == "training") # Want to test train/test sensitivity

training_recipe_og<-recipe(DO_mg.L ~ ., data = training_data_og)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

## set up workflow
rf_wflow_og <- workflow()|>
  add_recipe(training_recipe_og)|>
  add_model(rf_model)

## fit, predict, and evaluate
rf_fit_og <- parsnip::fit(rf_wflow_og, training_data_og)

testing_data_og <- lag0_df|>ungroup()|>filter(validation_period == "testing")
rf_predictions_og <- predict(rf_fit_og, testing_data_og)|>bind_cols(testing_data_og)|>
  group_by(Depth_m)|>
  #group_by(doy)|>summarize(.pred = mean(.pred), DO_mg.L = mean(DO_mg.L))|>select(.pred, DO_mg.L)|>
  rmse(truth = DO_mg.L, estimate = .pred)|>
  rename(rmse_original_split = ".estimate")|>
  select(-.metric,-.estimator)

# Generate switched  test/train evaluation (i.e., train on original test set)
training_data_sw <- lag0_df|>ungroup()|>filter(validation_period == "testing") # Want to test train/test sensitivity

training_recipe_sw<-recipe(DO_mg.L ~ ., data = training_data_sw)|>
  step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
  step_normalize(all_numeric(), -all_outcomes())

##set up workflow
rf_wflow_sw <- workflow()|>
  add_recipe(training_recipe_sw)|>
  add_model(rf_model)

##fit, predict, and evaluate
rf_fit_sw <- parsnip::fit(rf_wflow_sw, training_data_sw)

testing_data_sw <- lag0_df|>ungroup()|>filter(validation_period == "training")
rf_predictions_sw1 <- predict(rf_fit_sw, testing_data_sw)|>bind_cols(testing_data_sw)
rf_predictions_sw1|>write_csv(here("Predictive_Models/Fayetteville/split_switch_sensitivity_preds.csv"))
rf_predictions_sw <- rf_predictions_sw1|>
  group_by(Depth_m)|>
  #group_by(doy)|>summarize(.pred = mean(.pred), DO_mg.L = mean(DO_mg.L))|>select(.pred, DO_mg.L)|>
  rmse(truth = DO_mg.L, estimate = .pred)|>
  rename(rmse_switched_split = ".estimate")|>
  select(-.metric,-.estimator)


# Combine prediction results to compare
predictions_c <- left_join(rf_predictions_og,rf_predictions_sw)

predictions_c|>write_csv(here("Predictive_Models/Fayetteville/split_switch_sensitivity_results.csv"))


#################################### TEST 3 ######################################
# not relevant here because this is the lake we're downsampling to in Test 3.
