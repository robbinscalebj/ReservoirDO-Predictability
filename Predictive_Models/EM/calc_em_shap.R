#Calculate SHAP vals for Eagle Mountain test set predictions at top middle and bottom depths 
#####

library(tidyverse)
library(tidymodels)
library(tidytext)
library(furrr)
library(tictoc)
library(parallel)
library(doParallel)
library(doSNOW)
library(rLakeAnalyzer)
library(ggsci)
library(ggdist)
library(shapr)
library(here)

here::i_am("Predictive_Models/EM/calc_em_shap.R")

em_randfor<-readRDS(here("Predictive_Models/EM/randfor_fit.Rds")) #randfor_fit  is not specifically a workflow - this has already had last_fit() run on it because we had to specify the exact lag6 recipe for the final fit (instead of the split object) because ranger can't handle missing values
em_data <- em_randfor[[1]][[1]][["data"]]



#####

prep_data <- function(dat){
  
  shap_prep <- recipe(DO_mg.L ~ ., data = dat)|>
    step_select(-validation_period, -val_period,-Datetime,skip = TRUE)|>
    step_normalize(all_numeric(), -all_outcomes())|>prep(dat|>filter(validation_period == "training"))
  shap_dat_train <- shap_prep|>bake(dat|>filter(validation_period == "training"))

  shap_dat_test <- shap_prep|>bake(dat|>drop_na()|>filter(validation_period == "testing")|>
                                     filter(Depth_m %in% c(0,5,9))|>ungroup())
  dat_list <- list("shap_dat_train" = shap_dat_train,"shap_dat_test" = shap_dat_test)
}



train_vec <- prep_data(em_data)|>pluck("shap_dat_train")
test_vec <- prep_data(em_data)|>pluck("shap_dat_test")




#### nest test data so it can be partitioned into batches of ten rows, while keeping extractable grouping structure
nested_test_data <- test_vec|>
  mutate(Depth_m_raw = as_factor(round(Depth_m,2)),
         Depth_m_raw = fct_recode(Depth_m_raw, `0` = "-1.65",`5` = "0",`10` = "1.32"))|> #convert normalized depths to new grouping variable of actual depths
  group_by(Depth_m_raw, val_period)|>
  mutate(row = row_number(),
         batch = cut_width(row, 10, boundary = 0, labels = FALSE),
         group_id = paste(val_period, Depth_m_raw, batch, sep = "_"))|>
  arrange(val_period,Depth_m)|>
  select(-row,-batch)|>ungroup()|>
  nest_by(group_id)|>ungroup()|>mutate(batch_number = row_number())

n_data_batches <- nrow(nested_test_data)
batch_names <- nested_test_data|>pull(group_id)

#set up model
model <- em_randfor|>extract_fit_parsnip()|>pluck(3)

#set up training data for explain
train_data1 <- train_vec
phi0 <- mean(train_data1$DO_mg.L)
train_data <- train_data1|>select(-val_period,-validation_period, -DO_mg.L,-Datetime)



#run loop
tic()
#set up options and parallelization
cl <- makePSOCKcluster(5) 
registerDoSNOW(cl)
slice_size <- 2 # for testing
pb <- txtProgressBar(max = n_data_batches, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Loop
em_shaps <- foreach(i = 1:nrow(nested_test_data), .combine = 'c', .options.snow = opts, 
        .packages = c("tidyverse"), .inorder = TRUE, .final = function(x) setNames(x, batch_names)) %dopar% {
          
          #shape test data
          test_data <- nested_test_data|>filter(batch_number == i)|>unnest(data)
            #slice_head(n = slice_size) ### TESTING
            
            datetimes <- test_data|>mutate(Datetime = as.character(Datetime))|>pull(Datetime)|>as_tibble()|>rename(Datetime = "value")
            test_data <- test_data|>select(-val_period,-validation_period, -DO_mg.L,-Datetime,-Depth_m_raw,-group_id,-batch_number)
        
          #run function
          oplan <- future::plan(strategy = "sequential") #I don't know if setting this stops my cpu processes from blowing up
          explanation <- shapr::explain(
            model = model,
            x_train = train_data,
            x_explain = test_data,
            approach = "empirical",
            n_batches = 40,
            prediction_zero = phi0)|>
            pluck("shapley_values")
          on.exit(future::plan(oplan), add = TRUE)
          
          list(list(explanation, datetimes))
        }     
stopCluster(cl)
 toc()    
 #10 cores/10 data sets of 10 rows - 586 s - CPU processes 100% robbed, RAM OK, total estimated time for full data = 586/10*871 = 14 hours
 #5 cores/5 data sets of 10 rows - 360ish s - CPU processes largely robbed but only intermittently. Can mostly do other computer tasks 360/5*871= 18 hours, not a bad slow down!
                    

  



#plotting workflow

shap_names <- names(em_shaps)
em_shaps_df <- em_shaps|>map(~flatten(.)|>as_tibble())|>
  map2(.y = shap_names, ~as_tibble(.x)|>mutate(group_id = rep(.y, nrow(.x))))|>
  list_rbind()|>
  separate_wider_delim(group_id, names = c("val_period", "b"), delim = "_", too_many = "merge")|>
  separate_wider_delim(b, names = c("Depth_m_raw", "group_number"), delim = "_")
 
em_shaps_df|>write_csv(here("Predictive_Models/EM/SHAP_vals.csv"))
  
  em_shap_sums <- em_shaps_df|>
    select(-Datetime, -group_number)|>
    pivot_longer(c(-val_period, -Depth_m_raw), names_to = "variable", values_to = "SHAP")|>
    mutate(val_period = as_factor(val_period),
           Depth_m_raw = as.factor(Depth_m_raw),
           variable = as_factor(variable))|>
    group_by(val_period,Depth_m_raw, variable)|>
    summarize(mean_SHAP = abs(mean(SHAP)),
              sd_SHAP = sd(SHAP))|>
    ungroup()

#ggplot(aes(x = mean_SHAP, y = variable, fill = Depth_m_raw))+
  ggplot(em_shap_sums|>filter(Depth_m_raw == 0), aes(y=reorder_within(variable, mean_SHAP, list(val_period,Depth_m_raw)), x=mean_SHAP))+
  geom_bar(stat = "identity")+
    scale_y_reordered()+
    facet_wrap(val_period~Depth_m_raw, scales = "free")+
    xlab("absolute mean shap")


  

