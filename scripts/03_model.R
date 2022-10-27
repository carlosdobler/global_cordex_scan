
dom <- "SAM"



source("scripts/00_setup.R")
source("scripts/functions.R")

library(zoo)
library(imputeTS)

plan(multicore)

vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()


# TRAIN TABLE
gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1LMgcWh-jGNplCTl9JHlFHPvfWZZ7939UmF5SMoacERU/edit?usp=sharing") %>% 
  select(-obs) -> q_table

# SAMPLE TABLE
read_csv("output/tb_sample_v02.csv") -> tb_sample

# METRICS TABLE
"output/" %>%
  list.files(full.names = T) %>%
  # .[str_detect(., dom)] %>%
  .[str_detect(., "SAM")] %>%
  .[str_detect(., "v7")] %>%
  
  map_dfr(function(f){
    
    f %>%
      str_split("_", simplify = T) %>%
      .[,2] -> v
    
    f %>%
      str_split("_", simplify = T) %>%
      {str_glue("{.[,4]}_{.[,5]}")} -> m
    
    readRDS(f) %>%
      split("band") %>%
      
      as_tibble() %>%
      mutate(var = v,
             model = m)
    
  }) %>%
  filter(!is.na(time_bkpt)) %>%
  mutate(time_bkpt = as_date(time_bkpt)) %>%
  mutate(x = round(x,1),
         y = round(y,1)) %>%
  {.} -> tb_allvars_allmods


# JOIN ALL
tb_sample %>% 
  left_join(tb_allvars_allmods) %>%
  left_join(q_table) -> tb_sample



# TRAIN MODEL

library(randomForest)


tb_sample %>% 
  group_by(Q) %>% 
  mutate(r = row_number()) %>% 
  ungroup() %>% 
  mutate(var = factor(var)) -> tb

tb %>% 
  select(starts_with("diff_")) %>% 
  names() %>% 
  str_flatten(" + ") %>% 
  {str_flatten(str_glue("Q ~ {.}"))} %>% 
  as.formula() -> form

# estimate error
{
  map_dbl(seq_len(200), function(...){
    
    nn <- nrow(filter(tb, Q == "g"))
    
    sample(nn, 1/5*nn) %>% 
      {tibble(r = .,
              Q = "g",
              test = 1)} -> test_ind_g
    
    nn <- nrow(filter(tb, Q == "b"))
    
    sample(nn, 1/5*nn) %>% 
      {tibble(r = .,
              Q = "b",
              test = 1)} -> test_ind_b
    
    bind_rows(test_ind_g, test_ind_b) %>%
      {left_join(tb, ., by = c("r", "Q"))} %>% 
      mutate(test = ifelse(is.na(test), 0, test)) %>% 
      mutate(Q = factor(Q)) -> tb_2
    
    tb_2[tb_2$test == 0, ] -> tb_2_train
    tb_2[tb_2$test == 1, ] -> tb_2_test
    
    tree <- randomForest(form,
                         data = tb_2_train,
                         ntree = 1000)
    
    predict(tree, tb_2_test, type = "class") -> preds
    tb_2_test$pred <- preds
    
    tb_2_test %>% 
      count(Q,pred) %>% 
      mutate(err = case_when(Q == "g" & pred == "g" ~ 1,
                             Q == "b" & pred == "b" ~ 1,
                             TRUE ~ 0)) %>% 
      group_by(err) %>% 
      summarize(n = sum(n)) %>% 
      pull(n) %>% 
      {.[1]/.[2]}
    
  }) -> errs
  
  quantile(errs, na.rm = T) %>% round(3)
  mean(errs, na.rm = T)
}

# final model
set.seed(123)
tree <- randomForest(form,
                     data = tb %>% mutate(Q = factor(Q)),
                     ntree = 1000,
                     importance = T)

saveRDS(tree, "output/tree.rds")

randomForest::varImpPlot(tree)


predict(tree, type = "class") -> preds
tb$pred <- preds

tb %>% 
  count(Q,pred) %>% 
  mutate(err = case_when(Q == "g" & pred == "g" ~ 1,
                         Q == "b" & pred == "b" ~ 1,
                         TRUE ~ 0)) %>% 
  group_by(err) %>% 
  summarize(n = sum(n)) %>% 
  pull(n) %>% 
  {.[1]/.[2]}


# with rpart
{
  # library(rpart)
  # 
  # 
  # # readRDS("output/tb_sample_Q.rds") %>% 
  # #   mutate(Q = factor(Q)) -> tb
  # 
  # tb_sample_Q %>% 
  #   group_by(Q) %>% 
  #   mutate(r = row_number()) %>% 
  #   ungroup() %>% 
  #   mutate(var = factor(var)) -> tb
  # 
  # 
  # # estimate error
  # map_dbl(seq_len(100), function(...){
  #   
  #   sample(nrow(filter(tb, Q == 0)), 
  #          1/5*nrow(filter(tb, Q == 0))) %>% 
  #     {tibble(r = .,
  #             Q = 0,
  #             test = 1)} -> test_ind_0
  #   
  #   sample(nrow(filter(tb, Q == 1)), 
  #          1/5*nrow(filter(tb, Q == 1))) %>% 
  #     {tibble(r = .,
  #             Q = 1,
  #             test = 1)} -> test_ind_1
  #   
  #   bind_rows(test_ind_0, test_ind_1) %>%
  #     {left_join(tb, ., by = c("r", "Q"))} %>% 
  #     mutate(test = ifelse(is.na(test), 0, test)) %>% 
  #     mutate(Q = factor(Q)) -> tb_2
  #   
  #   tb_2[tb_2$test == 0, ] -> tb_2_train
  #   tb_2[tb_2$test == 1, ] -> tb_2_test
  #   
  #   # tree <- randomForest(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2,
  #   #                      data = tb_2_train)
  #   
  #   tree <- rpart(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2,
  #                 data = tb_2_train,
  #                 method = "class",
  #                 maxdepth = 3)
  #   
  #   predict(tree, tb_2_test, type = "class") -> preds
  #   tb_2_test$pred <- preds
  #   
  #   tb_2_test %>% 
  #     count(Q,pred) %>% 
  #     mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
  #                            Q == 0 & pred == 0 ~ 1,
  #                            TRUE ~ 0)) %>% 
  #     group_by(err) %>% 
  #     summarize(n = sum(n)) %>% 
  #     pull(n) %>% 
  #     {.[1]/.[2]}
  #   
  # }) -> errs
  # 
  # quantile(errs)
  # mean(errs)
  # 
  # 
  # tree <- rpart(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2, 
  #                      data = tb, 
  #               method = "class",
  #               maxdepth = 3)
  # 
  # predict(tree, type = "class") -> preds
  # tb$pred <- preds
  # 
  # tb %>% 
  #   count(Q,pred) %>% 
  #   mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
  #                          Q == 0 & pred == 0 ~ 1,
  #                          TRUE ~ 0)) %>% 
  #   group_by(err) %>% 
  #   summarize(n = sum(n)) %>% 
  #   pull(n) %>% 
  #   {.[1]/.[2]}
  # 
  # plot(tree, margin = 0.2)
  # text(tree)
}



