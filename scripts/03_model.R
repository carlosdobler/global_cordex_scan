
dom <- "AUS"



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
tree <- randomForest(form,
                     data = tb %>% mutate(Q = factor(Q)),
                     ntree = 1000,
                     importance = T)

# saveRDS(tree, "output/tree.rds")

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




# PREDICTION MAPS
map(vars, function(v){
  
  if(dom == "AUS"){
    end_date <- "1990-01-01"
  } else {
    end_date <- "2003-01-01"
  }
  
  print(v)
  
  "output/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., dom)] %>% 
    .[str_detect(., "v7")] %>% 
    .[str_detect(., v)] -> files
    
  files %>% 
    map_chr(function(f){
      
      f %>% 
        str_split("_", simplify = T) %>% 
        {str_glue("{.[,4]}_{.[,5]}")}
    }) -> files_names
  
  files %>% 
    set_names(files_names) %>% 
    future_map(function(f){
      
      readRDS(f) %>% 
        split("band") %>%
        {.} -> s
      
      s %>% 
        mutate(diff_midpt = ifelse(as_date(time_bkpt) > end_date, 0, diff_midpt),
               diff_median = ifelse(as_date(time_bkpt) > end_date, 0, diff_median),
               diff_mean = ifelse(as_date(time_bkpt) > end_date, 0, diff_mean),
               diff_range = ifelse(as_date(time_bkpt) > end_date, 1, diff_range),
               diff_stddev = ifelse(as_date(time_bkpt) > end_date, 1, diff_stddev),
               diff_q1090 = ifelse(as_date(time_bkpt) > end_date, 1, diff_q1090)) -> s
      
      s %>% 
        predict(tree, type = "prob") %>% 
        select(1) %>%
        {.} -> s_pred
      
      # c(s, s_pred)
      return(s_pred)
      
    })
  
}) -> l_s_pred


l_s_pred %>% 
  map(function(l_v){
    
    l_v %>% 
      imap(function(s, i){
        
        s %>%
          as_tibble() %>% 
          mutate(model = i)
          
        
      })
    
  }) -> l_tb_pred


l_tb_pred %>% 
  imap(function(l_v, v){
    
    l_v %>%
      imap(function(tb, mod){
        
        th <- 0.6
        
        tb %>%
          filter(!is.na(b)) %>% 
          pull(b) %>% 
          {. > th} %>% 
          mean() %>% 
          {. * 100} -> prop
        
        tb %>% 
          mutate(bad = ifelse(b > th, 1L, 0L) %>% factor()) %>% 
          ggplot(aes(x,y, fill = bad)) +
          geom_raster(show.legend = F) +
          coord_equal() +
          scale_fill_manual(values = c("grey80", "red"),
                                       na.value = "transparent") +
          theme(axis.title = element_blank()) +
          labs(subtitle = str_glue("VAR: {v}
                                   {mod}"),
               caption = str_glue("Prop: {round(prop,1)} %"))
      
            
        
      })
    
  }) -> l_maps


l_maps %>%
  map(patchwork::wrap_plots, ncol = 3) -> l_maps

# l_maps[[1]]




l_s_pred %>% 
  imap(function(l_v, v){
    
    th <- 0.75
    
    l_v %>% 
      # .[str_detect(names(l_v), "RegCM4") %>% which()] %>%
      
      map(mutate, b = ifelse(b > th, 1, 0)) %>% 
      do.call(c, .) %>% 
      merge() %>% 
      st_apply(c(1,2), sum) %>% 
      mutate(b = ifelse(sum > 0, 1, sum)) -> s
    
    s %>% 
      as_tibble() %>% 
      filter(!is.na(b)) %>% 
      pull(b) %>% 
      mean() %>% 
      {. * 100} -> prop
    
    s %>%
      as_tibble() %>% 
      mutate(bad = b %>% factor()) %>% 
      ggplot(aes(x,y, fill = bad)) +
      geom_raster(show.legend = F) +
      coord_equal() +
      scale_fill_manual(values = c("grey80", "red"),
                        na.value = "transparent") +
      theme(axis.title = element_blank()) +
      labs(subtitle = str_glue("VAR: {v}  [all mods]"),
           caption = str_glue("Prop: {round(prop,1)} %"))
    
  }) -> l_maps

l_maps %>%
  patchwork::wrap_plots(ncol = 3)





l_s_pred %>% 
  imap(function(l_v, v){
    
    th <- 0.75
    
    l_v %>% 
      # .[str_detect(names(l_v), "RegCM4") %>% which()] %>%
      map(mutate, b = ifelse(b > th, 1, 0)) %>% 
      do.call(c, .) %>% 
      merge() %>% 
      st_apply(c(1,2), sum) %>% 
      mutate(b = ifelse(sum > 0, 1, sum)) %>% 
      select(b)

  }) -> l_maps

l_maps %>% 
  # map(select, b) %>% 
  do.call(c, .) %>% 
  merge() %>% 
  st_apply(c(1,2), sum) %>% 
  mutate(b = ifelse(sum > 0, 1, sum)) %>% 
  select(b) -> s


# *****************
# EXPORT THIS MAP!

dir.create("~/bucket_mine/results/global_cordex_scan")

devtools::source_url("https://github.com/carlosdobler/global_spei_v2_ww/blob/main/scripts/write_nc.R?raw=TRUE")

s %>% 
  setNames("bad_cells") %>% 
  func_write_nc_notime("~/bucket_mine/results/global_cordex_scan/AUS_bad_cells_mask.nc")

s %>% 
  as_tibble() %>% 
  filter(b == 1) %>% 
  mutate(x = x-0.1,
         y = y-0.1) %>% #ggplot(aes(x,y)) + geom_point()
  select(b) %>% 
  write_csv("~/bucket_mine/results/global_cordex_scan/AUS_bad_cells_mask.csv")
  


# *****************


s %>% 
  as_tibble() %>% 
  filter(!is.na(b)) %>% 
  pull(b) %>% 
  mean() %>% 
  {. * 100} -> prop

s %>%
  as_tibble() %>% 
  mutate(bad = b %>% factor()) %>% 
  ggplot(aes(x,y, fill = bad)) +
  geom_raster(show.legend = F) +
  coord_equal() +
  scale_fill_manual(values = c("grey80", "red"),
                    na.value = "transparent") +
  theme(axis.title = element_blank()) +
  labs(subtitle = str_glue("[all vars]  [all models]"),
       caption = str_glue("Prop: {round(prop,1)} %"))


map2_dfr(l_s_pred,
         "output/" %>% 
           list.files(full.names = T) %>% 
           .[str_detect(., "SAM")] %>% 
           .[str_detect(., "v5")] %>% 
           .[str_detect(., v)],
         function(s,i){
           
           i %>% 
             str_split("_", simplify = T) %>% 
             {str_glue("{.[,4]}_{.[,5]}")} -> i
           
           s %>% 
             as_tibble() %>% 
             mutate(model = i)
           
         }) -> tb_pred


v <- "tas"

l_tb_pred %>%
  pluck(v) %>%
  # pluck("REMO2015_MOHC-HadGEM2-ES") %>%
  bind_rows() %>% 
  filter(near(b, 0.75, 0.05)) %>% 
  {.} -> tb_errors

{
  tb_errors %>% 
    slice_sample() -> tb_errors_1
  
  tb_errors_1 %>% 
    mutate(rcm = str_split(model, "_", simplify = T)[,1],
           gcm = str_split(model, "_", simplify = T)[,2]) %>%
    select(rcm,gcm,x,y) %>%
    t() %>%
    as.vector() %>%
    {func_retrieve_ts(.[1],.[2], v ,as.numeric(.[3]), as.numeric(.[4]))} -> a
  
  func_jump_7(a$val, a$time) -> b
  
  str_glue(
    "midpt = {round(b[2],2)} // median = {round(b[3],2)} // mean =  {round(b[4],2)}
      range = {round(b[5],2)} // stddev = {round(b[6],2)} // q 10 90 = {round(b[7], 2)}"
  ) -> labs_metrics
  
  func_plot_ts_raw(a$val, a$time, func_jump_7(a$val, a$time) %>% .[1] %>% as_date()) +
    labs(title = str_glue("PROB: {tb_errors_1$b}  //  VAR: {v}  //  MODEL: {tb_errors_1$model}"),
         subtitle = labs_metrics)
}
















