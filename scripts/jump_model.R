# source("~/00-mount.R")

source("scripts/00_setup.R")

plan(multisession)

source("scripts/functions.R")

dom <- "SAM"

vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()



"output/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., dom)] %>% 
  .[str_detect(., "v4")] %>% 
  .[str_detect(., "RegCM4")] %>% 
  
  map_dfr(function(f){
    
    f %>% 
      str_split("_", simplify = T) %>% 
      .[,2] -> v
    
    f %>% 
      str_split("_", simplify = T) %>% 
      {str_glue("{.[,4]}_{.[,5]}")} -> m
    
    readRDS(f) %>% 
      split("band") %>% 
      
      mutate(seg_diff_midp = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_midp),
             seg_diff_medn = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_medn),
             seg_diff_sprd = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_sprd),
             seg_diff_sprd2 = ifelse(year(as_date(time_bkpt_var)) > 2000, 0, seg_diff_sprd2)) %>% 
      
      as_tibble() %>% 
      mutate(var = v,
             mod = m)
    
  }) -> tb_allvars_allmods

tb_allvars_allmods %>% 
  filter(!is.na(count_na)) %>% # tiles not calculated 
  filter(count_na < 1500) %>% # ocean 
  {.} -> tb_allvars_allmods


# set.seed(123)
# vars %>% 
#   map_dfr(function(v){
#     
#     tb_allvars_allmods %>% 
#       filter(var == v) -> tb
#     
#     map_dfr(names(tb)[c(4,5,6,8)], function(met){
#       
#       map_dfr(seq(0.01,0.99,0.245), function(quan){
#       
#         tb %>% 
#           rename("vv" = met) %>%
#           filter(near(vv, quan, 0.05)) %>% 
#           slice_sample(n = 3) %>% 
#           rename(!!met := "vv")
#         
#       })
#     })
#     
#   }) -> tb_sample

# set.seed(123)
# vars %>% 
#   map_dfr(function(v){
#     
#     tb_allvars_allmods %>% 
#       filter(var == v) -> tb
#     
#     map_dfr(names(tb)[c(4,5,6,8)], function(met){
#       
#       map_dfr(c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75), function(quan){
#         
#         tb %>% 
#           rename("vv" = met) %>%
#           filter(near(vv, quan, 0.025)) -> tb_fil
#       
#         if(nrow(tb_fil) > 3){
#           tb_fil %>% 
#             slice_sample(n = 3) %>% 
#             rename(!!met := "vv")
#         } else {
#           tb_fil %>% 
#             rename(!!met := "vv")
#         }
#         
#         
#       })
#     }) %>% 
#       
#       {.[!duplicated(.), ]}
#     
#   }) -> tb_sample
# 
# 
# 
# func_l_s <- function(v){
#   
#   "output/" %>% 
#     list.files(full.names = T) %>% 
#     .[str_detect(., dom)] %>% 
#     .[str_detect(., "v4")] %>%
#     .[str_detect(., v)] %>%
#     .[str_detect(., "RegCM4")] %>% # *****
#     str_split("_", simplify = T) %>% 
#     {str_glue("{.[,4]}_{.[,5]}")} %>% 
#     as.vector() %>% 
#     set_names() %>% 
#     map(function(m){
#       
#       m %>% 
#         str_split("_", simplify = T) %>% 
#         .[,1] -> rm
#       
#       m %>% 
#         str_split("_", simplify = T) %>% 
#         .[,2] -> mm
#       
#       str_glue("~/bucket_mine/remo/monthly/{dom}-22/{v}") %>% 
#         list.files(full.names = T) %>% 
#         .[str_detect(., "regrid")] %>% 
#         .[str_detect(., rm)] %>% 
#         .[str_detect(., mm)] %>%
#         
#         future_map(function(f){
#           
#           f %>% 
#             str_split("_", simplify = T) %>% 
#             .[,10] -> date_range
#           
#           date_range %>% str_sub(end = 6) %>% str_c("01") -> t_i
#           date_range %>% str_sub(start = 8, end = 14) %>% str_c("01") -> t_f
#           
#           read_ncdf(f) %>% 
#             st_set_dimensions("time",
#                               values = seq(as_date(t_i), as_date(t_f), by = "1 month"))
#           
#         }) %>% 
#         suppressMessages() %>% 
#         suppressWarnings() %>% 
#         do.call(c, .)
#   
#     })
# } 
#     
# func_l_plots <- function(v){
#   
#   func_l_s(v) -> l_s
#   
#   pmap(tb_sample %>% filter(var == v), function(x,y,mod,
#                                                 # time_bkpt_trend,
#                                                 # seg_diff_midp,
#                                                 # seg_diff_medn,
#                                                 # seg_diff_sprd,
#                                                 # seg_diff_sprd2,
#                                                 ...){
#     
#     l_s %>% 
#       pluck(mod) -> s
#     
#     s[,
#       which.min(abs(st_get_dimension_values(s, "lon") - x)),
#       which.min(abs(st_get_dimension_values(s, "lat") - y)),
#     ] %>% 
#       pull(1) %>% 
#       as.vector() -> xx
#     
#     xx %>% 
#       sort() %>% 
#       {c(.[length(.)*1/4], .[length(.)*1/2], .[length(.)*3/4])} %>% 
#       {c((.[1]-(.[2]-.[1])*0.1), (.[3]+(.[3]-.[2])*0.1))} %>%
#       {xx[xx <= .[1] | xx >= .[2]]} -> out
#     
#     func_jump_4(x = xx, dates = st_get_dimension_values(s, "time")) -> jump_xx
#     
#     tibble(
#       # vv = xx,
#       vv = ifelse(xx %in% out, NA, xx),
#       time = st_get_dimension_values(s, "time")
#     ) %>% 
#       
#       {
#         ggplot(., aes(x = time, y = vv, fill = vv)) +
#           geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
#           scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
#                        minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
#           geom_vline(xintercept = as_date(jump_xx["time_bkpt_trend"]), alpha = 0.3, linetype = "2222") +
#           colorspace::scale_fill_continuous_sequential("Plasma") +
#           theme(axis.title = element_blank()) +
#           labs(subtitle = str_glue("midp = {round(jump_xx['seg_diff_midp'], 2)}  //  medn = {round(jump_xx['seg_diff_medn'], 2)}  //  mean = {round(jump_xx['seg_diff_mean'], 2)}  //  rnge = {round(jump_xx['seg_diff_rnge'], 2)}  //  vari = {round(jump_xx['seg_diff_vari'], 2)}  //  iqrr = {round(jump_xx['seg_diff_iqrr'], 2)}  //  qn19 = {round(jump_xx['seg_diff_qn19'], 2)}  //  rnge_2 = {round(jump_xx['seg_diff_rnge_2'], 2)}  //  vari_2 = {round(jump_xx['seg_diff_vari_2'], 2)}  //  iqrr_2 = {round(jump_xx['seg_diff_iqrr_2'], 2)}  //  qn19_2 = {round(jump_xx['seg_diff_qn19_2'], 2)}"))
#       }
#   })
#   
# }
# 
# # plots
# func_l_plots("pr") -> l_plots
# 
# seq_len(length(l_plots)) %>% 
#   {split(., ceiling(seq_along(.)/5))} %>% 
#   map(function(p){
#     
#     patchwork::wrap_plots(l_plots[p], ncol = 1)
#     
#   }) -> l_l_plots
# 
# 
# {
#   bind_rows(
#     
#     tb_sample %>% 
#       filter(var == "pr") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,0,1,1,1,
#                    1,0,1,1,0, # 3
#                    0,0,1,1,0,
#                    1,1,1,1,1, # 5
#                    1,1,1,0,0,
#                    0,0,0,0,0, # 7
#                    0,0,0,0,0,
#                    0,0,1,1,1, # 9
#                    1,1,1,1,1,
#                    1,0,0,1,0, # 11
#                    0,1,0,1,1,
#                    1,0,0,1,0, # 13
#                    1,1,0,1,1,
#                    1,1,0,0,0, # 15
#                    0,1,0,0,0,
#                    0,0,1,0)), # 17
#     
#     tb_sample %>% 
#       filter(var == "tasmin") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,0,0,0,0,
#                    0,0,0,0,0, # 3
#                    0,0,0,0,0,
#                    0,1,1,1,1, # 5
#                    1,1,0,0,0,
#                    0,0,0,0,0, # 7
#                    0,0,0,0,0,
#                    1,1,1,1,1, # 9
#                    1,0,1,1,1,
#                    1,0,1,1,1, # 11
#                    0,1,1,1,0,
#                    1,0,0,0,1, # 13
#                    1,1,1,0,0)),
#     
#     tb_sample %>% 
#       filter(var == "tasmax") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,0,0,0,0,
#                    0,0,0,0,0, # 3
#                    0,0,0,0,0,
#                    0,1,1,1,1, # 5
#                    1,1,0,0,0,
#                    0,0,0,0,0, # 7
#                    0,0,0,0,0,
#                    1,0,1,1,1, # 9
#                    1,0,1,0,0,
#                    1,0,0,1,1, # 11
#                    0,1,0,1,1,
#                    0,0,0,0,0)),  # 13
#     
#     tb_sample %>% 
#       filter(var == "sfcWind") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,1,0,0,0,
#                    0,0,0,0,0, # 3
#                    0,0,0,0,0,
#                    0,1,1,1,1, # 5
#                    1,1,0,0,0,
#                    0,0,0,0,0, # 7
#                    0,0,0,0,0,
#                    1,1,1,1,1, # 9
#                    1,1,1,1,1,
#                    0,0,0,1,0, # 11
#                    0,0,0,0,0,
#                    1,0,0,0,0, # 13
#                    1,0,1,0,0,
#                    0,0,0,0,0, # 15
#                    0,0,1,0)),
#     
#     tb_sample %>% 
#       filter(var == "rsds") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,1,1,0,0,
#                    0,0,0,0,0, # 3
#                    0,0,0,1,1,
#                    1,1,1,1,1, # 5
#                    0,0,0,0,0,
#                    0,0,0,0,0, # 7
#                    1,1,1,0,1,
#                    1,1,1,1,0, # 9
#                    0,0,0,0,0,
#                    0,0,0,1,0, # 11
#                    0,1,0,1,0,
#                    0,1,0,0,1, # 13
#                    0,0,1,0,0,
#                    0,0)), # 15
#     
#     tb_sample %>% 
#       filter(var == "hurs") %>% 
#       mutate(Q = c(1,1,1,1,1, # 1
#                    1,1,0,1,0,
#                    0,0,0,0,0, # 3
#                    0,0,0,1,1,
#                    1,1,1,1,0, # 5
#                    1,1,0,0,0,
#                    0,0,0,0,0, # 7
#                    0,0,0,1,1,
#                    1,1,1,1,1, # 9
#                    1,1,1,1,1,
#                    1,1,0,1,1, # 11
#                    1,0,1,0,0,
#                    0,0,1,0,1, # 13
#                    1,0,1,1,0,
#                    1,0,0,1,0, # 15
#                    1,1,0))
#     
#   ) -> tb_sample_Q
# }
# 
# saveRDS(tb_allvars_allmods_sample_Q, "output/tb_sample_Q.rds")
# 
# tb_sample_Q %>%
#   select(x,y,var,mod,Q) -> tb_sample_Q_2
# 
# saveRDS(tb_sample_Q_2, "output/tb_sample_Q_2.rds")


"output/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., dom)] %>% 
  .[str_detect(., "v5")] %>% 
  .[str_detect(., "RegCM4")] %>% 
  
  map_dfr(function(f){
    
    f %>% 
      str_split("_", simplify = T) %>% 
      .[,2] -> v
    
    f %>% 
      str_split("_", simplify = T) %>% 
      {str_glue("{.[,4]}_{.[,5]}")} -> m
    
    readRDS(f) %>% 
      split("band") %>% 
      
      # mutate(seg_diff_midp = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_midp),
      #        seg_diff_medn = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_medn),
      #        seg_diff_sprd = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_sprd),
      #        seg_diff_sprd2 = ifelse(year(as_date(time_bkpt_var)) > 2000, 0, seg_diff_sprd2)) %>% 
      
      as_tibble() %>% 
      mutate(var = v,
             mod = m)
    
  }) -> tb_allvars_allmods

tb_allvars_allmods %>% 
  filter(!is.na(count_na)) %>% # tiles not calculated 
  filter(count_na < 1500) %>% # ocean 
  {.} -> tb_allvars_allmods

tb_sample_Q_2 %>% 
  left_join(tb_allvars_allmods, by = c("x", "y", "var", "mod")) -> tb_sample_Q_3




library(randomForest)

tb_sample_Q_3 %>% 
  group_by(Q) %>% 
  mutate(r = row_number()) %>% 
  ungroup() %>% 
  mutate(var = factor(var)) -> tb

names(tb) %>% 
  .[c(3, 7:11, 13, 15:16, 18)] %>% 
  str_flatten(" + ") %>% 
  {str_flatten(str_glue("Q ~ {.}"))} %>% 
  as.formula() -> form

# estimate error
map_dbl(seq_len(200), function(...){
  
  nn <- nrow(filter(tb, Q == 0))
  
  sample(nn, 1/5*nn) %>% 
    {tibble(r = .,
            Q = 0,
            test = 1)} -> test_ind_0
  
  nn <- nrow(filter(tb, Q == 1))
  
  sample(nn, 1/5*nn) %>% 
    {tibble(r = .,
            Q = 1,
            test = 1)} -> test_ind_1
  
  bind_rows(test_ind_0, test_ind_1) %>%
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
    mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
                           Q == 0 & pred == 0 ~ 1,
                           TRUE ~ 0)) %>% 
    group_by(err) %>% 
    summarize(n = sum(n)) %>% 
    pull(n) %>% 
    {.[1]/.[2]}
  
}) -> errs

quantile(errs)
mean(errs)


tree <- randomForest(form,
                     data = tb %>% mutate(Q = factor(Q)),
                     ntree = 1000,
                     importance = T)


randomForest::varImpPlot(tree)


predict(tree, type = "class") -> preds
tb$pred <- preds

tb %>% 
  count(Q,pred) %>% 
  mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
                         Q == 0 & pred == 0 ~ 1,
                         TRUE ~ 0)) %>% 
  group_by(err) %>% 
  summarize(n = sum(n)) %>% 
  pull(n) %>% 
  {.[1]/.[2]}





library(rpart)


# readRDS("output/tb_sample_Q.rds") %>% 
#   mutate(Q = factor(Q)) -> tb

tb_sample_Q %>% 
  group_by(Q) %>% 
  mutate(r = row_number()) %>% 
  ungroup() %>% 
  mutate(var = factor(var)) -> tb


# estimate error
map_dbl(seq_len(100), function(...){
  
  sample(nrow(filter(tb, Q == 0)), 
         1/5*nrow(filter(tb, Q == 0))) %>% 
    {tibble(r = .,
            Q = 0,
            test = 1)} -> test_ind_0
  
  sample(nrow(filter(tb, Q == 1)), 
         1/5*nrow(filter(tb, Q == 1))) %>% 
    {tibble(r = .,
            Q = 1,
            test = 1)} -> test_ind_1
  
  bind_rows(test_ind_0, test_ind_1) %>%
    {left_join(tb, ., by = c("r", "Q"))} %>% 
    mutate(test = ifelse(is.na(test), 0, test)) %>% 
    mutate(Q = factor(Q)) -> tb_2
  
  tb_2[tb_2$test == 0, ] -> tb_2_train
  tb_2[tb_2$test == 1, ] -> tb_2_test
  
  # tree <- randomForest(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2,
  #                      data = tb_2_train)
  
  tree <- rpart(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2,
                data = tb_2_train,
                method = "class",
                maxdepth = 3)
  
  predict(tree, tb_2_test, type = "class") -> preds
  tb_2_test$pred <- preds
  
  tb_2_test %>% 
    count(Q,pred) %>% 
    mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
                           Q == 0 & pred == 0 ~ 1,
                           TRUE ~ 0)) %>% 
    group_by(err) %>% 
    summarize(n = sum(n)) %>% 
    pull(n) %>% 
    {.[1]/.[2]}
  
}) -> errs

quantile(errs)
mean(errs)


tree <- rpart(Q ~ seg_diff_midp + seg_diff_medn + seg_diff_sprd + seg_diff_sprd2, 
                     data = tb, 
              method = "class",
              maxdepth = 3)

predict(tree, type = "class") -> preds
tb$pred <- preds

tb %>% 
  count(Q,pred) %>% 
  mutate(err = case_when(Q == 1 & pred == 1 ~ 1,
                         Q == 0 & pred == 0 ~ 1,
                         TRUE ~ 0)) %>% 
  group_by(err) %>% 
  summarize(n = sum(n)) %>% 
  pull(n) %>% 
  {.[1]/.[2]}

plot(tree, margin = 0.2)
text(tree)

# v <- vars[1]
# tb %>% 
#   filter(Q != pred) %>% 
#   filter(var == v) %>%
#   pmap(function(x,y,mod,trend_time_bkpt,sprd2_time_bkpt,Q,pred,...){
#     
#     l_s %>% 
#       pluck(mod) -> s
#     
#     s[,
#       which.min(abs(st_get_dimension_values(s, "lon") - x)),
#       which.min(abs(st_get_dimension_values(s, "lat") - y)),
#     ] %>% 
#       pull(1) %>% 
#       as.vector() -> xx
#     
#     xx %>% 
#       sort() %>% 
#       {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#       {c((.[1]-(.[2]-.[1])*1.5), (.[3]+(.[3]-.[2])*1.5))} %>%
#       {xx[xx <= .[1] | xx >= .[2]]} -> out
#     
#     # boxplot.stats(xx)$out -> out
#     
#     tibble(
#       # vv = xx,
#       vv = ifelse(xx %in% out, NA, xx),
#       time = st_get_dimension_values(s, "time")
#     ) %>% 
#       
#       {
#         ggplot(., aes(x = time, y = vv, fill = vv)) +
#           geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
#           scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
#                        minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
#           geom_vline(xintercept = as_date(trend_time_bkpt), alpha = 0.3, linetype = "2222") +
#           colorspace::scale_fill_continuous_sequential("Plasma") +
#           theme(axis.title = element_blank()) +
#           labs(subtitle = str_glue("Model says {pred}, but it is {Q}"))
#       }
#   }) -> l_plots





#

v <- "hurs"

"output/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "SAM")] %>% 
  .[str_detect(., "v4")] %>% 
  .[str_detect(., v)] %>% 
  
  map(function(f){
    
    readRDS(f) %>% 
      split("band") %>% 
      
      mutate(seg_diff_midp = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_midp),
             seg_diff_medn = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_medn),
             seg_diff_sprd = ifelse(year(as_date(time_bkpt_trend)) > 2000, 0, seg_diff_sprd),
             seg_diff_sprd2 = ifelse(year(as_date(time_bkpt_var)) > 2000, 0, seg_diff_sprd2)) %>% 
      
    {.} -> s
    
    s %>% 
      predict(tree, type = "prob") %>% 
      select(1) %>%
      {.} -> s_pred
    
    c(s, s_pred)
    
  }) -> l_s_pred


l_s_pred[[2]] %>% select(X0) %>% mapview::mapview()

map2_dfr(l_s_pred,
         "output/" %>% 
           list.files(full.names = T) %>% 
           .[str_detect(., "SAM")] %>% 
           .[str_detect(., "v4")] %>% 
           .[str_detect(., v)],
         function(s,i){
           
           i %>% 
             str_split("_", simplify = T) %>% 
             {str_glue("{.[,4]}_{.[,5]}")} -> i
           
           s %>% 
             as_tibble() %>% 
             mutate(mod = i)
           
         }) -> tb_pred


tb_pred %>% 
  as_tibble() %>% 
  filter(X0 > 0.5) -> tb_errors

func_l_s(v) -> l_s

tb_errors %>% 
  filter(mod == "RegCM4_MPI-M-MPI-ESM-MR",
         near(x, -53, 0.5)) %>% 
  slice_sample() -> miau





pmap(#tb_errors %>% slice_sample(n = 5), 
     miau,
     function(x,y,mod,time_bkpt_trend,...){
  
  l_s %>% 
    pluck(mod) -> s
  
  s[,
    which.min(abs(st_get_dimension_values(s, "lon") - x)),
    which.min(abs(st_get_dimension_values(s, "lat") - y)),
  ] %>% 
    pull(1) %>% 
    as.vector() -> xx
  
  xx %>% 
    sort() %>% 
    {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
    {c((.[1]-(.[2]-.[1])*1.5), (.[3]+(.[3]-.[2])*1.5))} %>%
    {xx[xx <= .[1] | xx >= .[2]]} -> out
  
  # boxplot.stats(xx)$out -> out
  
  tibble(
    # vv = xx,
    vv = ifelse(xx %in% out, NA, xx),
    time = st_get_dimension_values(s, "time")
  ) %>% 
    
    {
      ggplot(., aes(x = time, y = vv, fill = vv)) +
        geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
        scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                     minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
        geom_vline(xintercept = as_date(time_bkpt_trend), alpha = 0.3, linetype = "2222") +
        colorspace::scale_fill_continuous_sequential("Plasma") +
        theme(axis.title = element_blank())
    } -> p
  
  list(p, xx)
}) -> p_xx
  #patchwork::wrap_plots(l_plots, ncol = 1)

#

p_xx[[1]][[2]] -> x
