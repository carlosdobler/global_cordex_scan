# source("~/00-mount.R")

source("scripts/00_setup.R")
source("scripts/functions.R")

library(zoo)
library(imputeTS)

plan(multicore)

dom <- "SAM"

vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()

# GENERATE SAMPLE TABLE
{
  # "output/" %>%
  #   list.files(full.names = T) %>%
  #   .[str_detect(., dom)] %>%
  #   .[str_detect(., "v6")] %>%
  # 
  #   map_dfr(function(f){
  # 
  #     f %>%
  #       str_split("_", simplify = T) %>%
  #       .[,2] -> v
  # 
  #     f %>%
  #       str_split("_", simplify = T) %>%
  #       {str_glue("{.[,4]}_{.[,5]}")} -> m
  # 
  #     readRDS(f) %>%
  #       split("band") %>%
  # 
  #       as_tibble() %>%
  #       mutate(var = v,
  #              model = m)
  # 
  #   }) -> tb_allvars_allmods
  # 
  # 
  # tb_allvars_allmods %>%
  #   filter(!is.na(time_bkpt)) %>%
  #   mutate(time_bkpt = as_date(time_bkpt)) %>%
  #   mutate(x = round(x,1),
  #          y = round(y,1)) %>%
  #   {.} -> tb_allvars_allmods
  # 
  # # ggplot(tb_allvars_allmods %>% filter(str_detect(model, "RegCM4_MPI")),
  # #        aes(x = x, y = y, fill = diff_median)) +
  # #   geom_raster() +
  # #   coord_equal() +
  # #   facet_wrap(~var) +
  # #   colorspace::scale_fill_continuous_sequential("viridis", rev = F)
  # 
  # 
  # 
  # # ASSEMBLE SAMPLE TABLE
  # # option 1
  # {
  #   # set.seed(123)
  #   # vars %>%
  #   #   map_dfr(function(v){
  #   # 
  #   #     tb_allvars_allmods %>%
  #   #       filter(var == v) %>%
  #   #       pivot_longer(starts_with("diff"), "metric", "value") -> tb
  #   # 
  #   #     tb$metric %>%
  #   #       unique() %>%
  #   #       map_dfr(function(met){
  #   # 
  #   #         c(0.05, 0.1, 0.18, 0.22, 0.3, 0.5) %>%
  #   #           map_dfr(function(quan){
  #   # 
  #   #             tb %>%
  #   #               filter(#model == mod,
  #   #                 metric == met,
  #   #                 near(value, quan, 0.025)) -> tb_fil
  #   # 
  #   #             if(nrow(tb_fil) > 3){
  #   #               tb_fil %>%
  #   #                 slice_sample(n = 3)
  #   #             } else {
  #   #               tb_fil
  #   #             }
  #   # 
  #   #           })
  #   #         
  #   # 
  #   #       })
  #   #   }) -> tb_sample
  #   # 
  #   # tb_sample %>%
  #   #   select(-metric, -value, -starts_with("time_bkpt")) %>%
  #   #   # left_join(tb_allvars_allmods, by = c("x", "y",
  #   #   #                                      "var",
  #   #   #                                      "time_bkpt_m",
  #   #   #                                      "time_bkpt_v",
  #   #   #                                      "model")) %>%
  #   #   {.} -> tb_sample
  #   # 
  #   # tb_sample[!duplicated(tb_sample),] -> tb_sample
  #   # 
  #   # tb_sample %>% 
  #   #   mutate(id = row_number(), .before = 1) -> tb_sample
  # }
  # 
  # # option 2 (k-means)
  # {
  #   set.seed(123)
  #   vars %>%
  #     map_dfr(function(v){
  #       
  #       tb_allvars_allmods %>%
  #         filter(var == v) %>% 
  #         filter(if_all(starts_with("diff"), ~ . > 0)) -> tb
  #       
  #       tb %>% 
  #         select(starts_with("diff")) %>% 
  #         select(-diff_stddev) %>% 
  #         kmeans(6, nstart = 25, iter.max = 1000, algorithm = "Lloyd") %>% 
  #         .$cluster -> clus
  #       
  #       tb %>% 
  #         mutate(gp = clus) %>% 
  #         group_by(gp) %>% 
  #         slice_sample(n = 16)
  #       
  #     }) %>% 
  #     ungroup() -> tb_sample
  #   
  #   # group means
  #   tb_sample %>% 
  #     group_by(gp, var) %>% 
  #     summarize(across(diff_midpt:diff_q1090, mean)) %>% 
  #     ungroup() %>% 
  #     mutate(across(starts_with("diff"), ~round(.x, 2))) %>% 
  #     arrange(var, gp) %>% 
  #     View()
  #   
  #   tb_sample %>% 
  #     group_by(gp, var) %>% 
  #     summarize(across(diff_midpt:diff_q1090, range)) %>% 
  #     ungroup() %>% 
  #     mutate(across(starts_with("diff"), ~round(.x, 2))) %>% 
  #     arrange(var, gp) %>% 
  #     View()
  #   
  #   tb_sample %>%
  #     select(gp,x,y,var,model) -> tb_sample
  #   
  #   tb_sample %>% 
  #     arrange(var, gp) %>% 
  #     mutate(id = row_number(), .before = 1) -> tb_sample
  # }
  # 
  # write_csv(tb_sample, "output/tb_sample_v02.csv")
}

read_csv("output/tb_sample_v02.csv") -> tb_sample



# GENERATE TABLE WITH METRICS
"output/" %>%
  list.files(full.names = T) %>%
  .[str_detect(., dom)] %>%
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


# JOIN
tb_sample %>% 
  left_join(tb_allvars_allmods) -> tb_sample


# PLOTS TO TRAIN
tb_sample %>% 
  # .[148,] %>%
  future_pmap(function(id, gp,
                       x,y, 
                       var, model,
                       
                       time_bkpt,
                       diff_midpt, diff_median, diff_mean,
                       diff_range, diff_stddev, diff_q1090){
    
    model %>% 
      str_split("_", simplify = T) -> m
  
    func_retrieve_ts(m[,1], m[,2], var, x, y) -> tb
    
    str_glue(
      "midpt = {round(diff_midpt,2)} // median = {round(diff_median,2)} // mean =  {round(diff_mean,2)}
      range = {round(diff_range,2)} // stddev = {round(diff_stddev,2)} // q 10 90 = {round(diff_q1090, 2)}"
    ) -> labs_metrics
    
    which(tb$time == time_bkpt) -> bkpt
    {bkpt-12*40} %>% 
      {ifelse(.<0, 0, .)} -> lim1
    
    tb$val[lim1:(bkpt+12*40)] %>% 
      quantile(c(0.005, 0.995), na.rm = T) -> lim
    
    # boxplot.stats(tb$val, coef = 0.75)$stats %>% {c(.[1], .[5])} -> lim
    # # quantile(tb$val, c(0.005, 0.995), na.rm = T) -> lim
    tb$val[tb$val < lim[1] | tb$val > lim[2]] <- NA
    
    func_plot_ts_raw(tb$val, tb$time, time_bkpt) +
      labs(subtitle = labs_metrics,
           title = str_glue("ID: {id}  //  VAR: {str_to_upper(var)}  //  MOD: {model}  //  GP: {gp}"))
      
  }) -> l_plots

seq_along(l_plots) %>%
  {split(., ceiling(./4))} %>%
  map(function(p){

    patchwork::wrap_plots(l_plots[p], ncol = 1)

  }) -> l_l_plots






# *************

