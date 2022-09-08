# **************************************************************************************************


























source("scripts/00_setup.R")
plan(multisession)

dom <- "SAM"
vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()

map(vars, function(v){
  
  # v <- "pr"
  
  list.files("output/", full.names = T) %>% 
    .[str_detect(., dom)] %>%
    .[str_detect(., "_pr_")] %>% 
    .[str_detect(., "RegCM4")] %>% 
    .[str_detect(., "v3_pr")] %>%
    
    map(function(f){
      
      f %>% 
        str_split(., "_", simplify = T) %>% 
        {str_glue("{.[4]}_{.[5]}")} -> nm
      
      readRDS(f) -> ss
      
      ss %>% 
        split("band") %>% 
        
        # mutate(
        #   midp_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980 | year(as_date(trend_time_bkpt)) > 2000, 0, midp_seg_diff),
        #   mean_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980 | year(as_date(trend_time_bkpt)) > 2000, 0, mean_seg_diff),
        #   sprd_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980 | year(as_date(trend_time_bkpt)) > 2000, 0, sprd_seg_diff),
        #   sprd2_seg_diff = ifelse(year(as_date(sprd2_time_bkpt)) < 1980 | year(as_date(sprd2_time_bkpt)) > 2000, 0, sprd2_seg_diff)
        # ) %>% 
        {.} -> ss
      
      # if(v != "pr"){
      
      ss %>% 
        mutate(
          test = ifelse(
            midp_seg_diff >= 0.2 |
              sprd_seg_diff >= 0.35 |
              (mean_seg_diff >= 0.2 & midp_seg_diff > 0.18) |
              sprd2_seg_diff >= 0.35,
            
            2,
            1
          )
        ) %>% 
        
        select(test) %>% 
        setNames(nm)
      
      # } else {
      #   
      #   ss %>% 
      #     mutate(
      #       test = ifelse(
      #         # midp_seg_diff >= 0.2 |
      #         # sprd_seg_diff >= 0.35 |
      #         # (mean_seg_diff >= 0.2 & midp_seg_diff > 0.15) |
      #         mean_seg_diff >= 0.3 |
      #         sprd2_seg_diff >= 0.4,
      #         
      #         2,
      #         1
      #       )
      #     ) %>% 
      #     
      #     select(test) %>% 
      #     setNames(nm)
      #   
      # }
      
    }) %>% 
    do.call(c, .) -> s_q
  
  s_q %>% names() -> models
  
  s_q %>% 
    setNames(letters[seq_along(models)]) %>% 
    mutate(
      q = ifelse(a == 2 |
                   b == 2 |
                   c == 2,
                 2,
                 1)
    ) %>% 
    setNames(c(models, "q")) -> s_q
  
  s_q %>% 
    as_tibble() %>% 
    group_by(q) %>% 
    count() %>% 
    pull(n) %>% 
    {.[2]/.[1]*100} %>% 
    round(2) -> perc
  
  s_q %>% 
    as_tibble() %>% 
    group_by(q) %>% 
    count() %>% 
    filter(q == 2) %>% 
    pull(n) -> ct
  
  
  s_q %>% 
    select(q) %>% 
    st_as_sf(as_points = F, merge = T) -> pol_q
  
  ggplot() +
    geom_raster(data = as_tibble(select(s_q, q)), aes(x,y,fill = factor(q)), show.legend = F) +
    geom_sf(data = pol_q %>% filter(q == 2), fill = NA, color = "black") +
    colorspace::scale_fill_discrete_qualitative(na.value = "transparent") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = str_glue("VARIABLE: {v}"),
         subtitle = str_glue("Percentage of bad cells = {perc}%\nCount of bad cells = {ct}")) -> p
  
  list(s_q = s_q,
       p = p)
  
  
}) -> q_plots


map(q_plots, function(qp){
  
  qp[[1]] %>% 
    select(q)
  
}) %>% 
  do.call(c, .) -> s_q_allvars


s_q_allvars %>% 
  setNames(letters[1:6]) %>% 
  mutate(
    q = ifelse(a == 2 |
                 b == 2 |
                 c == 2 |
                 d == 2 |
                 e == 2 |
                 f == 2,
               2,
               1)
  ) %>% 
  setNames(c(vars, "q")) -> s_q


s_q %>% 
  as_tibble() %>% 
  group_by(q) %>% 
  count() %>% 
  pull(n) %>% 
  {.[2]/.[1]*100} %>% 
  round(2) -> perc

s_q %>% 
  as_tibble() %>% 
  group_by(q) %>% 
  count() %>% 
  filter(q == 2) %>% 
  pull(n) -> ct


s_q %>% 
  select(q) %>% 
  st_as_sf(as_points = F, merge = T) -> pol_q

ggplot() +
  geom_raster(data = as_tibble(select(s_q, q)), aes(x,y,fill = factor(q)), show.legend = F) +
  geom_sf(data = pol_q %>% filter(q == 2), fill = NA, color = "black") +
  colorspace::scale_fill_discrete_qualitative(na.value = "transparent") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = str_glue("ALL VARIABLES"),
       subtitle = str_glue("Percentage of bad cells = {perc}%\nCount of bad cells = {ct}")) -> p

list(s_q = s_q,
     p = p) -> q_plots[[7]]


map(q_plots, pluck(2))







mmm <- 3

list.files("output/", full.names = T) %>% 
  .[str_detect(., dom)] %>%
  .[str_detect(., v)] %>% 
  .[str_detect(., "RegCM4")] %>% 
  .[str_detect(., "v3")] %>%
  .[str_detect(., models[mmm])] %>% 
  readRDS() -> s_jump

models[mmm] %>% 
  str_split("_", simplify = T) %>% 
  .[,1] -> rm

models[mmm] %>% 
  str_split("_", simplify = T) %>% 
  .[,2] -> mm

str_glue("~/bucket_mine/remo/monthly/{dom}-22/{v}") %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "regrid")] %>% 
  .[str_detect(., rm)] %>% 
  .[str_detect(., mm)] %>%
  
  future_map(function(f){
    
    f %>% 
      str_split("_", simplify = T) %>% 
      .[,10] -> date_range
    
    date_range %>% str_sub(end = 6) %>% str_c("01") -> t_i
    date_range %>% str_sub(start = 8, end = 14) %>% str_c("01") -> t_f
    
    read_ncdf(f) %>% 
      st_set_dimensions("time",
                        values = seq(as_date(t_i), as_date(t_f), by = "1 month"))
    
  }) %>% 
  suppressMessages() %>% 
  suppressWarnings() %>% 
  do.call(c, .) -> s


s_q %>% 
  select(mmm) %>% 
  as_tibble() %>%
  rename(v = 3) %>% 
  filter(v == 2) %>% 
  # filter(near(x, -36, 2)) %>%
  
  slice_sample() -> miau

s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau$x)),
  which.min(abs(st_get_dimension_values(s, "lat") - miau$y)),
] %>% pull(1) %>% as.vector() -> x

tibble(time = st_get_dimension_values(s, "time"),
       v = x) %>%
  {
    ggplot(., aes(time, v)) +
      geom_point(alpha = 0.5, size = 2) +
      # geom_smooth(color = NA, fill = "red", method = "loess", span = 0.1, alpha = 0.3) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) #+
    # scale_y_continuous(limits = c(0, 0.0002))
  }

s_jump %>% 
  split("band") %>% 
  .[,
    which.min(abs(st_get_dimension_values(s_jump, "x") - miau$x)),
    which.min(abs(st_get_dimension_values(s_jump, "y") - miau$y))] %>% 
  as_tibble() %>% 
  mutate(trend_time_bkpt = year(as_date(trend_time_bkpt)),
         sprd2_time_bkpt = year(as_date(sprd2_time_bkpt)))





# **************************************************************************************************



# pf tests -----


source("scripts/00_setup.R")
# library(zoo)

plan(multisession)


dom <- c("SAM", "EUR")[1]

vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()

dates <- seq(as_date("19700101"), as_date("21001201"), by = "1 month")

str_glue("~/bucket_mine/remo/monthly/{dom}-22/hurs/") %>% 
  list.files() %>% 
  str_split("_", simplify = T) %>% 
  {str_glue("{.[,6]}_{.[,3]}")} %>% 
  unique() -> models

tribble(
  ~met, ~th,
  "midp", 0.2,
  "sprd", 0.35,
  "mean", 0.2,
  "sprd2", 0.35,
  
  "midp2", 0.15
  
) -> tb_th


v <- vars[3]
m <- models[4]



# ***************************

# LOAD DATA

{
  m %>% 
    str_split("_", simplify = T) %>% 
    .[,1] %>% 
    str_split("-", simplify = T) %>% 
    .[,2] -> rm
  
  m %>% 
    str_split("_", simplify = T) %>% 
    .[,2] -> mm
  
  str_glue("~/bucket_mine/remo/monthly/{dom}-22/{v}") %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "regrid")] %>% 
    .[str_detect(., rm)] %>% 
    .[str_detect(., mm)] %>%
    
    future_map(function(f){
      
      f %>% 
        str_split("_", simplify = T) %>% 
        .[,10] -> date_range
      
      date_range %>% str_sub(end = 6) %>% str_c("01") -> t_i
      date_range %>% str_sub(start = 8, end = 14) %>% str_c("01") -> t_f
      
      read_ncdf(f) %>% 
        st_set_dimensions("time",
                          values = seq(as_date(t_i), as_date(t_f), by = "1 month"))
      
    }) %>% 
    suppressMessages() %>% 
    suppressWarnings() %>% 
    do.call(c, .) -> s
  
  
  list.files("output/", full.names = T) %>% 
    .[str_detect(., dom)] %>%
    .[str_detect(., v)] %>%
    .[str_detect(., mm)] %>% 
    .[str_detect(., rm)] %>%
    .[str_detect(., "v3", negate = F)] %>%
    readRDS() -> s_jump
  
  s_jump %>% 
    split("band") %>% 
    as_tibble() %>% 
    mutate(trend_time_bkpt = year(as_date(trend_time_bkpt)),
           sprd2_time_bkpt = year(as_date(sprd2_time_bkpt)),
           
           midp_seg_diff = ifelse(trend_time_bkpt < 1980, 0, midp_seg_diff),
           mean_seg_diff = ifelse(trend_time_bkpt < 1980, 0, mean_seg_diff),
           sprd_seg_diff = ifelse(trend_time_bkpt < 1980, 0, sprd_seg_diff),
           
           sprd2_seg_diff = ifelse(sprd2_time_bkpt < 1980, 0, sprd2_seg_diff)) -> tb
  
}

metric_ <- c("midp", "sprd", "mean")[2]

tb %>% 
  select(-starts_with("sprd2")) %>% 
  filter(midp_seg_diff < tb_th %>% filter(met == "midp") %>% .$th) %>% # activate with [2]
  # filter(sprd_seg_diff < tb_th %>% filter(met == "sprd") %>% .$th) %>% # activate with [3]
  
  rename("metric" = starts_with(metric_)) -> tb_post

tb_th %>% 
  filter(met == metric_) %>% 
  .$th -> th

# ***************************
# WITH TREND BREAKPOINT

{
  
  if(metric_ != "mean"){
    bind_rows(
      tb_post %>% 
        filter(metric > th) %>% 
        filter(near(metric, quantile(metric, 0.9, type = 3, na.rm = T), 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Above, high"),
      
      tb_post %>% 
        filter(metric > th) %>% 
        filter(near(metric, th, 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Above, low"),
      
      tb_post %>% 
        filter(metric <= th) %>% 
        filter(near(metric, th, 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Below, high"),
      
      tb_post %>% 
        filter(metric <= th) %>%
        filter(metric > 0) %>% 
        filter(near(metric, quantile(metric, 0.1, type = 3, na.rm = T), 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Below, low")
      
    ) -> tb_ts_select
    
  } else {
    
    bind_rows(
      tb_post %>%
        filter(midp_seg_diff > tb_th %>% filter(met == "midp2") %>% .$th) %>% 
        filter(metric > th) %>% 
        filter(near(metric, quantile(metric, 0.9, type = 3, na.rm = T), 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Above, high"),
      
      tb_post %>% 
        filter(midp_seg_diff > tb_th %>% filter(met == "midp2") %>% .$th) %>%
        filter(metric > th) %>% 
        filter(near(metric, th, 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Above, low"),
      
      tb_post %>% 
        filter(midp_seg_diff <= tb_th %>% filter(met == "midp2") %>% .$th) %>% 
        filter(metric <= th) %>% 
        filter(near(metric, th, 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Below, high"),
      
      tb_post %>% 
        filter(midp_seg_diff <= tb_th %>% filter(met == "midp2") %>% .$th) %>% 
        filter(metric <= th) %>%
        filter(metric > 0) %>% 
        filter(near(metric, quantile(metric, 0.1, type = 3, na.rm = T), 0.03)) %>% 
        slice_sample() %>% 
        mutate(cat = "Below, low")
      
    ) -> tb_ts_select
    
  }
  
  
  pmap(tb_ts_select, function(x, y, cat, metric, trend_time_bkpt, ...){
    
    s[,
      which.min(abs(st_get_dimension_values(s, "lon") - x)),
      which.min(abs(st_get_dimension_values(s, "lat") - y)),
    ] %>% 
      setNames("vv") %>% 
      as_tibble() %>% 
      mutate(cat = cat,
             time = time %>% as.character() %>% as_date(),
             vv = set_units(vv, NULL)) -> tb_p
    
    ggplot(tb_p, aes(x = time, y = vv, fill = vv)) +
      geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
      #geom_vline(xintercept = str_glue("{trend_time_bkpt}-01-01") %>% as_date(), linetype = "2222") +
      scale_x_date(breaks = seq(min(tb_p$time), max(tb_p$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(tb_p$time), max(tb_p$time), by = "1 year")) +
      colorspace::scale_fill_continuous_sequential("Plasma") +
      theme(axis.title = element_blank()) +
      labs(subtitle = str_glue("Coef: {round(metric,3)}")) -> p
    
    if(cat != "Below, low"){
      
      if(cat == "Above, high"){
        p +
          labs(title = str_glue("[{metric_}]   {rm} - {mm} / VARIABLE: {v}")) -> p
        
      } 
      p +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    } else {
      
      return(p)
    }
    
  }) -> tb_ts_plots
  
  print(patchwork::wrap_plots(tb_ts_plots, ncol = 1))
  print(tb_ts_select)
  
}


# **************************

# WITH VARIANCE BREAKPOINT

{
  
  tb %>%
    # filter(!is.na(midp_seg_diff)) %>% 
    filter(midp_seg_diff < tb_th %>% filter(met == "midp") %>% .$th) %>%
    filter(sprd_seg_diff < tb_th %>% filter(met == "sprd") %>% .$th) %>% #arrange(desc(midp_seg_diff))
    filter(!(mean_seg_diff >= tb_th %>% filter(met == "mean") %>% .$th & midp_seg_diff > tb_th %>% filter(met == "midp2") %>% .$th)) -> tb_post_2
  
  bind_rows(
    tb_post_2 %>% 
      filter(sprd2_seg_diff > tb_th %>% filter(met == "sprd2") %>% .$th) %>% 
      filter(near(sprd2_seg_diff, quantile(sprd2_seg_diff, 0.9, type = 3, na.rm = T), 0.03)) %>% 
      slice_sample() %>% 
      mutate(cat = "Above, high"),
    
    tb_post_2 %>% 
      filter(sprd2_seg_diff > tb_th %>% filter(met == "sprd2") %>% .$th) %>% 
      filter(near(sprd2_seg_diff, th, 0.03)) %>% 
      slice_sample() %>% 
      mutate(cat = "Above, low"),
    
    tb_post_2 %>% 
      filter(sprd2_seg_diff <= tb_th %>% filter(met == "sprd2") %>% .$th) %>% 
      filter(near(sprd2_seg_diff, th, 0.03)) %>% 
      slice_sample() %>% 
      mutate(cat = "Below, high"),
    
    tb_post_2 %>% 
      filter(sprd2_seg_diff <= tb_th %>% filter(met == "sprd2") %>% .$th) %>% 
      filter(sprd2_seg_diff > 0) %>% 
      filter(near(sprd2_seg_diff, quantile(sprd2_seg_diff, 0.1, type = 3, na.rm = T), 0.03)) %>% 
      slice_sample() %>% 
      mutate(cat = "Below, low")
    
  ) -> tb_ts_select
  
  pmap(tb_ts_select, function(x, y, cat, sprd2_seg_diff, sprd2_time_bkpt, ...){
    
    s[,
      which.min(abs(st_get_dimension_values(s, "lon") - x)),
      which.min(abs(st_get_dimension_values(s, "lat") - y)),
    ] %>% 
      setNames("vv") %>% 
      as_tibble() %>% 
      mutate(cat = cat,
             time = time %>% as.character() %>% as_date(),
             vv = set_units(vv, NULL)) -> tb_p
    
    ggplot(tb_p, aes(x = time, y = vv, fill = vv)) +
      geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
      #geom_vline(xintercept = str_glue("{sprd2_time_bkpt}-01-01") %>% as_date(), linetype = "2222") +
      scale_x_date(breaks = seq(min(tb_p$time), max(tb_p$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(tb_p$time), max(tb_p$time), by = "1 year")) +
      colorspace::scale_fill_continuous_sequential("Plasma") +
      theme(axis.title = element_blank()) +
      labs(subtitle = str_glue("Coef: {round(sprd2_seg_diff,3)}")) -> p
    
    if(cat != "Below, low"){
      
      if(cat == "Above, high"){
        p +
          labs(title = str_glue("[sprd2]   {rm} - {mm} / VARIABLE: {v}")) -> p
        
      } 
      p +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    } else {
      
      return(p)
    }
    
  }) -> tb_ts_plots
  
  print(patchwork::wrap_plots(tb_ts_plots, ncol = 1))
  print(tb_ts_select)
}



# ***************************

# MAPS
{
  s_jump %>%
    split("band") %>% 
    as_tibble() %>% 
    # filter(!is.na(midp_seg_diff)) %>%
    
    mutate(
      
      midp_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980, 0, midp_seg_diff),
      mean_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980, 0, mean_seg_diff),
      sprd_seg_diff = ifelse(year(as_date(trend_time_bkpt)) < 1980, 0, sprd_seg_diff),
      
      sprd2_seg_diff = ifelse(year(as_date(sprd2_time_bkpt)) < 1980, 0, sprd2_seg_diff)
    ) %>% 
    
    filter(is.na(midp_seg_diff)  | midp_seg_diff < tb_th %>% filter(met == "midp") %>% .$th) %>% 
    filter(is.na(sprd_seg_diff)  | sprd_seg_diff < tb_th %>% filter(met == "sprd") %>% .$th) %>% 
    filter(is.na(mean_seg_diff)  | !(mean_seg_diff >= tb_th %>% filter(met == "mean") %>% .$th & midp_seg_diff > tb_th %>% filter(met == "midp2") %>% .$th)) %>% 
    filter(is.na(sprd2_seg_diff)  | sprd2_seg_diff <= tb_th %>% filter(met == "sprd2") %>% .$th) %>% 
    mutate(test = 1) %>% 
    select(x,y,test) -> tb_good
  
  s_jump %>%
    split("band") %>%
    mutate(a = ifelse(is.na(midp_seg_diff), NA, 1)) %>% 
    select(a) %>% 
    as_tibble() %>% 
    filter(!is.na(a)) %>% 
    left_join(tb_good, by = c("x", "y")) %>% 
    mutate(test = ifelse(is.na(test), 2, test)) -> t_qc
  
  t_qc %>% 
    mutate(num = 1) %>% 
    group_by(test) %>% 
    summarize(q = sum(num)) -> q_perc
  
  if(nrow(q_perc) == 1){
    q_perc <- 0
  } else {
    q_perc %>% 
      pull(q) %>% 
      {round(.[2]/.[1]*100, 2)} -> q_perc
  }
  
  ggplot() +
    geom_raster(data = t_qc, aes(x,y, fill = factor(test)), show.legend = F) +
    # geom_sf(data = pol_qc %>% filter(test == 0), color = "black", fill = NA) +
    colorspace::scale_fill_discrete_qualitative(na.value = "transparent") +
    coord_equal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = str_glue("VARIABLE: {v}"),
         subtitle = str_glue("{rm} - {mm}"),
         caption = str_glue("Percentage of bad cells = {q_perc}"))
}



# **********

t_qc %>% 
  filter(test == 2) %>% 
  slice_sample() -> miau 

s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau$x)),
  which.min(abs(st_get_dimension_values(s, "lat") - miau$y)),
] %>% pull(1) %>% as.vector() -> x

tibble(time = st_get_dimension_values(s, "time"),
       v = x) %>%
  {
    ggplot(., aes(time, v)) +
      geom_point(alpha = 0.5, size = 2) +
      # geom_smooth(color = NA, fill = "red", method = "loess", span = 0.1, alpha = 0.3) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) 
  }



# *************************************************************************************************
# END




























rr <- 3

s[,
  which.min(abs(st_get_dimension_values(s, "lon") - tb_ts_select[rr,1:2] %>% as.matrix() %>% .[,1])),
  which.min(abs(st_get_dimension_values(s, "lat") - tb_ts_select[rr,1:2] %>% as.matrix() %>% .[,2])),
] %>% pull(1) %>% as.vector() -> x


tibble(time = st_get_dimension_values(s, "time"),
       v = x) %>%
  {
    ggplot(., aes(time, v)) +
      geom_point(alpha = 0.5, size = 2) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
  }

func_jump_3(x, dates[1:length(x)])[c(2,3,5,6)]
as_date(func_jump_3(x, dates[1:length(x)])[c(1,4)])


# TEST ********************************

# s_jump %>% 
#   split("band") %>% 
#   as_tibble() %>% 
#   
#   arrange(desc(mean_seg_diff)) %>%
#   # mutate(sprd_seg_diff = ifelse(mean_seg_diff > 0.137, NA, sprd_seg_diff)) %>% arrange(desc(sprd_seg_diff)) %>%
#   
#   mutate(mean_time_bkpt = year(as_date(mean_time_bkpt)),
#          sprd_time_bkpt = year(as_date(sprd_time_bkpt))) -> miau
# 
# rr <- 682
# miau[rr,]
# 
# s[,
#   which.min(abs(st_get_dimension_values(s, "lon") - miau[rr,1:2] %>% as.matrix() %>% .[,1])),
#   which.min(abs(st_get_dimension_values(s, "lat") - miau[rr,1:2] %>% as.matrix() %>% .[,2])),
# ] %>% pull(1) %>% as.vector() -> x
# 
# tibble(time = st_get_dimension_values(s, "time") %>% as.character() %>% as_date(),
#        v = x) %>%
#   {
#     ggplot(., aes(time, v)) +
#       geom_point(alpha = 0.5, size = 2) +
#       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
#                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
#   }


# END TEST ****************************





# plots basic ----
tibble(time = seq(as_date("19700101"), as_date("21001201"), by = "1 month")[1:length(x)],
       v = x_clean,
       vv = x,
       top = x_outliers[,1],
       bot = x_outliers[,2]) %>%
  {
    ggplot(., aes(x = time)) +
      
      geom_line(aes(y = top)) +
      geom_line(aes(y = bot)) +
      geom_point(aes(y = vv), size = 1.7, color = "red") +
      geom_point(aes(y = v), size = 2) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
  }



tibble(time = dates[1:length(x_trend_cut)],
       v = x_trend_cut) %>%
  {
    ggplot(., aes(x = time)) +
      # geom_point(aes(y = vv), size = 1.7, color = "red") +
      geom_point(aes(y = v), size = 2) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
  }


tibble(time = dates[1:length(x)],
       v = x) %>%
  {
    ggplot(., aes(x = time)) +
      # geom_point(aes(y = vv), size = 1.7, color = "red") +
      geom_point(aes(y = v), size = 2) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
  }


#




















# **************************************************************************************************

s_jump %>% 
  split("band") %>% 
  select(c(1,3)) %>% 
  setNames(c("mean", "sprd")) %>% 
  as_tibble() %>% 
  filter(mean > 0.15,
         mean < 0.16) %>% 
  slice(10) -> s_mask_1































modd <- 6
s <- l_s_models_masked[[modd]]

l_s_jump[[modd]] %>% 
  as_tibble() %>% 
  
  arrange(desc(mean_seg_diff)) %>%
  # mutate(sprd_seg_diff = ifelse(mean_seg_diff > 0.18, NA, sprd_seg_diff)) %>% arrange(desc(sprd_seg_diff)) %>%
  
  mutate(mean_time_bkpt = year(as_date(mean_time_bkpt)),
         sprd_time_bkpt = year(as_date(sprd_time_bkpt))) -> miau


{
  miau %>% filter(near(mean_seg_diff, 0.2, 0.05)) %>% slice_sample() -> guau
  # miau %>% filter(near(sprd_seg_diff, 0.45, 0.1)) %>% slice_sample() -> guau
  print(guau)
  s[,
    which.min(abs(st_get_dimension_values(s, "lon") - guau[,1:2] %>% as.matrix() %>% .[,1])),
    which.min(abs(st_get_dimension_values(s, "lat") - guau[,1:2] %>% as.matrix() %>% .[,2])),
  ] %>% pull(1) %>% as.vector() -> x
  
  tibble(time = st_get_dimension_values(s, "time"),
         v = x) %>%
    {
      ggplot(., aes(time, v)) +
        geom_point(alpha = 0.5, size = 2) +
        # geom_smooth(color = NA, fill = "red", method = "loess", span = 0.1, alpha = 0.3) +
        scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                     minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) 
    }
  
}

# rr <- 2
# miau[rr,]

s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau[rr,1:2] %>% as.matrix() %>% .[,1])),
  which.min(abs(st_get_dimension_values(s, "lat") - miau[rr,1:2] %>% as.matrix() %>% .[,2])),
] %>% pull(1) %>% as.vector() -> x

tibble(time = st_get_dimension_values(s, "time"),
       v = x) %>%
  {
    ggplot(., aes(time, v)) +
      geom_point(alpha = 0.5, size = 2) +
      scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                   minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
  }

#









modd <- 
  tb_files$model %>% 
  unique() %>%
  set_names() %>% 
  
  
  
  -72.5
-14.7

"~/pers_disk/hurs_f/mosaic_REMO2015_MOHC-HadGEM2-ES.rds" %>% readRDS() -> s

s %>% slice(band, 1) %>% mapview::mapview()

s %>% slice(band, 1) %>% mapview::mapview() #as_tibble() %>% 
filter(values > 0.18) %>% 
  slice_sample() -> miau


# tb_files %>% 
#   filter(var == v,
#          model == "RegCM4_MOHC-HadGEM2-ES") %>% 
#   pull(file) %>% 
#   map(function(f){
#     str_glue("~/bucket_mine/remo/monthly/SAM-22/hurs/{f}") %>% 
#       read_ncdf()
#   }) %>% 
#   do.call(c, .) -> s_1_mod

s_1_mod[,
        which.min(abs(st_get_dimension_values(s_1_mod, "lon") - miau$x)),
        which.min(abs(st_get_dimension_values(s_1_mod, "lat") - miau$y)),
] %>% pull(1) %>% as.vector() %>% plot()






modd <- 1
ss <- l_s_jump[[modd]]
s <- l_s_models[[modd]]
ss %>% 
  mutate(mean_time_bkpt = year(as_date(mean_time_bkpt)),
         sprd_time_bkpt = year(as_date(sprd_time_bkpt)),
         
         mask = ifelse(mean_seg_diff <= 0.16 & sprd_seg_diff <= 0.33, 1, 0)) %>% 
  as_tibble() -> ss

ss %>% 
  filter(mask == 0) %>%
  slice_sample() -> miau
miau  
s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau$lon)),
  which.min(abs(st_get_dimension_values(s, "lat") - miau$lat)),
] %>%
  as_tibble() %>% 
  mutate(hurs = set_units(hurs, NULL)) %>% 
  ggplot(aes(time, hurs)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_x_date(breaks = seq(first(dates), last(dates), by = "5 years"), date_labels = "%Y",
               minor_breaks = seq(first(dates), last(dates), by = "1 year"))























# ****

library(rpart)

# -70, -15: jump
# -76, -12: diff std dev

which.min(abs(st_get_dimension_values(l_s_models[[1]], "lon", center = F) - -70)) -> rand_lon
which.min(abs(st_get_dimension_values(l_s_models[[1]], "lat", center = F) - -15)) -> rand_lat

which.min(abs(st_get_dimension_values(l_s_models[[1]], "lon", center = F) - -76)) -> rand_lon
which.min(abs(st_get_dimension_values(l_s_models[[1]], "lat", center = F) - -12)) -> rand_lat

l_s_models %>%
  imap_dfr(function(s, i){
    
    s[, rand_lon, rand_lat, ] %>% 
      pull(1) %>% 
      as.vector() -> x
    
    tibble(date = st_get_dimension_values(s, "time"),
           val = x,
           model = i)
    
  }) -> tb

tb %>% 
  ggplot(aes(x = date, y = val, color = model)) +
  # geom_line() +
  geom_point(show.legend = F, size = 0.6) +
  facet_wrap(~model, ncol = 1) +
  scale_x_date(breaks = seq(as_date("19800101"), as_date("21001201"), by = "20 years"))


# ****

tb %>% 
  filter(model == names(l_s_models)[6]) -> tb_mod

tb_mod$val -> x

ts(x, start = 1970, frequency = 12) -> x_ts

stl(x_ts, s.window = "periodic", t.window = 5*12) -> x_stl
# plot(x_stl)

# x_stl$time.series[,"trend"] -> x_stl_deseas
# x_stl$time.series[,"trend"] + x_stl$time.series[,"remainder"] -> x_stl_deseas



tb_mod %>% 
  ggplot(aes(date, val_spread)) +
  # geom_line() +
  geom_point(size = 0.8)








rpart(val ~ r, data = tb_mod, maxdepth = 1) -> tree

tb_mod %>% 
  mutate(seg = tree$where) %>% 
  group_by(seg) %>% 
  summarize(val_deseas = mean(val_deseas)) %>% 
  pull(val_deseas) %>% 
  diff() %>% 
  abs() %>% 
  max()

tb_mod %>% 
  mutate(seg = tree$where) %>% 
  group_by(seg) %>% 
  summarize(date = max(date)) %>% 
  filter(year(date) < 2090) %>% 
  pull(date) %>% 
  as.integer()

# *****

rpart(val_spread ~ r, data = tb_mod, maxdepth = 1) -> tree

tb_mod %>% 
  mutate(seg = c(rep(first(tree$where), 25/2), tree$where, rep(last(tree$where), 25/2))) %>% 
  group_by(seg) %>% 
  summarize(val_deseas = sd(val_deseas)) %>% 
  pull(val_deseas) %>% 
  diff() %>%
  abs() %>% 
  max()

tb_mod %>% 
  mutate(seg = c(rep(first(tree$where), 25/2), tree$where, rep(last(tree$where), 25/2))) %>% 
  group_by(seg) %>% 
  summarize(date = max(date)) %>% 
  filter(year(date) < 2090) %>% 
  pull(date) %>% 
  as.integer()




# ***************

l_s_models %>% 
  map(function(s){
    
    st_get_dimension_values(s, "time") %>% 
      as.character() %>% 
      as_date() -> dates
    
    s %>% 
      st_apply(c(1,2), function(x){
        
        changepoint::cpt.mean(x, class = F, minseglen = 12*10) -> tree
        tree[1] -> bkpt
        
        # rpart(x ~ seq_along(x), minsplit = 12*20, maxdepth = 1)
        
        if(bkpt == length(x)){
          seg_diff <- 0
          time_bkpt <- NA
          
        } else {
          
          dates[bkpt] %>% 
            as.integer() -> time_bkpt
          
          ts(x, start = 1970, frequency = 12) %>% 
            stl(s.window = "periodic", t.window = 5*12) %>%
            .$time.series %>% 
            .[, "trend"] %>% 
            as.vector() -> x_trend
          
          scales::rescale(x_trend, from = quantile(x, c(0.02, 0.98))) %>%
            {case_when(. > 1 ~ 1,
                       . < 0 ~ 0,
                       TRUE ~ .)} -> x_scaled
          
          range1 <- (bkpt-20*12):(bkpt-1) %>% .[. > 0]
          range2 <- (bkpt+1):(bkpt+20*12) %>% .[. <= 1560]
          
          rg1 <- x_scaled[range1] %>% na.omit() %>% mean()
          rg2 <- x_scaled[range2] %>% na.omit() %>% mean()
          
          seg_diff <- abs(rg1 - rg2)
          
        }
        
        c(seg_diff = seg_diff,
          time_bkpt = time_bkpt)
        
      },
      FUTURE = T,
      future.seed = NULL,
      .fname = "func") %>% 
      split("func")
    
  }) -> l_s_jump_mean


l_s_jump_mean %>% 
  map(function(s){
    
    s %>% 
      mutate(mask = ifelse(seg_diff >= 0.16, 0, 1))
    
  }) -> l_s_jump_mean


modd <- 6
ss <- l_s_jump_mean[[modd]]
s <- l_s_models[[modd]]

ss %>% 
  mutate(time_bkpt = year(as_date(time_bkpt))) %>% 
  as_tibble() %>% #.$seg_diff %>% range(na.rm = T)
  
  filter(near(seg_diff, 0.16, 0.005)) %>%
  # filter(seg_diff <= 0.15)
  # filter(mask == 0) %>% #.$seg_diff %>% range
  slice_sample() -> miau

miau  
s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau$lon)),
  which.min(abs(st_get_dimension_values(s, "lat") - miau$lat)),
] %>%
  as_tibble() %>% 
  mutate(hurs = set_units(hurs, NULL)) %>% 
  ggplot(aes(time, hurs)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_x_date(breaks = seq(first(dates), last(dates), by = "5 years"), date_labels = "%Y",
               minor_breaks = seq(first(dates), last(dates), by = "1 year"))













# ******
# SD

l_s_models %>% 
  map(function(s){
    
    st_get_dimension_values(s, "time") %>% 
      as.character() %>% 
      as_date() -> dates
    
    s %>% 
      st_apply(c(1,2), function(x){
        
        ts(x, start = 1970, frequency = 12) %>% 
          stl(s.window = "periodic", t.window = 15*12) -> x_stl
        
        x_stl$time.series[,"seasonal"] + x_stl$time.series[,"remainder"] -> x_detrend
        
        x_detrend %>% 
          as.vector() -> x_detrend
        
        x_detrend[1:(12*60)] -> x_detrend_cut
        
        changepoint::cpt.var(x_detrend_cut, class = T, minseglen = 10*12) -> tree
        tree@cpts[1] -> bkpt
        
        if(bkpt == length(x_detrend_cut)){
          seg_diff <- 0
          time_bkpt <- NA
          
        } else {
          
          dates[bkpt] %>% 
            as.integer() -> time_bkpt
          
          # scales::rescale(x_detrend_cut, from = quantile(x_detrend_cut, c(0.01, 0.99))) %>%
          #   {case_when(. > 1 ~ 1,
          #              . < 0 ~ 0,
          #              TRUE ~ .)} -> x_scaled
          
          # scales::rescale(x_detrend_cut) -> x_scaled
          
          # scales::rescale(x_detrend, from = quantile(x_detrend_cut, c(0.005, 0.995))) %>%
          #   {case_when(. > 1 ~ 1,
          #              . < 0 ~ 0,
          #              TRUE ~ .)} -> x_scaled
          
          range1 <- (bkpt-20*12-3):(bkpt-3) %>% .[. > 0]
          range2 <- (bkpt+3):(bkpt+20*12+3) %>% .[. <= 1560]
          
          rg1 <- x_detrend[range1] %>% na.omit() %>% quantile(c(0.02, 0.98), type = 3) %>% unname() %>% diff()
          rg2 <- x_detrend[range2] %>% na.omit() %>% quantile(c(0.02, 0.98), type = 3) %>% unname() %>% diff()
          
          if(rg1 > rg2){
            # r <- x_detrend[range2] %>% 
            #   scales::rescale(from = quantile(x_detrend[range1], c(0.01, 0.99), type = 3))
            r <- 1-(rg2/rg1)
            
          } else {
            # r <- x_detrend[range1] %>% 
            #   scales::rescale(from = quantile(x_detrend[range2], c(0.01, 0.99), type = 3))
            r <- 1-(rg1/rg2)
            
          }
          
          # r %>% 
          #   na.omit() %>% 
          #   quantile(c(0.01, 0.99), type = 3) %>% 
          #   unname() %>% 
          #   diff() %>% 
          #   {1 - .} -> seg_diff
          
          seg_diff <- r
          
          
          # seg_diff <- abs(diff(rg1) - diff(rg2))
          # # abs(sd(rg1, na.rm = T)/mean(rg1, na.rm = T) - sd(rg2, na.rm = T)/mean(rg2, na.rm = T)) -> seg_diff
          # abs(diff(quantile(rg1, c(0.2,0.8), na.rm = T)) - diff(quantile(rg2, c(0.2,0.8), na.rm = T))) %>% unname() -> seg_diff
          # # abs(diff(range(rg1, na.rm = T)) - diff(range(rg2, na.rm = T))) -> seg_diff
          
        }
        
        c(seg_diff = seg_diff,
          time_bkpt = time_bkpt)
        
      },
      FUTURE = T,
      future.seed = NULL,
      .fname = "func") %>% 
      split("func") #-> ss     # *************************************
    
  }) -> l_s_jump_cv


map2(l_s_jump_cv, l_s_jump_mean, function(s_sd, s_mean){
  
  s_sd %>% 
    c(s_mean %>% select(mask)) %>% 
    mutate(seg_diff = ifelse(mask == 0, NA, seg_diff),
           time_bkpt = ifelse(mask == 0, NA, time_bkpt)) %>% 
    select(-mask)
  
}) -> l_s_jump_cv_m1


modd <- 6
ss <- l_s_jump_cv_m1[[modd]]
s <- l_s_models[[modd]]

ss %>% 
  mutate(time_bkpt = year(as_date(time_bkpt))) %>% 
  as_tibble() %>% #.$seg_diff %>% range(na.rm = T)
  
  # filter(near(abs(seg_diff), 0.4, 0.05)) %>%
  filter(abs(seg_diff) > 0 & abs(seg_diff) <= 0.25) %>%
  # filter(time_bkpt > 2010) %>% 
  # filter(mask == 0) %>% #.$seg_diff %>% range
  slice_sample() -> miau

miau  
s[,
  which.min(abs(st_get_dimension_values(s, "lon") - miau$lon)),
  which.min(abs(st_get_dimension_values(s, "lat") - miau$lat)),
] %>%
  as_tibble() %>% 
  mutate(hurs = set_units(hurs, NULL)) %>% 
  ggplot(aes(time, hurs)) +
  geom_point(size = 2, pch = 1, alpha = 0.8) +
  # geom_hex() +
  # colorspace::scale_fill_continuous_sequential("Plasma", rev = F) +
  scale_x_date(breaks = seq(first(dates), last(dates), by = "5 years"), date_labels = "%Y",
               minor_breaks = seq(first(dates), last(dates), by = "1 year"))




l_s_jump_mean[[6]] %>% select(1) %>% .[,
                                       which.min(abs(st_get_dimension_values(s, "lon") - -75.9)),
                                       which.min(abs(st_get_dimension_values(s, "lat") - -12.1))]




l_s_jump_mean[[6]] %>%
  mutate(time_bkpt = year(as_date(time_bkpt))) %>%
  
  # mutate(seg_diff = ifelse(mean_diff < 0.9, NA, mean_diff),
  #        time_bkpt = ifelse(mean_diff < 0.9, NA, time_bkpt)) %>% 
  
  select(3) %>%
  mapview::mapview()



s[,
  which.min(abs(st_get_dimension_values(s, "lon") - -75.9)),
  which.min(abs(st_get_dimension_values(s, "lat") - -12.1)),
] %>% pull(1) %>% as.vector() -> x



tibble(y = x, x = dates) %>% 
  ggplot(aes(x,y)) +
  geom_point() +
  scale_x_date(breaks = seq(first(dates), last(dates), by = "5 years"), date_labels = "%Y",
               minor_breaks = seq(first(dates), last(dates), by = "1 year"))



