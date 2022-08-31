
source("scripts/00_setup.R")
source("scripts/functions.R")
plan(multisession)
dom <- "SAM"

readRDS("~/projects/global_cordex_scan/output/tb_sample_Q_2.rds") -> tb_sample_Q_2

func_l_s <- function(v){
  
  # dir_tmp <- "~/pers_disk/tmp"
  # dir.create(dir_tmp)
  
  "output/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., dom)] %>% 
    .[str_detect(., "v4")] %>%
    .[str_detect(., v)] %>%
    .[str_detect(., "RegCM4")] %>% # *****
    str_split("_", simplify = T) %>% 
    {str_glue("{.[,4]}_{.[,5]}")} %>% 
    as.vector() %>% 
    set_names() %>% 
    map(function(m){
      
      
      m %>% 
        str_split("_", simplify = T) %>% 
        .[,1] -> rm
      
      m %>% 
        str_split("_", simplify = T) %>% 
        .[,2] -> mm
      
      str_glue("~/bucket_mine/remo/monthly/{dom}-22/{v}") %>% 
        list.files(full.names = T) %>% 
        .[str_detect(., "regrid")] %>% 
        .[str_detect(., rm)] %>% 
        .[str_detect(., mm)] %>%
        
        future_map(function(f){
          
          # f %>% 
          #   str_sub(start = 27) %>% 
          #   {str_glue("gs://clim_data_reg_useast1/{.}")} -> file_orig
          # 
          # 
          # f %>% 
          #   str_split("/", simplify = T) %>% 
          #   .[,ncol(.)] %>% 
          #   {str_glue("{dir_tmp}/{.}")} -> file_dest
          #   
          # system(str_glue("gsutil cp {file_orig} {file_dest}"))
          
          
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
        do.call(c, .)
      
    })
} 

v = "tasmax"

func_l_s(v) -> l_s

tb_sample_Q_2 %>% 
  filter(var == v) %>%
  # filter(Q == 1) %>% 
  mutate(r = row_number()) %>% 
  
  pmap_dfr(function(x,y,mod,r,...){
    
    l_s %>% 
      pluck(mod) -> s
    
    s[,
      which.min(abs(st_get_dimension_values(s, "lon") - x)),
      which.min(abs(st_get_dimension_values(s, "lat") - y)),
    ] %>% 
      as_tibble() %>% 
      mutate(r = r)
    
  }) -> tb_ts

tb_ts %>%
  rename(vv = 4) %>% 
  mutate(vv = vv %>% set_units(NULL)) -> tb_ts


# sample(unique(tb_ts$r), 1) -> rr
rr <- 9
{
tb_ts %>% 
  filter(r == rr) -> tb_ts_one
  
tb_ts_one %>% 
  pull(vv) -> x

tb_ts_one %>%
  pull(time) -> dates

tibble(xx = dates, yy = x) %>% 
  ggplot(aes(xx, yy)) +
  geom_point()
}
func_jump_4(x, dates)[-c(1,9)] %>% format(scientific = F)
func_jump_4(x, dates)[1] %>% as_date()
func_jump_4(x, dates)[9] %>% as_date()





