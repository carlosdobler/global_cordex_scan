# source("~/00-mount.R")

source("scripts/00_setup.R")
source("scripts/functions.R")

library(zoo)
library(imputeTS)

plan(multicore)


# *****************

dom <- "SAM"

vars <- c("hurs", "rsds", "sfcWind", "tasmax", "tasmin", "pr") %>% set_names()


# *****************

# TILING

str_glue("~/bucket_mine/remo/monthly/{dom}-22/") %>% 
  list.dirs(recursive = F) %>% 
  .[1] %>% 
  list.files(full.names = T) %>%
  .[str_detect(., "REMO")] %>% 
  .[str_detect(., "Had")] %>% 
  .[str_detect(., "regrid")] %>% 
  .[str_detect(., "cut", negate = T)] %>% 
  .[1] -> f

source("scripts/tiling.R")


# *****************

# MAIN TABLE

{
  str_glue("~/bucket_mine/remo/monthly/{dom}-22") %>% 
    list.dirs(recursive = F) %>% 
    .[str_detect(., str_flatten(vars, "|"))] %>% 
    map_dfr(function(d){
      
      pos_model <- 3
      pos_rmodel <- 6
      pos_date <- 9
      
      tibble(file = d %>%
               list.files() %>% 
               .[str_detect(., "regrid")] %>%
               .[str_detect(., "cut", negate = T)]) %>%
        mutate(
          
          var = file %>%
            str_split("_", simplify = T) %>%
            .[ , 1],
          
          model = file %>%
            str_split("_", simplify = T) %>%
            .[ , pos_model],
          
          rmodel = file %>%
            str_split("_", simplify = T) %>%
            .[ , pos_rmodel] %>% 
            str_split("-", simplify = T) %>% 
            .[ , 2],
          
          t_i = file %>%
            str_split("_", simplify = T) %>%
            .[ , pos_date] %>%
            str_sub(end = 6) %>% 
            str_c("01"),
          
          t_f = file %>%
            str_split("_", simplify = T) %>%
            .[ , pos_date] %>%
            str_sub(start = 8, end = 14) %>% 
            str_c("01")
          
        )
    }) %>% 
    mutate(model = str_glue("{rmodel}_{model}")) %>% 
    select(-rmodel) -> tb_files
  
  if(dom == "EUR"){
    tb_files %>% 
      filter(!model %in% c("RegCM4_CNRM-CERFACS-CNRM-CM5", "RegCM4_ICHEC-EC-EARTH")) -> tb_files
  }
  
  tb_files %>% 
    filter(year(as_date(t_i)) >= 1970) -> tb_files
  
}


# *****************

# COPY FILES!

# *****************

# LOOP VARIABLES
walk(vars[-6], function(v){                                                                             # *******
  
  # v <- vars[6]
  
  print(str_glue("PROCESSING VAR {v}"))
  
  
  # LOOP TILES
  dir_metrics <- str_glue("~/pers_disk/{v}_metrics")
  dir.create(dir_metrics)
  
  # select_chunks <- c(21,14,5,12,11,32,22,19)
  # pwalk(st_drop_geometry(chunks_ind)[select_chunks,], function(lon_ch, lat_ch, r, ...){             # *******
  pwalk(st_drop_geometry(chunks_ind), function(lon_ch, lat_ch, r, ...){
    
    print(str_glue("   PROCESSING TILE {r} / {nrow(chunks_ind)}"))
    
    # st_drop_geometry(chunks_ind)$lon_ch[21] -> lon_ch
    # st_drop_geometry(chunks_ind)$lat_ch[21] -> lat_ch
    # st_drop_geometry(chunks_ind)$r[21] -> r
    
    # load into stars
    
    print(str_glue("      Importing files"))
    
    cbind(start = c(lon_chunks[[lon_ch]][1], lat_chunks[[lat_ch]][1], 1),
          count = c(lon_chunks[[lon_ch]][2] - lon_chunks[[lon_ch]][1]+1,
                    lat_chunks[[lat_ch]][2] - lat_chunks[[lat_ch]][1]+1,
                    NA)) -> ncs
    
    tb_files$model %>% 
      unique() %>%
      set_names() %>% 
      map(function(mod_){
        
        tb_files %>% 
          filter(var == v,
                 model == mod_) %>%
          
          future_pmap(function(file, t_i, t_f, var, ...){
            
            seq(as_date(t_i), as_date(t_f), by = "1 month") -> d
            
            file %>%
              {str_glue("~/pers_disk/{var}/{.}")} %>% 
              read_ncdf(ncsub = ncs) %>% 
              suppressMessages() %>% 
              st_set_dimensions("time", values = d)
            
          },
          .options = furrr_options(seed = NULL)
          ) %>% 
          do.call(c, .) -> s
        
      }) -> l_s_models
    
    
    # remove ocean
    l_s_models %>% 
      map(function(s){
        
        st_warp(land, s %>% slice(time, 1)) -> l
        
        s[is.na(l)] <- NA
        
        return(s)
        
      }) -> l_s_models_masked
    
    
    # calculate metrics
    {
      print(str_glue("      Calculating metrics"))
      tic(str_glue("         Done"))
      
      l_s_models_masked %>% 
        imap(function(s, i){
          
          tic(str_glue("         - {i} done"))
          
          st_get_dimension_values(s, "time") %>% 
            as.character() %>% 
            as_date() -> d
          
          {
            # l_s_models_masked[[5]] -> s
            # 
            # 
            # for(rr in seq_len(dim(s)[1])){
            #   for(cc in seq_len(dim(s)[2])){
            #     
            #     print(str_glue("{rr} - {cc}"))
            #     s[,rr,cc,] %>% pull(1) %>% as.vector() -> x
            #     func_jump_3(x, d)
            #     
            #   }
            # }
            # 
            # s[,31,33,] %>% pull(1) %>% as.vector() -> x
          } # for when this section fails
          
          s %>%
            st_apply(c(1,2),
                     func_jump_7,
                     dates = d,
                     FUTURE = T,
                     future.seed = NULL,
                     .fname = "func") %>% 
            split("func") -> s
          
          toc()
          
          return(s)
          
        }) -> l_s_jump
      toc()
    }
    
    # save
    l_s_jump %>% 
      iwalk(function(s, i){
        
        saveRDS(s, str_glue("{dir_metrics}/{i}_{str_pad(r, 2, 'left', '0')}.rds"))
        
      })
    
  })
  
  # mosaick
  
  print(str_glue("   MOSAICKING"))
  
  tb_files$model %>% 
    unique() %>%
    set_names() %>% 
    walk(function(mod_){
      
      list.files(dir_metrics, full.names = T) %>% 
        .[str_detect(., mod_)] %>% 
        map(readRDS) %>%
        map(merge) %>%
        map(setNames, "X") -> l_s
        
      if(dom == "AUS"){
        l_s %>% 
          map(function(s){
            
            st_get_dimension_values(s, "lon") -> dim_l
            
            if(all(dim_l <= 0)){
              s %>% 
                st_set_dimensions("lon",
                                  values = st_get_dimension_values(s, "lon", center = F) + 360) %>% 
                st_set_crs(4326) -> s
            }
            
            return(s)
            
          }) -> l_s
      }
      
      l_s %>% 
        map(as, "SpatRaster") %>% 
        do.call(terra::merge, .) %>% 
        st_as_stars() -> s_mos
      
      saveRDS(s_mos, str_glue("output/{dom}_{v}_issues_{mod_}_v7.rds"))
      
    })
  
  unlink(dir_metrics, recursive = T)
  
  print(str_glue(" "))
  
})













