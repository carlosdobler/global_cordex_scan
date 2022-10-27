# source("~/00-mount.R")


dom <- "SAM"

v_short <- "ps" #"huss"  # "tas"  #"ps"

source("scripts/00_setup.R")
source("scripts/functions.R")


plan(multicore)


# *****************

tribble(
  ~v_sh, ~v, ~temp_res,
  "huss", "surface_specific_humidity", "daily",
  "ps", "surface_pressure", "daily",
  "tas", "average_temperature", "daily"
) -> tb_vars

tb_vars[tb_vars$v_sh == v_short, "v"] %>% pull() -> v
tb_vars[tb_vars$v_sh == v_short, "temp_res"] %>% pull() -> temp_res


# *****************

# OBTAIN MODELS

str_glue("~/bucket_risk/RCM_regridded_data/CORDEX_22/{dom}/{temp_res}/{v}/") %>% 
  list.files() %>% 
  str_split("_", simplify = T) %>% 
  .[,3] %>% 
  unique() -> mods_driving



# *****************


for(mod in mods_driving){
  
  
  print(str_glue("PROCESSING MOD {mod}"))
  
  # COPY FILES
  
  print("Copying files...")
  
  dir_var <- str_glue("~/pers_disk/{v}")
  dir.create(dir_var)
  
  str_glue("~/bucket_risk/RCM_regridded_data/CORDEX_22/{dom}/{temp_res}/{v}") %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., mod)] %>% 
    
    future_walk(function(f){
      
      f %>%
        str_replace("/home/cdobler/bucket_risk/", "gs://cmip5_data/") -> orig
      
      f %>%
        str_split("/", simplify = T) %>% 
        .[, ncol(.)] %>% 
        {str_glue("{dir_var}/{.}")} -> dest
      
      system(str_glue("gsutil cp {orig} {dest}"),
             ignore.stdout = TRUE, ignore.stderr = TRUE)
      
    })
  
  
  # TEMPORAL AGGREGATION
  
  if(temp_res == "daily"){
    
    
    print("Aggregating...")
    
    dir_var %>% 
      list.files(full.names = T) %>% 
      
      future_walk(function(f){
        
        f %>% 
          str_split("_", simplify = T) %>% 
          .[, ncol(.)] %>% 
          str_remove(".nc") %>% 
          str_sub(c(1,10), c(6,15)) %>% 
          str_flatten("-") -> new_dates
        
        f %>% 
          str_split("_", simplify = T) %>% 
          .[, -ncol(.)] %>%
          str_flatten("_") %>% 
          {str_glue("{.}_{new_dates}.nc")} -> dest
        
        system(str_glue("cdo monmean {f} {dest}"))
        
        file.remove(f)
        
      })
    
  }
  
  
  # *****************
  
  # TILING
  
  
  print("Tiling...")
  
  dir_var %>% 
    list.files(full.names = T) %>% 
    .[1] -> f
  
  source("scripts/tiling.R")
  
  
  # *****************
  
  
  # LOOP TILES
  dir_metrics <- str_glue("~/pers_disk/{v}_metrics")
  dir.create(dir_metrics)
  
  # select_chunks <- c(21,14,5,12,11,32,22,19)
  # pwalk(st_drop_geometry(chunks_ind)[select_chunks,], function(lon_ch, lat_ch, r, ...){             # *******
  pwalk(st_drop_geometry(chunks_ind), function(lon_ch, lat_ch, r, ...){
    
    print(str_glue("   PROCESSING TILE {r} / {nrow(chunks_ind)}"))
    
    # st_drop_geometry(chunks_ind)$lon_ch[3] -> lon_ch
    # st_drop_geometry(chunks_ind)$lat_ch[3] -> lat_ch
    # st_drop_geometry(chunks_ind)$r[3] -> r
    
    # load into stars
    
    print(str_glue("      Importing files"))
    
    cbind(start = c(lon_chunks[[lon_ch]][1], lat_chunks[[lat_ch]][1], 1),
          count = c(lon_chunks[[lon_ch]][2] - lon_chunks[[lon_ch]][1]+1,
                    lat_chunks[[lat_ch]][2] - lat_chunks[[lat_ch]][1]+1,
                    NA)) -> ncs
    
    dir_var %>% 
      list.files(full.names = T) %>% 
      
      future_map(function(f){
        
        f %>%
          read_ncdf(ncsub = ncs) %>% 
          suppressMessages()
        
      },
      .options = furrr_options(seed = NULL)
      ) %>% 
      do.call(c, .) -> s
    
    
    # remove ocean
    st_warp(land, s %>% slice(time, 1)) -> l
    
    s[is.na(l)] <- NA
    
    
    # calculate metrics
    {
      print(str_glue("      Calculating metrics"))
      tic(str_glue("         Done"))
      
      # extract and format date vector
      # (all days = 01, no PCICt format)
      st_get_dimension_values(s, "time") %>% 
        as.character() %>% 
        str_sub(end = 7) %>% 
        {str_glue("{.}-01")} %>% 
        as_date() -> d
      
      s %>%
        st_apply(c(1,2),
                 func_jump_7,
                 dates = d,
                 FUTURE = T,
                 future.seed = NULL,
                 .fname = "func") %>% 
        split("func") -> s
      
      toc()
    }
    
    # save
    saveRDS(s, str_glue("{dir_metrics}/{mod}_{str_pad(r, 2, 'left', '0')}.rds"))
    
  })
  
  
  # *****************
  
  # MOSAIC
  
  # obtain all tiles per row
  unique(chunks_ind$lat_ch) %>% 
    as.numeric() %>% 
    sort() %>% 
    map(function(i){
      chunks_ind %>% 
        filter(lat_ch == i) %>% 
        pull(r) %>% 
        str_pad(2, "left", "0")
    }) -> tiles
  
  
  # reference 
  # str_glue("~/bucket_risk/RCM_regridded_data/CORDEX_22/{dom}/daily/") %>% 
  #   list.dirs(recursive = F) %>% 
  #   .[str_detect(., "relative_humidity")] %>% 
  #   list.files(full.names = T) %>% 
  #   .[1] %>% 
  #   read_ncdf(ncsub = cbind(start = c(1,1,1),
  #                           count = c(NA,NA,1))) %>% 
  #   suppressMessages() %>% 
  #   adrop() -> s_ref
  
  s_proxy %>% 
    st_get_dimension_values("lon") -> ref_lon
  
  
  
  print(str_glue("Mosaicking..."))
  
  
  
  tiles %>%
    map(function(r){
      
      ff <- character()
      while(length(ff) == 0){
        
        dir_metrics %>% 
          list.files(full.names = T) %>% 
          # .[str_detect(., mod)] %>% 
          .[str_detect(., str_flatten(r,"|"))] -> ff
        
      }
      
      ff %>%
        map(readRDS) %>%
        {do.call(c, c(., along = 1))} -> roww
      
      matrix(NA, length(ref_lon), dim(roww)[2]) %>%
        st_as_stars() %>%
        st_set_dimensions(1, values = ref_lon) %>%
        st_set_dimensions(2, values = st_get_dimension_values(roww, "lat")) %>%
        st_set_crs(4326) -> mm
      
      mm %>%
        st_set_dimensions(1, name = "lon") %>%
        st_set_dimensions(2, name = "lat") -> mm
      
      st_warp(roww, mm) -> roww_mm
      
      
      # fill positions not in the row mosaic with NA
      
      roww_mm %>% 
        st_dimensions() -> roww_mm_dim
      
      # which lon values in the the full array of lon values are not in the row mosaic 
      which(!round(st_get_dimension_values(mm, "lon", center = F),1) %in% round(st_get_dimension_values(roww, "lon"),1)) -> ind_not
      
      names(roww_mm) %>%
        seq_along() %>% 
        map(function(i){
          
          roww_mm %>% 
            select(i) %>% 
            pull(1) -> r_m
          
          r_m[ind_not,] <- NA
          
          r_m %>% 
            st_as_stars() -> r_m
          
          st_dimensions(r_m) <- roww_mm_dim
          
          return(r_m)
          
        }) %>% 
        do.call(c, .) %>% 
        setNames(names(roww_mm)) -> roww_mm
      
      return(roww_mm)
      
    }#,
    #.options = furrr_options(seed = NULL)
    ) -> l_rows
  
  do.call(c, c(l_rows, along = 2)) -> metrics_map
  
  metrics_map %>% merge(name = "band") -> metrics_map
  
  saveRDS(metrics_map, str_glue("output/{dom}_{v_short}_issues_RegCM4_{mod}_v7.rds"))
  
  unlink(dir_var, recursive = T)
  unlink(dir_metrics, recursive = T)
  

  
}



# *****************

library(randomForest)
tree <- readRDS("output/tree.rds")


# the location of bkpt in AUS is consistently 1980;
# no need to search for bkpt beyond 1990
if(dom == "AUS"){
  end_date <- "1990-01-01"
} else {
  end_date <- "2003-01-01"
}


"output/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., dom)] %>% 
  .[str_detect(., "v7")] %>% 
  .[str_detect(., str_glue("_{v_short}_"))] -> metrics_maps

metrics_maps %>% 
  map_chr(function(f){
    
    f %>% 
      str_split("_", simplify = T) %>% 
      {str_glue("{.[,4]}_{.[,5]}")}
    
  }) -> maps_models


metrics_maps %>% 
  set_names(maps_models) %>% 
  future_map(function(f){
    
    readRDS(f) %>% 
      split("band") %>%
      {.} -> s
    
    # no need for anything after end_date 
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
    
  }) -> l_s_pred



# turn prediction maps into tables
l_s_pred %>% 
  imap(function(s, i){
    
    s %>%
      as_tibble() %>% 
      mutate(model = i)
    
    
  }) -> l_tb_pred


# plot as maps
l_tb_pred %>% 
  imap(function(tb, mod){
    
    # probability threshold
    th <- 0.75
    
    tb %>%
      filter(!is.na(b)) %>% 
      pull(b) %>% 
      {. > th} %>% 
      mean() %>% 
      {. * 100} -> prop
    
    tb %>% 
      mutate(bad = ifelse(b > th, 1L, 0L) %>% factor()) %>% 
      ggplot(aes(lon, lat, fill = bad)) +
      geom_raster(show.legend = F) +
      coord_equal() +
      scale_fill_manual(values = c("grey80", "red"),
                        na.value = "transparent") +
      theme(axis.title = element_blank()) +
      labs(subtitle = str_glue("VAR: {v}
                                   {mod}"),
           caption = str_glue("Prop: {round(prop,1)} %"))
    
    
    
  }) %>% 
  patchwork::wrap_plots(ncol = 3)


































print(v)
  
"output/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., dom)] %>% 
  .[str_detect(., "v7")] %>% 
  .[str_detect(., v)] -> metrics_maps

metrics_maps %>% 
  map_chr(function(f){
    
    f %>% 
      str_split("_", simplify = T) %>% 
      {str_glue("{.[,4]}_{.[,5]}")}
    
  }) -> maps_models

metrics_maps %>% 
  set_names(maps_models) %>% 
  future_map(function(f){
    
    readRDS(f) %>% 
      split("band") %>%
      {.} -> s
    
    # no need for anything after end_date 
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
    
  }) -> l_s_pred


# turn prediction maps into tables
l_s_pred %>% 
  map(function(l_v){
    
    l_v %>% 
      imap(function(s, i){
        
        s %>%
          as_tibble() %>% 
          mutate(model = i)
        
        
      })
    
  }) -> l_tb_pred


# plot as maps
l_tb_pred %>% 
  imap(function(l_v, v){
    
    l_v %>%
      # .[str_detect(names(l_v), "RegCM4") %>% which()] %>%
      
      imap(function(tb, mod){
        
        # probability threshold
        th <- 0.75
        
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



# overlay bad cells per variable
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

# overlay bad cells overall
l_maps %>% 
  # map(select, b) %>% 
  do.call(c, .) %>% 
  merge() %>% 
  st_apply(c(1,2), sum) %>% 
  mutate(b = ifelse(sum > 0, 1, sum)) %>% 
  select(b) -> s


# *****************
# EXPORT OVERLAY MAP

dir.create("~/bucket_mine/results/global_cordex_scan")

devtools::source_url("https://github.com/carlosdobler/global_spei_v2_ww/blob/main/scripts/write_nc.R?raw=TRUE")

s %>% 
  setNames("bad_cells") %>% 
  func_write_nc_notime(str_glue("~/bucket_mine/results/global_cordex_scan/{dom}_bad_cells_mask.nc"))

s %>% 
  as_tibble() %>% 
  filter(b == 1) %>% 
  mutate(x = x-0.1,
         y = y-0.1) %>% #ggplot(aes(x,y)) + geom_point()
  # select(b) %>% 
  write_csv(str_glue("~/bucket_mine/results/global_cordex_scan/{dom}_bad_cells_mask.csv"))



# *****************
# EXPORT TMAX MAP





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



