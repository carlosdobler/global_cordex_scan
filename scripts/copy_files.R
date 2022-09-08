# COPY FILES

walk(vars, function(v){
  
  print(str_glue("COPYING VAR {v}"))
  
  dir_var <- str_glue("~/pers_disk/{v}")
  dir.create(dir_var)
  
  future_pwalk(tb_files %>% filter(var == v), function(file, var, ...){
    
    orig <- file %>%
      {str_glue("gs://clim_data_reg_useast1/remo/monthly/{dom}-22/{var}/{.}")}
    
    dest <- file %>%
      {str_glue("{dir_var}/{.}")}
    
    system(str_glue("gsutil cp {orig} {dest}"),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    
  })
  
})



# DELETE FILES
# walk(vars, function(v){
#   
#   dir_var <- str_glue("~/pers_disk/{v}")
#   
#   unlink(dir_var, recursive = T)
#   
# })

