

# RETRIEVE TS FROM A CELL IN SAMPLE TABLE
r <- 227 # 595

tb_sample[r,] %>%
  mutate(rcm = str_split(model, "_", simplify = T)[,1],
         gcm = str_split(model, "_", simplify = T)[,2]) %>%
  select(rcm,gcm,var,x,y) %>%
  t() %>%
  as.vector() %>%
  {func_retrieve_ts(.[1],.[2],.[3],as.numeric(.[4]), as.numeric(.[5]))} -> a

# see result
func_jump_7(a$val, a$time) %>% as_tibble()

# see plot
func_plot_ts_raw(a$val, a$time, func_jump_7(a$val, a$time) %>% .[1] %>% as_date())

a$val -> x
a$time -> dates






# TEST METRICS

read_csv("output/tb_sample_v01.csv") -> tb_sample

tb_sample %>% 
  # .[227,] %>%
  future_pmap(function(id, 
                       x,y, 
                       var, model){
    
    model %>% 
      str_split("_", simplify = T) -> m
    
    func_retrieve_ts(m[,1], m[,2], var, x, y) -> tb
    
    func_jump_7(tb$val, tb$time) -> a 
    
    quantile(tb$val, c(0.01, 0.99), na.rm = T) -> lim
    # # boxplot.stats(tb$val, coef = 0.75)$stats %>% {c(.[1], .[5])} -> lim
    tb$val[tb$val < lim[1] | tb$val > lim[2]] <- NA
    
    func_plot_ts_raw(tb$val, tb$time, as_date(a[1])) +
      labs(title = str_glue("ID: {id}  //  VAR: {str_to_upper(var)}  //  MOD: {model}"),
           subtitle = str_glue(
             "midpt = {round(a[2],2)} // median = {round(a[3],2)} // mean =  {round(a[4],2)}
             range = {round(a[5],2)} // stddev = {round(a[6],2)} // q 10 90 = {round(a[7],2)}"))
    
  }) -> l_plots

seq_along(l_plots) %>%
  {split(., ceiling(./4))} %>%
  map(function(p){
    
    patchwork::wrap_plots(l_plots[p], ncol = 1)
    
  }) -> l_l_plots

l_l_plots[[sample(length(l_l_plots), 1)]]







tb_allvars_allmods %>% 
  filter(diff_mean > 0.3,
         diff_q1090 < 0.5) %>% 
  filter(var != "pr") %>%
  slice_sample() -> tb

{
  tb$model %>% 
    str_split("_", simplify = T) -> m
  
  func_retrieve_ts(m[,1], m[,2], tb$var, tb$x, tb$y) -> t
  
  func_jump_7(t$val, t$time) -> a 
  
  which(t$time == as_date(a[1])) -> bkpt
  {bkpt-12*40} %>% 
    {ifelse(.<0, 0, .)} -> lim1
  t$val[lim1:(bkpt+12*40)] %>% 
    quantile(c(0.005, 0.995), na.rm = T) -> lim
  t$val[t$val < lim[1] | t$val > lim[2]] <- NA
  
  func_plot_ts_raw(t$val, t$time, as_date(a[1])) +
    labs(title = str_glue("VAR: {str_to_upper(tb$var)}  //  MOD: {tb$model}"))
}








"scripts/water_runde.csv" %>% read_csv() -> err_ir



v = "tasmin"

l_tb_pred %>%
  pluck(v) %>% 
  pluck(m) %>% 
  filter(b > 0.9) %>% 
  slice_sample() -> tb

{
  tb$model %>% 
    str_split("_", simplify = T) -> m
  
  func_retrieve_ts(m[,1], m[,2], v, tb$x, tb$y) -> t
  
  func_jump_7(t$val, t$time) -> a 
  
  which(t$time == as_date(a[1])) -> bkpt
  {bkpt-12*40} %>% 
    {ifelse(.<0, 0, .)} -> lim1
  t$val[lim1:(bkpt+12*40)] %>% 
    quantile(c(0.005, 0.995), na.rm = T) -> lim
  t$val[t$val < lim[1] | t$val > lim[2]] <- NA
  
  func_plot_ts_raw(t$val, t$time, as_date(a[1])) +
    labs(title = str_glue("VAR: {str_to_upper(v)}  //  MOD: {tb$model}"))
}






{
  v <- sample(vars, 1)
  
  l_tb_pred %>%
    pluck(v) %>%
    bind_rows() %>% 
    filter(b > 0.5) %>% 
    slice_sample() -> tb
  
  tb$model %>% 
    str_split("_", simplify = T) -> m
  
  func_retrieve_ts(m[,1], m[,2], v, tb$x, tb$y) -> t
  
  func_jump_7(t$val, t$time) -> a 
  
  which(t$time == as_date(a[1])) -> bkpt
  {bkpt-12*40} %>% 
    {ifelse(.<0, 0, .)} -> lim1
  t$val[lim1:(bkpt+12*40)] %>% 
    quantile(c(0.005, 0.995), na.rm = T) -> lim
  t$val[t$val < lim[1] | t$val > lim[2]] <- NA
  
  func_plot_ts_raw(t$val, t$time, as_date(a[1])) +
    labs(title = str_glue("PROB: {tb$b}  //  VAR: {str_to_upper(v)}  //  MOD: {tb$model}"))
}







