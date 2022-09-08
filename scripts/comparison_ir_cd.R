"scripts/sam_water_runde.csv" %>% read_csv() -> err_ir

err_ir %>% 
  mutate(x = round(x+0.1,1),
         y = round(y+0.1,1)) -> err_ir


l_tb_pred %>%
  pluck("pr") %>% 
  {.[str_detect(names(.), "RegCM4")]} %>% 
  bind_rows() %>% 
  mutate(x = round(x,1),
         y = round(y,1)) -> err_mine


left_join(err_ir, err_mine, by = c("x", "y")) -> err_both

err_both %>% 
  ggplot(aes(x = b)) +
  geom_histogram(bins = 10)

{
  err_both %>% 
    group_by(...1) %>% 
    summarise(b = max(b)) %>% 
    filter(b < 0.5) %>% 
    slice_sample() %>% 
    pull(...1) -> i
  
  err_both %>% 
    filter(...1 == i) -> tb
  
  
  pmap(tb, function(x,y,model,b,...){
    
    model %>% 
      str_split("_", simplify = T) -> m
    
    func_retrieve_ts(m[,1], m[,2], "pr", x, y) -> t
    
    func_jump_7(t$val, t$time) -> a 
    
    which(t$time == as_date(a[1])) -> bkpt
    {bkpt-12*40} %>% 
      {ifelse(.<0, 0, .)} -> lim1
    t$val[lim1:(bkpt+12*40)] %>% 
      quantile(c(0.005, 0.995), na.rm = T) -> lim
    t$val[t$val < lim[1] | t$val > lim[2]] <- NA
    
    func_plot_ts_raw(t$val, t$time) + #, as_date(a[1])) +
      labs(title = str_glue("PROB: {b}  //  VAR: PR  //  MOD: {model}"))
    
  }) %>% 
    patchwork::wrap_plots(ncol = 1)
  
}

