library(psychrolib)
SetUnitSystem("SI")

map_dfr(seq(5, 50, by = 10), function(tmp){
  
  map_dfr(seq(0.1, 1, by = 0.1), function(rh){
    
    map_dfr(seq(50000, 100000, by = 10000), function(ps){
      
      wb <- GetTWetBulbFromRelHum(tmp, rh, ps)
      
      tibble(tas = tmp,
             rh = round(rh*100),
             ps = factor(ps),
             wb = wb)
      
    })
    
  })
  
}) -> tb


tb %>% 
  ggplot(aes(x = tas, y = wb, group = ps, color = ps)) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~rh, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





map_dfr(seq(5, 50, by = 10), function(tmp){
  
  map_dfr(seq(0.1, 1, by = 0.2), function(rh){
    
    wb <- GetTWetBulbFromRelHum(tmp, rh, pull(t, val) %>% na.omit)
    time <- seq_along(wb)
      
      tibble(tas = tmp,
             rh = round(rh*100),
             wb = wb,
             ts = time)
      
    
  })
  
}) -> tb


tb %>% 
  ggplot(aes(x = ts, y = wb, group = tas, color = tas)) +
  geom_line() +
  facet_wrap(~rh, scales = "free_y", nrow = 1) #+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))








