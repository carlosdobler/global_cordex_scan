

func_plot_ts_raw <- function(x, dates, bkpt = NA){
  
  tibble(
    v = x,
    time = dates
  ) %>%
    
    {
      ggplot(., aes(x = time, y = v, fill = v)) +
        geom_point(shape = 21, alpha = 0.5, size = 2, show.legend = F) +
        scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
                     minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
        colorspace::scale_fill_continuous_sequential("Plasma") +
        theme(axis.title = element_blank())
    } -> p
  
  if(!is.na(bkpt)){
    p + geom_vline(xintercept = as_date(bkpt), alpha = 0.3, linetype = "2222") -> p
  }
  
  return(p)
  
}


# *****************


func_retrieve_ts <- function(rcm, gcm, vv, lon, lat){
  
  str_glue("~/pers_disk/{vv}") %>%
    list.files(full.names = T) %>% 
    .[str_detect(., dom)] %>% 
    .[str_detect(., rcm)] %>% 
    .[str_detect(., gcm)] %>% 
    read_stars(proxy = T) -> s
  
  s %>% st_get_dimension_values("x") -> coords_lon
  s %>% st_get_dimension_values("y") -> coords_lat
  
  which(near(coords_lon, lon)) -> pos_lon
  which(near(coords_lat, lat)) -> pos_lat
  
  s[,pos_lon,pos_lat,] %>% 
    st_as_stars() %>% 
    as_tibble() %>% 
    rename("val" = 4) %>% 
    drop_units() %>% 
    mutate(time = str_glue("{year(time)}-{str_pad(month(time), 2, 'left', '0')}-01") %>% as_date()) -> tb
  
  return(tb)
  
}


# *****************


func_remove_outliers <- function(bkpt_, x_){
  # 1st pass: to the left of bkpt
  w <- 15*12+1
  if(bkpt_ < w){
    w <- bkpt_  # in case bkpt is to the left of first window
  }
  
  rollapply(x_[1:bkpt_],
            FUN = function(y){
              
              sort(y) -> y
              c(y[1/10*w], y[1/2*w], y[9/10*w]) -> q
              
              q[1]-(q[2]-q[1])*0.5 -> q1
              q[3]+(q[3]-q[2])*0.5 -> q2
              c(q1,q2)
              
            },
            width = w,
            by = 24,
            fill = NA,
            align = "center"
  ) %>% 
    
    {
      cbind(na_locf(.[,1]),
            na_locf(.[,2]))
    } -> x_outliers_1
  
  
  # 2nd pass: to the right of bkpt
  w <- 15*12+1
  rollapply(x_[(bkpt_+1):length(x_)],
            FUN = function(y){
              
              sort(y) -> y
              c(y[1/10*w], y[1/2*w], y[9/10*w]) -> q
              
              q[1]-(q[2]-q[1])*0.5 -> q1
              q[3]+(q[3]-q[2])*0.5 -> q2
              c(q1,q2)
              
            },
            width = w,
            by = 24,
            fill = NA,
            align = "center"
  ) %>% 
    
    {
      cbind(na_locf(.[,1]),
            na_locf(.[,2]))
    } -> x_outliers_2
  
  rbind(x_outliers_1, x_outliers_2) -> x_outliers
  
  x_clean <- x_
  x_clean[x_ < x_outliers[,1] | x_ > x_outliers[,2]] <- NA
  na_interpolation(x_clean) -> x_clean
  
  return(list(x_clean = x_clean, x_outliers = x_outliers))
}


# *****************


func_jump_7 <- function(x, dates){
  
  count_na <- sum(is.na(x))
  
  if(count_na != length(x)){
    
    if(count_na > 0) na_interpolation(x) -> x
    
    
    # *********************************
    
    
    # DETECT BREAKPOINT
    
    x[1:which(dates == "2019-12-01")] -> x_cut
    
    changepoint::cpt.meanvar(x_cut, minseglen = 12*5, class = F)[1] %>% unname() -> bkpt
    
    if(bkpt == which(dates == "2019-12-01")){
      
      x_cut %>% 
        ts(start = first(dates) %>% {c(year(.), month(.))}, frequency = 12) %>% 
        stl("periodic", t.window = 12*10) -> x_stl
      
      x_stl %>% 
        .$time.series %>% 
        .[, "trend"] %>% 
        as.vector() -> x_trend
      
      rpart::rpart(x_trend ~ seq_along(x_trend), maxdepth = 1)$where %>% 
        unname() %>% 
        diff() %>% 
        {which(. != 0)} -> bkpt
      
    }
    
    
    # *********************************
    
    dates[bkpt] %>% 
      as.integer() -> time_bkpt
    
    
    # *********************************
    
    if(dom == "AUS"){
      ini_date <- "1970-01-01"
    } else {
      ini_date <- "1980-01-01"
    }
    
    if(bkpt > which(dates == ini_date) & bkpt < which(dates == "2009-12-01") & !is.na(bkpt)){
      
      # REMOVE OUTLIERS
      func_remove_outliers(bkpt, x) -> x_clean
      
      {
        # tibble(time = dates[1:length(x)],
        #        v = x_clean$x_clean,
        #        vv = x,
        #        top = x_clean$x_outliers[,1],
        #        bot = x_clean$x_outliers[,2]) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        # 
        #       geom_line(aes(y = top), linetype = "2222") +
        #       geom_line(aes(y = bot), linetype = "2222") +
        # 
        #       geom_point(aes(y = vv), size = 1.7, alpha = 0.5) +
        #       geom_point(aes(y = v), size = 1.7, color = "red") +
        # 
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
        #   }
      } # plot clean
      
      # SCALE TS
      {bkpt-12*30} %>% 
        {ifelse(.<0, 0, .)} -> lim1
      
      x_clean$x_clean[lim1:(bkpt+12*30)] %>% 
        range() -> range_sc
      
      x_clean$x_clean %>% 
        scales::rescale(from = range_sc) -> x_clean_sc
      
      
      {
        # tibble(time = dates,
        #        v = x_clean_sc) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        # 
        #       geom_point(aes(y = v), size = 1.7) +
        # 
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
        #       scale_y_continuous(breaks = seq(0,1,0.2))
        #   }
      } # plot scaled
      
      
      # *******************************
      
      # 1. CENTRAL TENDENCY METRICS
      
      # Obtain segments (10 yrs)
      seg_1 <- (bkpt-10*12-6):(bkpt-1-6) %>% .[. > 0]
      seg_2 <- (bkpt+1+6):(bkpt+10*12+6)
      
      x_seg_1 <- x_clean_sc[seg_1]
      x_seg_2 <- x_clean_sc[seg_2]
      
      
      # midpoint
      {
        boxplot.stats(x_seg_1, coef = 0.25)$stats %>%
          {(.[5] - .[1])/2+.[1]} -> rg1
        boxplot.stats(x_seg_2, coef = 0.25)$stats %>%
          {(.[5] - .[1])/2+.[1]} -> rg2
        
        # diff(range(x_seg_1))/2+min(x_seg_1) -> rg1
        # diff(range(x_seg_2))/2+min(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_midpt <- 0
        } else {
          diff_midpt <- abs(rg1-rg2)
        }
      }
      
      
      # median
      {
        median(x_seg_1) -> rg1
        median(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_median <- 0
        } else {
          diff_median <- abs(rg1-rg2)
        }
        
      }
      
      
      # mean
      {
        mean(x_seg_1) -> rg1
        mean(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_mean <- 0
        } else {
          diff_mean <- abs(rg1-rg2)
        }
        
      }
      
      
      # *******************************
      
      # 2. SPREAD METRICS
      
      # Obtain segments (15 yrs)
      seg_1 <- (bkpt-15*12-6):(bkpt-1-6) %>% .[. > 0]
      seg_2 <- (bkpt+1+6):(bkpt+15*12+6)
      
      x_seg_1 <- x_clean_sc[seg_1]
      x_seg_2 <- x_clean_sc[seg_2]
      
      
      # range
      {
        boxplot.stats(x_seg_1, coef = 0.75)$stats %>% 
          {.[5] - .[1]} -> rg1
        
        boxplot.stats(x_seg_2, coef = 0.75)$stats %>% 
          {.[5] - .[1]} -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_range <- 0
        } else {
          # diff_range <- abs(rg1-rg2)
          diff_range <- min(c(rg1,rg2))/max(c(rg1,rg2))
        }
      }
      
      
      # std dev
      {
        sd(x_seg_1) -> rg1
        sd(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_stddev <- 0
        } else {
          # diff_stddev <- abs(rg1-rg2)
          diff_stddev <- min(c(rg1,rg2))/max(c(rg1,rg2))
        }
        
      }
      
      
      # 10-90 perc
      {
        x_seg_1 %>% 
          quantile(c(0.1, 0.9)) %>% 
          unname() %>% 
          diff() -> rg1
        
        x_seg_2 %>% 
          quantile(c(0.1, 0.9)) %>% 
          unname() %>% 
          diff() -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_q1090 <- 0
        } else {
          # diff_q1090 <- abs(rg1-rg2)
          diff_q1090 <- min(c(rg1,rg2))/max(c(rg1,rg2))
        }
        
      }
      
      
    } else {
      
      diff_midpt <- 0
      diff_median <- 0
      diff_mean <- 0
      diff_range <- 0
      diff_stddev <- 0
      diff_q1090 <- 0
      
    }
    
    
  } else {
    
    time_bkpt <- NA
    diff_midpt <- NA
    diff_median <- NA
    diff_mean <- NA
    diff_range <- NA
    diff_stddev <- NA
    diff_q1090 <- NA
    
  }
  
  c(time_bkpt = time_bkpt,
    diff_midpt = diff_midpt,
    diff_median = diff_median,
    diff_mean = diff_mean,
    diff_range = diff_range,
    diff_stddev = diff_stddev,
    diff_q1090 = diff_q1090)
  
}




# *****************

func_jump_6 <- function(x, dates){
  
  count_na <- sum(is.na(x))
  
  if(count_na != length(x)){
    
    if(count_na > 0) na_interpolation(x) -> x
    
    
    # *********************************
    
    
    # # PRE-PROCESS TS
    # # 1. REMOVE OUTLIERS
    # w <- 15*12
    # rollapply(x,
    #           FUN = function(y){
    #             
    #             sort(y) -> y
    #             c(y[1/10*w], y[1/2*w], y[9/10*w]) -> q
    #             
    #             q[1]-(q[2]-q[1])*0.5 -> q1
    #             q[3]+(q[3]-q[2])*0.5 -> q2
    #             c(q1,q2)
    #             
    #           },
    #           width = w,
    #           by = 10,
    #           fill = NA,
    #           align = "center"
    # ) %>% 
    #   
    #   {
    #     cbind(na_locf(.[,1]),
    #           na_locf(.[,2]))
    #   } -> x_outliers
    # 
    # x_clean_pre <- x
    # x_clean_pre[x < x_outliers[,1] | x > x_outliers[,2]] <- NA
    # na_interpolation(x_clean_pre) -> x_clean_pre
    # 
    # {
    #   # tibble(time = dates,
    #   #        v = x_clean_pre,
    #   #        vv = x,
    #   #        top = x_outliers[,1],
    #   #        bot = x_outliers[,2]) %>%
    #   #   {
    #   #     ggplot(., aes(x = time)) +
    #   # 
    #   #       geom_line(aes(y = top), linetype = "2222") +
    #   #       geom_line(aes(y = bot), linetype = "2222") +
    #   # 
    #   #       geom_point(aes(y = vv), size = 1.7, alpha = 0.5) +
    #   #       geom_point(aes(y = v), size = 1.7, color = "red") +
    #   # 
    #   #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
    #   #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
    #   #   }
    # } # plot clean
    # 
    # # 2. RESCALE
    # scales::rescale(x_clean_pre) -> x
    
    
    # *********************************
    
    
    # DETECT BREAKPOINT 1 (MEAN)
    
    # changepoint::cpt.meanvar(x, minseglen = 12*5, class = F)[1] %>% unname() -> bkpt
    # strucchange::breakpoints(x~1, breaks = 1)$breakpoints -> bkpt
    
    x %>% 
      ts(start = first(dates) %>% {c(year(.), month(.))}, frequency = 12) %>% 
      stl("periodic", t.window = 12*10) -> x_stl
    
    x_stl %>% 
      .$time.series %>% 
      .[, "trend"] %>% 
      as.vector() -> x_trend
    
    x_trend[1:which(dates == "2019-12-01")] -> x_trend_cut
    
    rpart::rpart(x_trend_cut ~ seq_along(x_trend_cut), maxdepth = 1)$where %>% 
      unname() %>% 
      diff() %>% 
      {which(. != 0)} -> bkpt
    
    
    # *********************************
    
    dates[bkpt] %>% 
      as.integer() -> time_bkpt_m
    
    if(bkpt > which(dates == "1980-01-01") & bkpt < which(dates == "2009-12-01") & !is.na(bkpt)){
      
      # REMOVE OUTLIERS
      func_remove_outliers(bkpt, x) -> x_clean
      
      {
        # tibble(time = dates[1:length(x)],
        #        v = x_clean$x_clean,
        #        vv = x,
        #        top = x_clean$x_outliers[,1],
        #        bot = x_clean$x_outliers[,2]) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        # 
        #       geom_line(aes(y = top), linetype = "2222") +
        #       geom_line(aes(y = bot), linetype = "2222") +
        # 
        #       geom_point(aes(y = vv), size = 1.7, alpha = 0.5) +
        #       geom_point(aes(y = v), size = 1.7, color = "red") +
        # 
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
        #   }
      } # plot clean
      
      # SCALE TS
      {bkpt-12*40} %>% 
        {ifelse(.<0, 0, .)} -> lim1
      
      x_clean$x_clean[lim1:(bkpt+12*40)] %>% 
        range() -> range_sc
      
      x_clean$x_clean %>% 
        scales::rescale(from = range_sc) -> x_clean_sc
      
      
      {
        # tibble(time = dates,
        #        v = x_clean_sc) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        # 
        #       geom_point(aes(y = v), size = 1.7) +
        # 
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
        #       scale_y_continuous(breaks = seq(0,1,0.2))
        #   }
      } # plot scaled
      
      
      # OBTAIN SEGMENTS (10 yrs)
      seg_1 <- (bkpt-10*12-6):(bkpt-1-6) %>% .[. > 0]
      seg_2 <- (bkpt+1+6):(bkpt+10*12+6)
      
      x_seg_1 <- x_clean_sc[seg_1]
      x_seg_2 <- x_clean_sc[seg_2]
      
      
      # CALCULATE METRICS
      
      # midpoint
      {
        diff(range(x_seg_1))/2+min(x_seg_1) -> rg1
        diff(range(x_seg_2))/2+min(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_midpt <- 0
        } else {
          diff_midpt <- abs(rg1-rg2)
        }
      }
      
      
      # median
      {
        median(x_seg_1) -> rg1
        median(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_median <- 0
        } else {
          diff_median <- abs(rg1-rg2)
        }
        
      }
      
      
      # mean
      {
        mean(x_seg_1) -> rg1
        mean(x_seg_2) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_mean <- 0
        } else {
          diff_mean <- abs(rg1-rg2)
        }
        
      }
      
      
      # OBTAIN SEGMENTS (15 yrs)
      seg_1 <- (bkpt-15*12-6):(bkpt-1-6) %>% .[. > 0]
      seg_2 <- (bkpt+1+6):(bkpt+15*12+6)
      
      x_seg_1 <- x_clean_sc[seg_1]
      x_seg_2 <- x_clean_sc[seg_2]
      
      
      # CALCULATE METRICS
      
      # range 1
      {
        diff(range(x_seg_1)) -> rg1
        diff(range(x_seg_2)) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_range_1 <- 0
        } else {
          diff_range_1 <- abs(rg1-rg2)
        }
      }
      
      
      # variance 1
      {
        var(x_seg_1)*10 -> rg1
        var(x_seg_2)*10 -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_var_1 <- 0
        } else {
          diff_var_1 <- abs(rg1-rg2)
        }
        
      }
      
    } else {
      
      diff_midpt <- 0
      diff_median <- 0
      diff_mean <- 0
      diff_range_1 <- 0
      diff_var_1 <- 0
      
    }
    
    
    # ************************
    
    # DETECT BREAKPOINT 2: VAR
    changepoint::cpt.var(x, minseglen = 12*5, class = F)[1] %>% unname() -> bkpt
    
    dates[bkpt] %>% 
      as.integer() -> time_bkpt_v
    
    if(bkpt > which(dates == "1980-01-01") & bkpt < which(dates == "2009-12-01")){
      
      # REMOVE OUTLIERS
      func_remove_outliers(bkpt, x) -> x_clean
      
      {
        # tibble(time = dates[1:length(x)],
        #        v = x_clean$x_clean,
        #        vv = x,
        #        top = x_clean$x_outliers[,1],
        #        bot = x_clean$x_outliers[,2]) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        #
        #       geom_line(aes(y = top), linetype = "2222") +
        #       geom_line(aes(y = bot), linetype = "2222") +
        #
        #       geom_point(aes(y = vv), size = 1.7, alpha = 0.5) +
        #       geom_point(aes(y = v), size = 1.7, color = "red") +
        #
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
        #   }
      } # plot clean
      
      # SCALE TS
      {bkpt-12*40} %>% 
        {ifelse(.<0, 0, .)} -> lim1
      
      x_clean$x_clean[lim1:(bkpt+12*40)] %>% 
        range() -> range_sc
      
      x_clean$x_clean %>% 
        scales::rescale(from = range_sc) -> x_clean_sc
      
      
      {
        # tibble(time = dates,
        #        v = x_clean_sc) %>%
        #   {
        #     ggplot(., aes(x = time)) +
        # 
        #       geom_point(aes(y = v), size = 1.7) +
        # 
        #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
        #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
        #       scale_y_continuous(breaks = seq(0,1,0.2))
        #   }
      } # plot scaled
      
      
      # OBTAIN SEGMENTS (15 yrs)
      seg_1 <- (bkpt-15*12-6):(bkpt-1-6) %>% .[. > 0]
      seg_2 <- (bkpt+1+6):(bkpt+15*12+6)
      
      x_seg_1 <- x_clean_sc[seg_1]
      x_seg_2 <- x_clean_sc[seg_2]
      
      
      # CALCULATE METRICS
      
      # range
      {
        diff(range(x_seg_1)) -> rg1
        diff(range(x_seg_2)) -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_range_2 <- 0
        } else {
          diff_range_2 <- abs(rg1-rg2)
        }
      }
      
      
      # variance
      {
        var(x_seg_1)*10 -> rg1
        var(x_seg_2)*10 -> rg2
        
        if(anyNA(c(rg1,rg2))){
          diff_var_2 <- 0
        } else {
          diff_var_2 <- abs(rg1-rg2)
        }
        
      }
      
      
    } else {
      
      diff_range_2 <- 0
      diff_var_2 <- 0
      
    }
    
    
    
    
  } else {
    
    time_bkpt_m <- NA
    diff_midpt <- NA
    diff_median <- NA
    diff_mean <- NA
    diff_range_1 <- NA
    diff_var_1 <- NA
    
    time_bkpt_v <- NA
    diff_range_2 <- NA
    diff_var_2 <- NA
    
  }
  
  c(time_bkpt_m = time_bkpt_m,
    diff_midpt = diff_midpt,
    diff_median = diff_median,
    diff_mean = diff_mean,
    diff_range_1 = diff_range_1,
    diff_var_1 = diff_var_1,
    
    time_bkpt_v = time_bkpt_v,
    diff_range_2 = diff_range_2,
    diff_var_2 = diff_var_2
    
    )
  
}


# *****************






# *****************

func_jump_4 <- function(x, dates){
  
  # dates <- d
  
  count_na <- sum(is.na(x))
  
  if(count_na == length(x)){
    
    time_bkpt_trend = time_bkpt_trend <- NA
    seg_diff_midp = seg_diff_midp <- NA
    seg_diff_medn = seg_diff_medn <- NA
    seg_diff_mean = seg_diff_mean <- NA
    seg_diff_rnge = seg_diff_rnge <- NA
    seg_diff_vari = seg_diff_vari <- NA
    seg_diff_iqrr = seg_diff_iqrr <- NA
    seg_diff_qn19 = seg_diff_qn19 <- NA
    time_bkpt_var = time_bkpt_var <- NA
    seg_diff_rnge_2 = seg_diff_rnge_2 <- NA
    seg_diff_vari_2 = seg_diff_vari_2 <- NA
    seg_diff_iqrr_2 = seg_diff_iqrr_2 <- NA
    seg_diff_qn19_2 = seg_diff_qn19_2 <- NA
    
    
  } else {
    
    if(count_na > 0){
      
      na_interpolation(x) -> x
      
    }
    
    
    # TREND ****************************
    
    
    # PT 1: BREAKPOINT DETECTION
    
    x %>% 
      ts(start = first(dates) %>% {c(year(.), month(.))}, frequency = 12) %>% 
      stl("periodic", t.window = 12*10) -> x_stl
    
    x_stl %>% 
      .$time.series %>% 
      .[, "trend"] %>% 
      as.vector() -> x_trend
    
    x_trend[1:(which(dates == "2019-12-01"))] -> x_trend_cut
    
    rpart::rpart(x_trend_cut ~ seq_along(x_trend_cut), maxdepth = 1)$where %>% 
      unname() %>% 
      diff() %>% 
      {which(. != 0)} -> bkpt
    
    
    # tibble(
    #   v = x_trend,
    #   time = dates
    # ) %>%
    #   ggplot(aes(x = time, y = v)) +
    #   geom_line() +
    #   geom_vline(xintercept = dates[bkpt], alpha = 0.8, linetype = "2222")
    
    
    dates[bkpt] %>% 
      as.integer() -> time_bkpt_trend
    
    
    if(bkpt == length(x_trend_cut) | bkpt < which(dates == "1980-01-01")){
      seg_diff_midp <- 0
      seg_diff_medn <- 0
      seg_diff_mean <- 0
      seg_diff_rnge <- 0
      seg_diff_vari <- 0
      seg_diff_iqrr <- 0
      seg_diff_qn19 <- 0
      
    } else {
      
      # REMOVE OUTLIERS
      func_remove_outliers() -> x_clean
      
      
      # tibble(time = dates[1:length(x)],
      #        v = x_clean$x_clean,
      #        vv = x,
      #        top = x_clean$x_outliers[,1],
      #        bot = x_clean$x_outliers[,2]) %>%
      #   {
      #     ggplot(., aes(x = time)) +
      # 
      #       geom_line(aes(y = top), linetype = "2222") +
      #       geom_line(aes(y = bot), linetype = "2222") +
      # 
      #       geom_point(aes(y = vv), size = 1.7, alpha = 0.5) +
      #       geom_point(aes(y = v), size = 1.7, color = "red") +
      # 
      #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
      #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year"))
      #   }

      
      # SCALE TS (range taken from 20 yrs to the left and right of bkpt)
      {bkpt-12*20} %>% 
        {ifelse(.<0, 0, .)} -> lim1
      
      x_clean$x_clean[lim1:(bkpt+12*20)] %>% 
        range() -> range_sc
      
      x_clean$x_clean %>% 
        scales::rescale(from = range_sc) -> x_clean_sc
      
      # tibble(time = dates[1:length(x)],
      #        v = x_clean_sc) %>%
      #   {
      #     ggplot(., aes(x = time)) +
      # 
      #       geom_point(aes(y = v), size = 1.7) +
      # 
      #       scale_x_date(breaks = seq(min(.$time), max(.$time), by = "5 years"), date_labels = "%Y",
      #                    minor_breaks = seq(min(.$time), max(.$time), by = "1 year")) +
      #       scale_y_continuous(breaks = seq(0,1,0.2))
      #   }

      
      # SEGMENTS
      range1 <- (bkpt-10*12-6):(bkpt-1-6) %>% .[. > 0]
      range2 <- (bkpt+1+6):(bkpt+10*12+6)
      
      x_clean_sc[range1] %>%
        # sort() %>% 
        # {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
        # {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
        # {x[range1][x[range1] > .[1] & x[range1] < .[2]]} %>% 
        {.} -> rg1_rg
      
      x_clean_sc[range2] %>%
        # sort() %>% 
        # {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
        # {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
        # {x[range2][x[range2] > .[1] & x[range2] < .[2]]} %>% 
        {.} -> rg2_rg
      
      
      # PT 2: MEASURE DIFF IN MIDPOINT
      rg1_rg %>%
        boxplot.stats() %>% 
        .$out %>% 
        {rg1_rg[!rg1_rg %in% .]} %>% 
        range() -> rg1_pre
      
      {(rg1_pre[2]-rg1_pre[1])/2+rg1_pre[1]} -> rg1
      
      rg2_rg %>%
        boxplot.stats() %>% 
        .$out %>% 
        {rg2_rg[!rg2_rg %in% .]} %>% 
        range() -> rg2_pre
      
      {(rg2_pre[2]-rg2_pre[1])/2+rg2_pre[1]} -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_midp <- 0
      } else {
        # seg_diff_midp <- abs(rg1-rg2)/ts_diff
        seg_diff_midp <- abs(rg1-rg2)
      }
      
      
      # PT 3: MEASURE DIFF IN MEDIAN
      rg1_rg %>%
        median() -> rg1
      
      rg2_rg %>%
        median() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_medn <- 0
      } else {
        # seg_diff_medn <- abs(rg1-rg2)/ts_diff
        seg_diff_medn <- abs(rg1-rg2)
      } 
      
      
      # PT 4: MEASURE DIFF IN MEAN
      rg1_rg %>%
        mean() -> rg1
      
      rg2_rg %>%
        mean() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_mean <- 0
      } else {
        # seg_diff_mean <- abs(rg1-rg2)/ts_diff
        seg_diff_mean <- abs(rg1-rg2)
      } 
      
      
      # PT 5: MEASURE DIFF IN RANGE
      
      rg1_pre %>% 
        diff() -> rg1
      
      rg2_pre %>%
        diff() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_rnge <- 0
      } else {
        # seg_diff_sprd <- abs(rg1/ts_diff - rg2/ts_diff)
        seg_diff_rnge <- abs(rg1 - rg2)
      }
      
      
      # PT 6: MEASURE DIFF IN VARIANCE
      
      rg1_rg %>%
        sd() -> rg1
      
      rg2_rg %>% 
        sd() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_vari <- 0
      } else {
        seg_diff_vari <- abs(rg1 - rg2)
      }
      
      
      # PT 6: MEASURE DIFF IN IQR
      
      rg1_rg %>%
        IQR() -> rg1
      
      rg2_rg %>%
        IQR() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_iqrr <- 0
      } else {
        seg_diff_iqrr <- abs(rg1 - rg2)
      }
      
      
      # PT 7: MEASURE DIFF IN 10-90th perc
      
      rg1_rg %>%
        quantile(c(0.1,0.9)) %>% 
        unname() %>% 
        diff() -> rg1
      
      rg2_rg %>%
        quantile(c(0.1,0.9)) %>% 
        unname() %>% 
        diff() -> rg2
      
      if(anyNA(c(rg1,rg2))){
        seg_diff_qn19 <- 0
      } else {
        seg_diff_qn19 <- abs(rg1 - rg2)
      }
      
    }
    
    
    # VARIANCE ************************
    
    changepoint::cpt.var(x, class = F, minseglen = 12*5)[1] -> bkpt_v
    
    if(bkpt_v > which(dates == "2030-01-01")){
      
      x_stl %>% 
        .$time.series %>% 
        {.[, "seasonal"] + .[, "remainder"]} %>% 
        as.vector() -> x_detrend
      
      w <- 15*12+1
      rollapply(x_detrend,
                     FUN = function(y){
                       
                       sort(y) -> y
                       c(y[1/10*w], y[1/2*w], y[9/10*w]) -> q
                       
                       q[1]-(q[2]-q[1])*0.5 -> q1
                       q[3]+(q[3]-q[2])*0.5 -> q2
                       abs(q1 - q2)
                       
                     },
                     width = w,
                     by = 24,
                     fill = NA,
                     align = "center"
      ) %>% 
        na_interpolation() -> x_var
      
      x_var[1:(which(dates == "2019-12-01"))] -> x_var_cut
      
      rpart::rpart(x_var_cut ~ seq_along(x_var_cut), maxdepth = 1)$where %>% 
        unname() %>% 
        diff() %>% 
        {which(. != 0)} -> bkpt_v
      
    }
    
    
    if(abs(bkpt_v - bkpt) < 12*5){
      time_bkpt_var <- time_bkpt_trend
      seg_diff_rnge_2 <- seg_diff_rnge
      seg_diff_vari_2 <- seg_diff_vari
      seg_diff_iqrr_2 <- seg_diff_iqrr
      seg_diff_qn19_2 <- seg_diff_qn19
      
    } else {
      
      bkpt <- bkpt_v
      
      dates[bkpt] %>% 
        as.integer() -> time_bkpt_var
      
      if(bkpt > which(dates == "2019-12-01") | bkpt < which(dates == "1980-01-01")){
        seg_diff_rnge_2 <- 0
        seg_diff_vari_2 <- 0
        seg_diff_iqrr_2 <- 0
        seg_diff_qn19_2 <- 0
        
      } else {
        
        # REMOVE OUTLIERS
        func_remove_outliers()
        
        # SCALE TS
        {bkpt-12*20} %>% 
          {ifelse(.<0, 0, .)} -> lim1
        
        x_clean[lim1:(bkpt+12*20)] %>% 
          range() -> range_sc
        
        x_clean %>% 
          scales::rescale(from = range_sc) -> x_clean_sc
        
        
        range1 <- (bkpt-15*12-6):(bkpt-1-6) %>% .[. > 0]
        range2 <- (bkpt+1+6):(bkpt+10*15+6)
        
        x_clean_sc[range1] %>%
          # sort() %>% 
          # {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
          # {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
          # {x[range1][x[range1] > .[1] & x[range1] < .[2]]} %>% 
          {.} -> rg1_rg
        
        x_clean_sc[range2] %>%
          # sort() %>% 
          # {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
          # {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
          # {x[range2][x[range2] > .[1] & x[range2] < .[2]]} %>% 
          {.} -> rg2_rg
        
        
        # PT 5: MEASURE DIFF IN RANGE
        
        rg1_rg %>%
          boxplot.stats() %>% 
          .$out %>% 
          {rg1_rg[!rg1_rg %in% .]} %>% 
          range() %>% 
          diff() -> rg1
        
        rg2_rg %>%
          boxplot.stats() %>% 
          .$out %>% 
          {rg2_rg[!rg2_rg %in% .]} %>%
          range() %>% 
          diff() -> rg2
        
        if(anyNA(c(rg1,rg2))){
          seg_diff_rnge_2 <- 0
        } else {
          # seg_diff_sprd <- abs(rg1/ts_diff - rg2/ts_diff)
          seg_diff_rnge_2 <- abs(rg1 - rg2)
        }
        
        
        # PT 6: MEASURE DIFF IN VARIANCE
        
        rg1_rg %>%
          sd() -> rg1
        
        rg2_rg %>%
          sd() -> rg2
        
        if(anyNA(c(rg1,rg2))){
          seg_diff_vari_2 <- 0
        } else {
          seg_diff_vari_2 <- abs(rg1 - rg2)
        }
        
        
        # PT 6: MEASURE DIFF IN IQR
        
        rg1_rg %>%
          IQR() -> rg1
        
        rg2_rg %>%
          IQR() -> rg2
        
        if(anyNA(c(rg1,rg2))){
          seg_diff_iqrr_2 <- 0
        } else {
          seg_diff_iqrr_2 <- abs(rg1 - rg2)
        }
        
        
        # PT 7: MEASURE DIFF IN 10-90th perc
        
        rg1_rg %>%
          quantile(c(0.1,0.9)) %>% 
          unname() %>% 
          diff() -> rg1
        
        rg2_rg %>%
          quantile(c(0.1,0.9)) %>% 
          unname() %>% 
          diff() -> rg2
        
        if(anyNA(c(rg1,rg2))){
          seg_diff_qn19_2 <- 0
        } else {
          seg_diff_qn19_2 <- abs(rg1 - rg2)
        }
        
      }
      
    }
    
  }
  
  c(time_bkpt_trend = time_bkpt_trend,
    
    seg_diff_midp = seg_diff_midp,
    seg_diff_medn = seg_diff_medn,
    seg_diff_mean = seg_diff_mean,
    
    seg_diff_rnge = seg_diff_rnge,
    seg_diff_vari = seg_diff_vari,
    seg_diff_iqrr = seg_diff_iqrr,
    seg_diff_qn19 = seg_diff_qn19,
    
    time_bkpt_var = time_bkpt_var,
    
    seg_diff_rnge_2 = seg_diff_rnge_2,
    seg_diff_vari_2 = seg_diff_vari_2,
    seg_diff_iqrr_2 = seg_diff_iqrr_2,
    seg_diff_qn19_2 = seg_diff_qn19_2,
    
    count_na = count_na)
  
}


# ***********************************************




















# func_jump_1 <- function(x){
#   
#   count_na <- sum(is.na(x))
#   
#   if(count_na == length(x)){
#     
#     mean_seg_diff <- NA
#     mean_time_bkpt <- NA
#     sprd_seg_diff <- NA
#     sprd_time_bkpt <- NA
#     
#   } else {
#     
#     if(count_na > 0){
#       
#       imputeTS::na_interpolation(x) -> x
#       
#     }
#     
#     # CHANGE IN MEAN
#     
#     changepoint::cpt.mean(x, class = F, minseglen = 12*10)[1] -> bkpt
#     
#     if(bkpt == length(x)){
#       mean_seg_diff <- 0
#       mean_time_bkpt <- NA
#       
#     } else {
#       
#       dates[bkpt] %>%
#         as.integer() -> mean_time_bkpt
#       
#       scales::rescale(x, from = quantile(x, c(0.01, 0.99))) %>%
#         {case_when(. > 1 ~ 1,
#                    . < 0 ~ 0,
#                    TRUE ~ .)} -> x_scaled
#       
#       range1 <- (bkpt-10*12):(bkpt-1) %>% .[. > 0]
#       range2 <- (bkpt+1):(bkpt+10*12) %>% .[. <= 1560]
#       
#       rg1 <- x_scaled[range1] %>% na.omit() %>% mean(trim = 0.1)
#       rg2 <- x_scaled[range2] %>% na.omit() %>% mean(trim = 0.1)
#       
#       mean_seg_diff <- abs(rg1 - rg2)
#       
#     }
#     
#     
#     # CHANGE IN SPREAD
#     
#     x_cut <- x[1:(12*60)]
#     changepoint::cpt.var(x_cut, class = F, minseglen = 10*12)[1] -> bkpt
#     
#     if(bkpt == length(x_cut)){
#       sprd_seg_diff <- 0
#       sprd_time_bkpt <- NA
#       
#     } else {
#       
#       dates[bkpt] %>%
#         as.integer() -> sprd_time_bkpt
#       
#       range1 <- (bkpt-15*12-1):(bkpt-1) %>% .[. > 0]
#       range2 <- (bkpt+1):(bkpt+15*12+1) %>% .[. <= 1560]
#       
#       rg1 <- x[range1] %>% na.omit() %>% {boxplot.stats(., coef = 0.95)$stats} %>% {c(first(.), last(.))} %>% diff()
#       rg2 <- x[range2] %>% na.omit() %>% {boxplot.stats(., coef = 0.95)$stats} %>% {c(first(.), last(.))} %>% diff()
#       
#       if(rg1 > rg2){
#         sprd_seg_diff <- 1-(rg2/rg1)
#         
#       } else {
#         sprd_seg_diff <- 1-(rg1/rg2)
#         
#       }
#       
#     }
#     
#   }
#   
#   c(mean_seg_diff = mean_seg_diff,
#     mean_time_bkpt = mean_time_bkpt,
#     sprd_seg_diff = sprd_seg_diff,
#     sprd_time_bkpt = sprd_time_bkpt,
#     count_na = count_na)
#   
# }


# ***********************************************

# func_jump_2 <- function(x, dates){
#   
#   count_na <- sum(is.na(x))
#   
#   if(count_na == length(x)){
#     
#     mean_seg_diff <- NA
#     mean_time_bkpt <- NA
#     sprd_seg_diff <- NA
#     sprd_time_bkpt <- NA
#     
#   } else {
#     
#     if(count_na > 0){
#       
#       imputeTS::na_interpolation(x) -> x
#       
#     }
#     
#     # remove outliers
#     # w <- 10*12+1
#     # zoo::rollapply(x,
#     #                FUN = function(y){
#     # 
#     #                  sort(y) -> y
#     #                  c(y[0.25*w], y[0.5*w], y[0.75*w]) -> q
#     # 
#     #                  q[1]-(q[2]-q[1])*2.5 -> q1
#     #                  q[3]+(q[3]-q[2])*2.5 -> q2
#     #                  c(q1,q2)
#     # 
#     #                },
#     #                width = w,
#     #                by = 12,
#     #                fill = NA,
#     #                align = "center"
#     # ) -> x_outliers
#     # 
#     # cbind(imputeTS::na_interpolation(x_outliers[,1]),
#     #       imputeTS::na_interpolation(x_outliers[,2])) -> x_outliers
#     # 
#     # x_clean <- x
#     # x_clean[x < x_outliers[,1] | x > x_outliers[,2]] <- NA
#     # imputeTS::na_interpolation(x_clean) -> x_clean
#     
#     
#     # CHANGE IN MEAN
#     
#     # changepoint::cpt.mean(x_clean, class = F, minseglen = 5*12)[1] -> bkpt
#     
#     x %>% 
#       ts(start = 1970, frequency = 12) %>% 
#       stl("periodic", t.window = 5*12) %>%
#       .$time.series %>% 
#       .[,"trend"] %>% 
#       as.vector() -> x_trend
#     
#     changepoint::cpt.mean(scales::rescale(x_trend), class = F, minseglen = 5*12)[1] -> bkpt
#     
#     if(bkpt > 12*50){
#       mean_seg_diff <- 0
#       mean_time_bkpt <- NA
#       
#     } else {
#       
#       dates[bkpt] %>% 
#         as.integer() -> mean_time_bkpt
#       
#       # scales::rescale(x_clean) -> x_scaled
#       
#       range1 <- (bkpt-10*12):(bkpt-1) %>% .[. > 0]
#       range2 <- (bkpt+1):(bkpt+10*12)
#       
#       # rg1 <- x_scaled[range1] %>% mean(trim = 0.1)
#       # rg2 <- x_scaled[range2] %>% mean(trim = 0.1)
#       
#       rg1 <- x[range1] %>%
#         boxplot.stats(coef = 1.25) %>% 
#         .$out %>% 
#         {x[range1][!x[range1] %in% .]}
#       
#       rg1m <- mean(rg1)
#       
#       rg2 <- x[range2] %>%
#         boxplot.stats(coef = 1.25) %>% 
#         .$out %>% 
#         {x[range2][!x[range2] %in% .]}
#       
#       rg2m <- mean(rg2)
#       
#       # mean_seg_diff <- abs(rg1 - rg2)
#       
#       if(rg1m > rg2m){
#         mean_seg_diff <- (rg1m-rg2m)/diff(range(c(rg1,rg2)))
#       } else {
#         mean_seg_diff <- (rg2m-rg1m)/diff(range(c(rg1,rg2)))
#       }
#       
#     }
#     
#     
#     # CHANGE IN SPREAD
#     
#     # changepoint::cpt.var(x_clean, class = F, minseglen = 10*12)[1] -> bkpt
#     changepoint::cpt.var(x, class = F, minseglen = 5*12)[1] -> bkpt
#     
#     if(bkpt > 12*50){
#       sprd_seg_diff <- 0
#       sprd_time_bkpt <- NA
#       
#     } else {
#       
#       dates[bkpt] %>% 
#         as.integer() -> sprd_time_bkpt
#       
#       range1 <- (bkpt-10*12-1):(bkpt-1) %>% .[. > 0]
#       range2 <- (bkpt+1):(bkpt+10*12+1)
#       
#       # rg1 <- x_clean[range1] %>% boxplot.stats(coef = 1) %>% .$stats %>% {c(first(.), last(.))} %>% diff()
#       # rg2 <- x_clean[range2] %>% boxplot.stats(coef = 1) %>% .$stats %>% {c(first(.), last(.))} %>% diff()
#       
#       rg1 <- x[range1] %>%
#         sort() %>% 
#         {c(.[length(.)*1/4], .[length(.)*2/4], .[length(.)*3/4])} %>% 
#         {c((.[1]-(.[2]-.[1])*1.5), (.[3]+(.[3]-.[2])*1.5))} %>% 
#         {x[range1][x[range1] > .[1] & x[range1] < .[2]]} %>% 
#         range() %>% 
#         diff()
#       
#       rg2 <- x[range2] %>%
#         sort() %>% 
#         {c(.[length(.)*1/4], .[length(.)*2/4], .[length(.)*3/4])} %>% 
#         {c((.[1]-(.[2]-.[1])*1.5), (.[3]+(.[3]-.[2])*1.5))} %>% 
#         {x[range2][x[range2] > .[1] & x[range2] < .[2]]} %>% 
#         range() %>% 
#         diff()
#       
#       
#       
#       if(rg1 > rg2){
#         sprd_seg_diff <- 1-(rg2/rg1)
#         
#       } else {
#         sprd_seg_diff <- 1-(rg1/rg2)
#         
#       }
#       
#     }
#     
#   }
#   
#   c(mean_seg_diff = mean_seg_diff,
#     mean_time_bkpt = mean_time_bkpt,
#     sprd_seg_diff = sprd_seg_diff,
#     sprd_time_bkpt = sprd_time_bkpt,
#     count_na = count_na)
#   
#   
# }


# ***********************************************

# func_jump_3 <- function(x, dates){
#   
#   count_na <- sum(is.na(x))
#   
#   if(count_na == length(x)){
#     
#     trend_time_bkpt <- NA
#     midp_seg_diff <- NA
#     mean_seg_diff <- NA
#     sprd_seg_diff <- NA
#     
#     sprd2_time_bkpt <- NA
#     sprd2_seg_diff <- NA
#     
#   } else {
#     
#     if(count_na > 0){
#       
#       imputeTS::na_interpolation(x) -> x
#       
#     }
#     
#     
#     # TREND ****************************
#     
#     
#     # PT 1: BREAKPOINT DETECTION
#     
#     x %>% 
#       ts(start = first(dates) %>% {c(year(.), month(.))}, frequency = 12) %>% 
#       stl("periodic", t.window = 12*10) -> x_stl
#     
#     x_stl %>% 
#       .$time.series %>% 
#       .[, "trend"] %>% 
#       as.vector() -> x_trend
#     
#     x_trend[1:(which(dates == "2019-12-01"))] -> x_trend_cut
#     
#     rpart::rpart(x_trend_cut ~ seq_along(x_trend_cut), maxdepth = 1)$where %>% 
#       unname() %>% 
#       diff() %>% 
#       {which(. != 0)} -> bkpt
#     
#     if(bkpt == length(x_trend_cut)){
#       trend_time_bkpt <- NA
#       midp_seg_diff <- 0
#       mean_seg_diff <- 0
#       sprd_seg_diff <- 0
#       
#     } else {
#       
#       dates[bkpt] %>% 
#         as.integer() -> trend_time_bkpt
#       
#       
#       # PT 2: MEASURE DIFF IN MIDPOINT
#       
#       range1 <- (bkpt-10*12-6):(bkpt-1-6) %>% .[. > 0]
#       range2 <- (bkpt+1+6):(bkpt+10*12+6)
#       
#       x[range1] %>%
#         sort() %>% 
#         {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#         {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
#         {x[range1][x[range1] > .[1] & x[range1] < .[2]]} -> rg1_rg
#       
#       rg1_rg %>%   
#         range() %>% 
#         {(.[2]-.[1])/2+.[1]} -> rg1
#       
#       x[range2] %>%
#         sort() %>% 
#         {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#         {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
#         {x[range2][x[range2] > .[1] & x[range2] < .[2]]} -> rg2_rg
#       
#       rg2_rg %>%
#         range() %>% 
#         {(.[2]-.[1])/2+.[1]} -> rg2
#       
#       x %>% 
#         sort() %>% 
#         {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#         {c((.[1]-(.[2]-.[1])*0.95), (.[3]+(.[3]-.[2])*0.95))} %>%
#         {x[x > .[1] & x < .[2]]} %>% 
#         range() %>% 
#         diff() -> ts_diff
#       
#       if(anyNA(c(rg1,rg2))){
#         midp_seg_diff <- 0
#       } else if(rg1 > rg2){
#         midp_seg_diff <- (rg1-rg2)/ts_diff
#       } else {
#         midp_seg_diff <- (rg2-rg1)/ts_diff
#       }
#       
#       
#       # PT 3: MEASURE DIFF IN MEAN
#       rg1_rg %>%
#         median() -> rg1
#       
#       rg2_rg %>%
#         median() -> rg2
#       
#       if(anyNA(c(rg1,rg2))){
#         mean_seg_diff <- 0
#       } else if(rg1 > rg2){
#         mean_seg_diff <- (rg1-rg2)/ts_diff
#       } else {
#         mean_seg_diff <- (rg2-rg1)/ts_diff
#       }
#       
#       
#       # PT 3: MEASURE DIFF IN SPREAD
#       
#       rg1_rg %>%
#         range() %>% 
#         diff() -> rg1
#       
#       rg2_rg %>%
#         range() %>% 
#         diff() -> rg2
#       
#       if(anyNA(c(rg1,rg2))){
#         sprd_seg_diff <- 0
#         
#       } else if(rg1 > rg2){
#         # sprd_seg_diff <- 1-(rg2/rg1)
#         sprd_seg_diff <- rg1/ts_diff - rg2/ts_diff
#         
#       } else {
#         # sprd_seg_diff <- 1-(rg1/rg2)
#         sprd_seg_diff <- rg2/ts_diff - rg1/ts_diff
#         
#       }
#       
#     }
#     
#     
#     # SPREAD 2 ************************
#     
#     changepoint::cpt.var(x, class = F, minseglen = 12*5)[1] -> bkpt
#     
#     if(bkpt > which(dates == "2019-12-01")){
#       sprd2_seg_diff <- 0
#       sprd2_time_bkpt <- NA
#       
#     } else {
#       
#       dates[bkpt] %>% 
#         as.integer() -> sprd2_time_bkpt
#       
#       range1 <- (bkpt-15*12-6):(bkpt-1-6) %>% .[. > 0]
#       range2 <- (bkpt+1+6):(bkpt+10*15+6)
#       
#       x[range1] %>%
#         sort() %>% 
#         {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#         {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
#         {x[range1][x[range1] > .[1] & x[range1] < .[2]]} %>% 
#         range() -> rg1_rg
#       
#       rg1_rg %>%
#         diff() -> rg1
#       
#       x[range2] %>%
#         sort() %>% 
#         {c(.[length(.)*1/10], .[length(.)*1/2], .[length(.)*9/10])} %>% 
#         {c((.[1]-(.[2]-.[1])*0.15), (.[3]+(.[3]-.[2])*0.15))} %>% 
#         {x[range2][x[range2] > .[1] & x[range2] < .[2]]} %>% 
#         range() -> rg2_rg
#       
#       rg2_rg %>%
#         diff() -> rg2
#       
#       if(rg1 > rg2){
#         # sprd2_seg_diff <- 1-(rg2/rg1)
#         sprd2_seg_diff <- rg1/ts_diff - rg2/ts_diff
#         
#       } else {
#         # sprd2_seg_diff <- 1-(rg1/rg2)
#         sprd2_seg_diff <- rg2/ts_diff - rg1/ts_diff
#       }
#       
#     }
#     
#   }
#   
#   c(trend_time_bkpt = trend_time_bkpt,
#     midp_seg_diff = midp_seg_diff,
#     mean_seg_diff = mean_seg_diff,
#     sprd_seg_diff = sprd_seg_diff,
#     
#     sprd2_time_bkpt = sprd2_time_bkpt,
#     sprd2_seg_diff = sprd2_seg_diff,
#     count_na = count_na)
#   
# }


# *******************************


