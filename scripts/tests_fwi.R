

library(cffdrs)
source("scripts/00_setup.R")
plan(multisession)


"~/bucket_risk/RCM_regridded_data/REMO2015/NAM/daily/average_temperature/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "MPI")] %>% 
  .[str_length(.) > 165] %>% 
  .[6:7] %>% 
  future_map(read_ncdf, ncsub = cbind(start = c(300,100,1),
                                      count = c(50,50,NA))) %>% 
  suppressMessages() %>% 
  do.call(c, .) -> s_tas

"~/bucket_risk/RCM_regridded_data/REMO2015/CAM/daily/precipitation/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "MPI")] %>% 
  .[str_length(.) > 165] %>% 
  .[6:7] %>% 
  future_map(read_ncdf, ncsub = cbind(start = c(300,100,1),
                                      count = c(50,50,NA))) %>%
  suppressMessages() %>% 
  do.call(c, .) -> s_precip

"~/bucket_risk/RCM_regridded_data/REMO2015/NAM/daily/surface_wind/" %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "MPI")] %>% 
  # .[str_length(.) > 160] %>% 
  .[2] %>% 
  read_ncdf(ncsub = cbind(start = c(300,100,1),
                          count = c(50,50,NA))) %>% 
  suppressMessages() -> s_wind







data(test_wDC)

# input <- with(test_wDC, test_wDC[order(id,yr,mon,day),])
input <- arrange(test_wDC, id, yr, mon, day)
a_fs <- fireSeason(input[input$id==1,])


data(test_fwi)
fwi(test_fwi, out = "fwi") %>% slice(10:20)
fwi(test_fwi, init=data.frame(ffmc=80,dmc=10,dc=10,lat=40), out = "fwi") %>% slice(10:20)
fwi(test_fwi, init=data.frame(ffmc=80,dmc=10,dc=10,lat=40), out = "fwi", batch = F) %>% slice(10:20)



data("test_wDC")
input <- test_wDC[test_wDC$id==1,]

# fwi_fs_wDC <- function(input){
  
all.fwi <- NULL
curYr.fwi <- NULL
  
  #Create date variable
input$date <- as.Date(as.POSIXlt(paste(input$yr, "-", input$mon, "-", input$day,sep="")))
  
#use default fire season start and end temperature thresholds
fs <- fireSeason(input)

#Fire season dates, ordered chronologically
# fs <- with(fs,fs[order(yr,mon,day),])
fs <- arrange(fs, yr, mon, day)

#Create same Date format as weather dataset for comparison
fs$date <- as.Date(as.POSIXlt(paste(fs$yr,"-",fs$mon,"-",fs$day,sep="")))
theyears <- unique(fs$yr)

for(curYr.row in 1:length(theyears)){
  # curYr.row = 1
  curYr <- theyears[curYr.row]
  curYr.d <- fs[fs$yr==curYr,] # select rows of year
  
  curYr.init <- data.frame(ffmc=80,dmc=10,dc=16) #set an initial startup values
  
  #if there is more than one year of data, accumulate precipitation, then calculate overwinterDC
  #and continue
  if(curYr.row > 1){
    #calculate the overwinter period
    #end of last year's fire season
    curYr.owd <- curYr.fsd[nrow(curYr.fsd),]
    #rbind with beginning of current year's fire season
    curYr.owd <- rbind(curYr.owd, curYr.d[1,])
    #accumulate precipitation for the period between end of last and start of current
    curYr.owdata <- sum(input[(input$date>curYr.owd[1,"date"] &
                                 input$date < curYr.owd[2,"date"]),]$prec)
    owDC <- wDC(DCf=tail(curYr.fwi$DC,n=1),rw=curYr.owdata) #calculate overwinter DC value
    curYr.init <- data.frame(ffmc=80,dmc=10,dc=owDC) #Initialize moisture codes
  }
  
  curYr.fsd <- curYr.d[c(1,nrow(curYr.d)),]#get first and last dates of this year
  
  #match input data to those dates for fire season data
  curYr.fsdata <- input[input$yr == curYr & input$date >= curYr.fsd[1,"date"] &
                          input$date <= curYr.fsd[2,"date"],]
  #run fwi on fireseason data
  curYr.fwi <- fwi(curYr.fsdata,init=curYr.init)
  #force column names to be uppercase for consistency
  names(curYr.fwi) <- toupper(names(curYr.fwi))
  all.fwi <- rbind(all.fwi,curYr.fwi)
}
all.fwi
# }


