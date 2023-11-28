#install.packages("ncdf4")
#install.packages("fields")
library(writexl)
library(readxl)
library(ncdf4)
library(fields)
stn_loc=read.csv("/Users/s.aashish/Documents/Project/Paper/GCM/Met_Station_list.csv")   # Location of your meteorological station csv file
#View(stn_loc)
gcm=c("ACCESS")  #can be expanded depending upon GCM to be used
#nrow(stn_loc)
#sapply(stn_loc,class)
#1:length(gcm
#for (k in 1:length(gcm))
for (k in 1:length(gcm)){
  df1=paste0("/Users/s.aashish/Documents/Project/Paper/GCM/",gcm[k])   #Location to extract the NC format GCM
  f1=nc_open(paste0(df1,"/historical.nc"))
  f2=nc_open(paste0(df1,"/ssp245.nc"))
  f3=nc_open(paste0(df1,"/ssp585.nc"))
      #Historical
  lat=ncvar_get(f1,"lat")
  #lat
  lon=ncvar_get(f1,"lon")
  #lon
  tm=ncvar_get(f1,"time")
  #tm
  tm=as.Date(tm,origin="1850-01-01",tz="UTC",format="%Y-%m-%d")
  #ts_303
  max_temp=data.frame(Date=seq(as.Date("1975-01-01"),as.Date("2014-12-31"),by="days")) #Historical starting and ending year
  max_temp$Date=as.Date(max_temp$Date,format="%Y-%m-%d")
  #View(tm)
  for(i in 1:nrow(stn_loc)){
    stn_lat=stn_loc[i,3]
    stn_lon=stn_loc[i,4]
    sq_diff_lat=(lat-stn_lat)**2
    
    lat_index=which.min(sq_diff_lat)
    sq_diff_lon=(lon-stn_lon)**2
    #lat
    lon_index=which.min(sq_diff_lon)
    lon_index
    #lon
    start_time_index=which(as.character(tm)=="1975-01-01")    #starting time
    #time_index
    end_time_index=which(as.character(tm)=="2014-12-31")     #ending time
    #end_time_index
    time_count=end_time_index-start_time_index+1
    time_count
    
    data=ncvar_get(f1,"pr",start = c(lon_index,lat_index,start_time_index),count=c(1,1,time_count))   #change needed
    data=data*1   #change to mm unit from kg/m2/s
    max_temp=cbind(max_temp,data)
    
  }
  colnames(max_temp)=c("Date",stn_loc$INDEX)
  max_temp=rapply(object=max_temp,f=round,classes="numeric",how="replace",digits=1)
  #View(max_temp)
  #write.csv(max_temp,paste0(df1,"/historical_GCM.csv"),row.names = F)
  write_xlsx(max_temp,paste0(df1,"/historical_GCM.xlsx"))    
  
  #for SSP245
  lat=ncvar_get(f2,"lat")
  #lat
  lon=ncvar_get(f2,"lon")
  #lon
  tm=ncvar_get(f2,"time")
  #tm
  tm=as.Date(tm,origin="1850-01-01",tz="UTC",format="%Y-%m-%d")
  #ts_303
  max_temp=data.frame(Date=seq(as.Date("2015-01-01"),as.Date("2100-12-31"),by="days"))   #start and end time of future data
  max_temp$Date=as.Date(max_temp$Date,format="%Y-%m-%d")
  #View(tm)
  for(i in 1:nrow(stn_loc)){
    stn_lat=stn_loc[i,3]
    stn_lon=stn_loc[i,4]
    sq_diff_lat=(lat-stn_lat)**2
    
    lat_index=which.min(sq_diff_lat)
    sq_diff_lon=(lon-stn_lon)**2
    #lat
    lon_index=which.min(sq_diff_lon)
    lon_index
    #lon
    start_time_index=which(as.character(tm)=="2015-01-01")  #start time for future data
    #time_index
    end_time_index=which(as.character(tm)=="2100-12-31")   ##end time for future data
    #end_time_index
    time_count=end_time_index-start_time_index+1
    time_count
    
    data=ncvar_get(f2,"pr",start = c(lon_index,lat_index,start_time_index),count=c(1,1,time_count))
    data=data
    max_temp=cbind(max_temp,data)
    
  }
  colnames(max_temp)=c("Date",stn_loc$INDEX)
  max_temp=rapply(object=max_temp,f=round,classes="numeric",how="replace",digits=1)
  #View(max_temp)
  #write.csv(max_temp,paste0(df1,"/ssp245_raw.csv"),row.names = F)
  write_xlsx(max_temp,paste0(df1,"/ssp245_raw.xlsx"))
  
  
  
  ##SSP585
  lat=ncvar_get(f3,"lat")
  #lat
  lon=ncvar_get(f3,"lon")
  #lon
  tm=ncvar_get(f3,"time")
  #tm
  tm=as.Date(tm,origin="1850-01-01",tz="UTC",format="%Y-%m-%d")
  #ts_303
  max_temp=data.frame(Date=seq(as.Date("2015-01-01"),as.Date("2100-12-31"),by="days"))
  max_temp$Date=as.Date(max_temp$Date,format="%Y-%m-%d")
  #View(tm)
  for(i in 1:nrow(stn_loc)){
    stn_lat=stn_loc[i,3]
    stn_lon=stn_loc[i,4]
    sq_diff_lat=(lat-stn_lat)**2
    
    lat_index=which.min(sq_diff_lat)
    sq_diff_lon=(lon-stn_lon)**2
    #lat
    lon_index=which.min(sq_diff_lon)
    lon_index
    #lon
    start_time_index=which(as.character(tm)=="2015-01-01")
    #time_index
    end_time_index=which(as.character(tm)=="2100-12-31")
    #end_time_index
    time_count=end_time_index-start_time_index+1
    time_count
    
    data=ncvar_get(f3,"pr",start = c(lon_index,lat_index,start_time_index),count=c(1,1,time_count))
    data=data*1
    max_temp=cbind(max_temp,data)
    
  }
  colnames(max_temp)=c("Date",stn_loc$INDEX)
  max_temp=rapply(object=max_temp,f=round,classes="numeric",how="replace",digits=1)
  #View(max_temp)
  #write.csv(max_temp,paste0(df1,"/ssp585_raw.csv"),row.names = F)
  write_xlsx(max_temp,paste0(df1,"/ssp585_raw.xlsx"))
}





