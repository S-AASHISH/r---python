#install.packages("tidyr")
#library(xlsx)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyselect)
library(forcats)
library(viridis)
library(hydroGOF)
gcm=c("ACCESS") #new GCM can be added
m2=vector("list",length(gcm))
#k in 1:length(gcm)
obs=read_excel("/Users/s.aashish/Documents/Project/Paper/GCM/hist_obs.xlsx")   #Location of historical observed data
for (k in 1:length(gcm)){
  loc=paste0("/Users/s.aashish/Documents/Project/Paper/GCM/",gcm[k])     #Give the location of GCM folder
  #obs <- read_excel(paste0(loc,"/hist_obs.xlsx"))
  mod <- read_excel(paste0(loc,"/historical_GCM.xlsx"))
  modssp245 <- read_excel(paste0(loc,"/ssp245_raw.xlsx"))
  modssp585 <- read_excel(paste0(loc,"/ssp585_raw.xlsx"))
  obs$Date=as.Date(obs$Date,format="%Y-%m-%d")
  mod$Date=as.Date(mod$Date,format="%Y-%m-%d")
  modssp245$Date=as.Date(modssp245$Date,format="%Y-%m-%d")
  modssp585$Date=as.Date(modssp585$Date,format="%Y-%m-%d")
  obs1=obs%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  mod1=mod%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  modssp2451=modssp245%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  modssp5851=modssp585%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  l=vector("list",12)    #creating empty list
  lm=vector("list",12) 
  lmssp245=vector("list",12) 
  lmssp585=vector("list",12) 
  for(i in 1:12){
    x1=obs1%>%filter(month==i)
    x2=mod1%>%filter(month==i)
    x3=modssp2451%>%filter(month==i)
    x4=modssp5851%>%filter(month==i)
    l[[i]]=dplyr::select(x1,-c(Date,year,month,day))
    lm[[i]]=dplyr::select(x2,-c(Date,year,month,day))
    lmssp245[[i]]=dplyr::select(x3,-c(Date,year,month,day))
    lmssp585[[i]]=dplyr::select(x4,-c(Date,year,month,day))
  }
  #View(lm[[2]])
  st_name=colnames(lm[[1]])
  lm_bias=vector("list",12)
  ssp245_bias=vector("list",12)
  ssp585_bias=vector("list",12)
  library(qmap)
  for(i in 1:length(lm)){      #bias correction method to be applied
    qm.fit <- fitQmapRQUANT(l[[i]],lm[[i]],
                            qstep=0.01,nboot=10,wet.day=TRUE)
    lm_bias[[i]] <- doQmapRQUANT(lm[[i]],qm.fit,type="linear")
    ssp245_bias[[i]] <- doQmapRQUANT(lmssp245[[i]],qm.fit,type="linear")
    ssp585_bias[[i]] <- doQmapRQUANT(lmssp585[[i]],qm.fit,type="linear")
    
  }
  #Creating list of future date
  lf_date=vector("list",12) 
  date2=data.frame(Date=seq(as.Date("2015-01-01"),as.Date("2100-12-31"),by="days"))   #future starting and ending year
  date2=date2%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  for(i in 1:12){
    lf_date[[i]]=date2%>%filter(month==i)
    
  }
  #column binding for arranging with respect to date
  ssp245=NULL
  ssp585=NULL
  for(i in 1:12){
    ssp245_bias[[i]]=cbind(ssp245_bias[[i]],lf_date[[i]])
    ssp585_bias[[i]]=cbind(ssp585_bias[[i]],lf_date[[i]])
    ssp245=rbind(ssp245,ssp245_bias[[i]])
    ssp585=rbind(ssp585,ssp585_bias[[i]])
  }
  #View(ssp245)
  ssp245=arrange(ssp245,year,month,day)
  ssp585=arrange(ssp585,year,month,day)
  ssp245=rapply(object=ssp245,f=round,classes="numeric",how="replace",digits=1)
  ssp585=rapply(object=ssp585,f=round,classes="numeric",how="replace",digits=1)
  #write.csv(ssp245,paste0(loc,"/ssp245_bias_corrected_2015-2100.csv"),row.names = F)
  #write.csv(ssp585,paste0(loc,"/ssp585_bias_corrected_2015-2100.csv"),row.names = F)
  write_xlsx(ssp245,paste0(loc,"/ssp245_bias_corrected_2015-2100.xlsx"))
  write_xlsx(ssp585,paste0(loc,"/ssp585_bias_corrected_2015-2100.xlsx"))
  l_date=vector("list",12) 
  date1=data.frame(Date=seq(as.Date("1975-01-01"),as.Date("2014-12-31"),by="days"))    #Historical starting and ending year
  date1=date1%>%mutate(year=as.numeric(format(Date,"%Y")),month=as.numeric(format(Date,"%m")),day=mday(Date))
  for(i in 1:12){
    l_date[[i]]=date1%>%filter(month==i)
    
  }
  #View(l_date[[12]])
  
  for(i in 1:12){
    l[[i]]=cbind(l[[i]],l_date[[i]])
    lm[[i]]=cbind(lm[[i]],l_date[[i]])
    lm_bias[[i]]=cbind(lm_bias[[i]],l_date[[i]])
    
  }
  #View(lm_bias[[12]])
  mod_c=NULL
  for(i in 1:12){
    
    mod_c=rbind(mod_c,lm_bias[[i]])
    
  }
  mod_c=arrange(mod_c,year,month,day)   #Bias corrected Dataframe
  type=c("Observed","Raw","Corrected")
  #Doing above by creating list
  m1=vector("list",3)
  m1[[1]]=obs1
  m1[[2]]=mod1
  m1[[3]]=mod_c
  for(i in 1:length(m1)){
    m1[[i]]=m1[[i]] %>% group_by(year,month) %>% summarise_at(st_name,sum)
    m1[[i]]=m1[[i]] %>% group_by(month) %>% summarise_at(st_name,mean)
    m1[[i]]$type=type[i]
  }
  #View(m1[[3]])
  df_final=NULL
  for(i in 1:length(m1)){
    df_final=rbind(df_final,m1[[i]])
    
  }
  df_long=df_final %>% gather(station,P,all_of(st_name))
  df_long$GCM=rep(gcm[k],nrow(df_long))
  m2[[k]]=df_long
  
}
#View(m2[[1]])
df1=NULL
for (i in 1:length(gcm)){
  df1=rbind(df1,m2[[i]])
}
#View(df1)
ggplot(df1,aes(x=month,y=P,color=type,size=type))+geom_line()+xlim(3, 9)+ylim(0, 1000)+scale_x_discrete(limits=month.abb)+facet_grid(rows=vars(fct_recode(GCM, "ACCESS-CM2" = "access","EC-EARTH3"="earth","MIROC6"="miroc","MPI-ESM1-2-HR"="mpi","MRI-ESM2-0"="mri")),cols = vars(station),scales="free_y")+labs(title="Bias Correction Result For Precipitation (mm) ")+scale_size_manual(values=c(3,1,1))+xlab("Month")+theme_bw()+theme(plot.title = element_text(face="bold",hjust = 0.5) ,legend.title = element_blank(),legend.position = c(0.68,0.96),axis.title = element_text(face = "bold"),axis.text = element_text(face="bold"),legend.text = element_text(face="bold"),axis.title.y = element_blank(),legend.direction = "horizontal",strip.text = element_text(face="bold",size=11))+scale_color_viridis(discrete = TRUE,option = "plasma") 
ggsave("/Users/s.aashish/Documents/Project/GCM/PPT/bias_correction_result_graph.png",dev="png",dpi=1500,height = 8,width = 13,units = "in")   #Location to save the bias corrected image


#statistical indicators
model.assess <- function(Sim, Obs) { 
  NSE=round(NSE(sim =Sim,obs =Obs),2)
  PBIAS=round(pbias(sim =Sim,obs =Obs),2)
  PBIAS=sprintf('%.2f',PBIAS)
  R2=round((cor(Sim, Obs, method="pearson"))^2,2)
  return(rbind(NSE,PBIAS,R2))
}

for (i in 1:length(gcm)){
  for(j in 1:length(st_name)){
    z1=m2[[i]] %>% spread(type,P)
    z2=filter(z1,station==st_name[j])
    pre_cor=model.assess(z2$Raw,z2$Observed)
    post_cor=model.assess(z2$Corrected,z2$Observed)
    stat_daily=data.frame(cbind(pre_cor,post_cor))
    stat_daily$station=st_name[j]
    colnames(stat_daily)=c("Pre_Correction","Post_Correction","Station")
    stat_daily <- data.frame(Statistical_indicator = row.names(stat_daily),stat_daily)
    stat_daily$Pre_Correction=as.numeric(stat_daily$Pre_Correction)
    stat_daily$Post_Correction=as.numeric(stat_daily$Post_Correction)
    loc1=paste0("/Users/s.aashish/Documents/Project/Paper/GCM/",gcm[i],"\\")     #Give the location of GCM folder
    write_xlsx(stat_daily,paste0(loc1,st_name[j],"_stat_indicator.xlsx"))
  }
}