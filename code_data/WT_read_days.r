library(raster)
library(ncdf4)
library(lubridate)
library(xts)
setwd("/home/alf/Scrivania/lav_check_seasonal/original_raw")

pct09=read.csv("pct09.cla",sep="",header=F)
san09=read.csv("san09_500HGT.cla",sep="",header=F)

WTS=data.frame(WT_pct09=as.factor(pct09$V5),
               WT_san09=as.factor(san09$V5[1:13545])
)

rownames(WTS)=ISOdate(pct09$V1,pct09$V2,pct09$V3)

saveRDS(WTS,"WTS_df.rds")

WTS_xts=as.xts(WTS)

saveRDS(WTS_xts,"WTS_xts.rds")

times_WTS=index(WTS_xts)

saveRDS(times_WTS,"times_WTS.rds")

months_WTS=month(times_WTS)

saveRDS(months_WTS,"months_WTS.rds")

list_month_f=list()

list_month_f[[1]]=table(months_WTS,WTS$WT_pct09)
list_month_f[[2]]=table(months_WTS,WTS$WT_san09)

write.csv(as.data.frame.array(list_month_f[[1]]),file="f_WT_pct09.csv")
write.csv(as.data.frame.array(list_month_f[[2]]),file="f_WT_san09.csv")


saveRDS(list_month_f,"list_month_f_WTS.rds")pct09=read.csv("pct09.cla",sep="",header=F)
san09=read.csv("san09_500HGT.cla",sep="",header=F)

WTS=data.frame(WT_pct09=as.factor(pct09$V5),
               WT_san09=as.factor(san09$V5[1:13545])
)

rownames(WTS)=ISOdate(pct09$V1,pct09$V2,pct09$V3)

saveRDS(WTS,"WTS_df.rds")

WTS_xts=as.xts(WTS)
WTS_clim=WTS[732:11688,]
WTS_xts_clim=as.xts(WTS_clim)

saveRDS(WTS_xts,"WTS_xts.rds")
saveRDS(WTS_xts_clim,"WTS_xts_clim.rds")

times_WTS=index(WTS_xts)
times_WTS_clim=index(WTS_xts_clim)

saveRDS(times_WTS,"times_WTS.rds")
saveRDS(times_WTS_clim,"times_WTS_clim.rds")

months_WTS=month(times_WTS)
months_WTS_clim=month(times_WTS_clim)

saveRDS(months_WTS,"months_WTS.rds")
saveRDS(months_WTS_clim,"months_WTS_clim.rds")

list_month_f=list()

list_month_f[[1]]=table(months_WTS,WTS$WT_pct09)
list_month_f[[2]]=table(months_WTS,WTS$WT_san09)

list_month_f_clim=list()

list_month_f_clim[[1]]=table(months_WTS_clim,WTS_clim$WT_pct09)
list_month_f_clim[[2]]=table(months_WTS_clim,WTS_clim$WT_san09)

write.csv(as.data.frame.array(list_month_f[[1]]),file="f_WT_pct09.csv")
write.csv(as.data.frame.array(list_month_f[[2]]),file="f_WT_san09.csv")

saveRDS(list_month_f_clim,"list_month_f_WTS_clim.rds")

saveRDS(list_month_f,"list_month_f_WTS.rds")


###########################################################################################
# list climatological 1981-2010

temp_pct9=data.frame(mon=months_WTS[732:11688],wt=WTS$WT_pct09[732:11688])
list_WT_pct09=split(temp_pct9,temp_pct9[c('mon','wt')])
list_WT_pct09=lapply(list_WT_pct09,function(x) rownames(x))
names(list_WT_pct09)<-gsub("\\.","_",paste0("ind_wt_",names(list_WT_pct09)))
saveRDS(list_WT_pct09,"list_WT_pct09.rds")

temp_san09=data.frame(mon=months_WTS[732:11688],wt=WTS$WT_san09[732:11688])
list_WT_san09=split(temp_san09,temp_san09[c('mon','wt')])
list_WT_san09=lapply(list_WT_san09,function(x) rownames(x))
names(list_WT_san09)<-gsub("\\.","_",paste0("ind_wt_",names(list_WT_san09)))
saveRDS(list_WT_san09,"list_WT_san09.rds")

###########################################################################################
# list recent full 1979-2016

temp=data.frame(mon=months_WTS,wt=WTS$WT_pct09)
list_WT_WT_pct09=split(temp,temp[c('mon','wt')])
list_WT_pct09=lapply(list_WT_WT_pct09,function(x) rownames(x))
names(list_WT_pct09)<-gsub("\\.","_",paste0("ind_wt_",names(list_WT_pct09)))
saveRDS(list_WT_pct09,"list_WT_pct09_full.rds")

temp=data.frame(mon=months_WTS,wt=WTS$WT_san09)
list_WT_WT_san09=split(temp,temp[c('mon','wt')])
list_WT_san09=lapply(list_WT_WT_san09,function(x) rownames(x))
names(list_WT_san09)<-gsub("\\.","_",paste0("ind_wt_",names(list_WT_san09)))
saveRDS(list_WT_san09,"list_WT_san09_full.rds")


