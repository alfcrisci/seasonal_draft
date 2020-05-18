library(xts)
library(lubridate)
library(raster)
library(ncdf4)

setwd("/home/salute/seasonal/procedure_data/WT_data/lists_WTs")

list_WT_pct09=readRDS("list_WT_pct09.rds")          
list_WT_san09=readRDS("list_WT_san09.rds") 

list_WTS=list(list_WT_pct09,list_WT_san09)
names(list_WTS)=c("WT_pct09","WT_san09")

setwd("/home/salute/seasonal/procedure_data/stack_eobs")


############################################################################################################


tx_daily=brick( "/home/salute/seasonal/last/eobs_v17/tx_0.25deg_reg_1981-2010_v17.0.nc",lvar=1)


j=2
  for (i in seq_along(list_WTS[[j]])) {

    days=as.numeric(unlist(list_WTS[[j]][i])) 
    
    if ( length(days) >1) {
   
    tx_temp_clim=subset(tx_daily,as.numeric(unlist(list_WTS[[j]][i])))
    
    writeRaster(tx_temp_clim,paste0("stacks_eobs_wt/tx_wt/all_tx_",names(list_WTS)[j],"_",names(list_WTS[[j]])[[i]]), "CDF", overwrite=TRUE,varname="tx",varunit="Celsius",longname="maximum temperature",xname="longitude",yname="latitude")
  }
}


rm(tx_daily)

############################################################################################################


tn_daily=brick( "/home/salute/seasonal/last/eobs_v17/tn_0.25deg_reg_1981-2010_v17.0.nc",lvar=1)

j=2
  for (i in seq_along(list_WTS[[j]])) {

    days=as.numeric(unlist(list_WTS[[j]][i])) 
    
    if ( length(days) >1) {
   
    tn_temp_clim=subset(tn_daily,as.numeric(unlist(list_WTS[[j]][i])))
    
    writeRaster(tn_temp_clim,paste0("stacks_eobs_wt/tn_wt/all_tn_",names(list_WTS)[j],"_",names(list_WTS[[j]])[[i]]), "CDF", overwrite=TRUE,varname="tn",varunit="Celsius",longname="minumum temperature",xname="longitude",yname="latitude")
  }
}


rm(tn_daily)

############################################################################################################

tg_daily=brick( "/home/salute/seasonal/last/eobs_v17/tg_0.25deg_reg_1981-2010_v17.0.nc",lvar=1)


j=2
  for (i in seq_along(list_WTS[[j]])) {

    days=as.numeric(unlist(list_WTS[[j]][i])) 
    
    if ( length(days) >1) {
   
    tg_temp_clim=subset(tg_daily,as.numeric(unlist(list_WTS[[j]][i])))
    
    writeRaster(tg_temp_clim,paste0("stacks_eobs_wt/tg_wt/all_tg_",names(list_WTS)[j],"_",names(list_WTS[[j]])[[i]]), "CDF", overwrite=TRUE,varname="tg",varunit="Celsius",longname="mean temperature",xname="longitude",yname="latitude")
  }
}


rm(tg_daily)

############################################################################################################



