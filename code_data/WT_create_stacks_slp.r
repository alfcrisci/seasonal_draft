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


############################################################################################################

pp_daily=brick( "/home/salute/seasonal/last/eobs_v17/pp_0.25deg_reg_1981-2010_v17.0.nc",lvar=1)


for (j in 1:2) {
 for (i in seq_along(list_WTS[[j]])) {
    
   days=as.numeric(unlist(list_WTS[[j]][i])) 
    
    if ( length(days) >1) {
    
    pp_temp_clim=subset(pp_daily,as.numeric(unlist(list_WTS[[j]][i])))
    
    writeRaster(pp_temp_clim,paste0("stacks_eobs_wt/pp_wt/all_pp_",names(list_WTS)[j],"_",names(list_WTS[[j]])[[i]]), "CDF", overwrite=TRUE,varname="pp",varunit="hPa",longname="air_pressure_at_sea_level",xname="longitude",yname="latitude")
  }
}
}


rm(pp_daily)

############################################################################################################




