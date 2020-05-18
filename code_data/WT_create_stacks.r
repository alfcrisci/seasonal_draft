library(xts)
library(lubridate)
library(raster)
library(ncdf4)

############################################################################################################
# function create stacks of WTs

create_stacks=function(dataraw_dir="/home/salute/seasonal/last",
                                   dataout_dir="stacks_eobs_wt_v20",
                                   period_clim="1981-2010"
                                   period_out_clim="clim",
                                   eobs_version="v20",
                                   minimal_days=1,
                                   varname="rr",
                                   varunit="mm",
                                   var_longname="thickness_of_rainfall_amount",
                                   wt_index="pct09",
                                   listWTS) {
                                                 require(raster)
                                                 var_daily=brick(paste0( dataraw_dir,"/eobs_",eobs_version,"/",varname,"_ens_mean_0.1deg_reg_",period_clim,"_v",eobs_version,"e.nc"),lvar=1)
                                                  j=ifelse(wt_index="pct09",1,2)
                                                  for (i in seq_along(list_WTS[[j]])) 
                                                       {
                                                       days=as.numeric(unlist(list_WTS[[j]][i])) 
                                                       if ( length(days) >minimal_days) {
                                                                                                       var_temp=subset(var_daily,as.numeric(unlist(list_WTS[[j]][i])))
                                                                                                       writeRaster(var_temp,
                                                                                                                         paste0(dataout_dir,"/",varname,"_wt_clim/all_",varname,"_clim_",names(list_WTS)[j],"_",names(list_WTS[[j]])[[i]]), 
                                                                                                                         "CDF", 
                                                                                                                         overwrite=TRUE,
                                                                                                                         varname=varname,
                                                                                                                         varunit=varunit,
                                                                                                                         longname=var_longname,
                                                                                                                         xname="longitude",
                                                                                                                         yname="latitude")
                                                                                                      }
                                                     }
                                                    rm(var_daily)
                                                    }

############################################################################################################
# Setup directory where WT lists are available

setwd("/home/salute/seasonal/procedure_data/WT_data/lists_WTs")

###################
## Read 

list_WT_pct09_clim=readRDS("list_WT_pct09_clim_1981-2010_NEW.rds")  
        
list_WT_san09_clim=readRDS("list_WT_san09_clim_1981-2010_NEW.rds") 

list_WT_pct09_9818=readRDS("list_WT_pct09_1998-2018_NEW.rds")    
      
list_WT_san09_9818=readRDS("list_WT_san09_1998-2018_NEW.rds") 

####################
# Compose and naming 

list_WTS_clim=list(list_WT_pct09_clim,list_WT_san09_clim)
names(list_WTS_clim)=c("WT_pct09_clim","WT_san09_clim")

list_WTS_9818=list(list_WT_pct09_9818,list_WT_san09_9818)
names(list_WTS_9818)=c("WT_pct09_9818","WT_san09_9818")

############################################################################################################
# Setup directory where data are available

setwd("/home/salute/seasonal/procedure_data/stack_eobs")

#################################################################################################
# General context

#################################################################################################

dataraw_dir="/home/salute/seasonal/last"
eobs_version="v20"
dataout_dir=paste0("stacks_eobs_wt_",eobs_version)
minimal_days=1 # relative to stack numerosity

#################################################################################################
# Cumulated rainfall
#################################################################################################

create_stacks( period_clim="1981-2010",
                       period_out_clim="clim",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="rr",
                       varunit="mm",
                       var_longname="thickness_of_rainfall_amount",
                       wt_index="pct09",
                       listWTS=list_WTS_clim)


create_stacks( period_clim="1998-2018",
                       period_out_clim="9818",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="rr",
                       varunit="mm",
                       var_longname="thickness_of_rainfall_amount",
                       wt_index="pct09",
                       listWTS=list_WTS_9818)

#################################################################################################
# Raining days
#################################################################################################

create_stacks( period_clim="1981-2010",
                       period_out_clim="clim",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="gp0",
                       varunit="days",
                       var_longname="days with rainfall amount > 0 mm",
                       wt_index="pct09",
                       listWTS=list_WTS_clim)


create_stacks( period_clim="1998-2018",
                       period_out_clim="9818",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="gp0",
                       varunit="days",
                       var_longname="days with rainfall amount > 0 mm",
                       wt_index="pct09",
                       listWTS=list_WTS_9818)

#################################################################################################
# Mean temperature
#################################################################################################

create_stacks( period_clim="1981-2010",
                       period_out_clim="clim",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tg",
                       varunit="degC",
                       var_longname="mean daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_clim)


create_stacks( period_clim="1998-2018",
                       period_out_clim="9818",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tg",
                       varunit="degC",
                       var_longname="mean daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_9818)




#################################################################################################
# Maximum temperature
#################################################################################################

create_stacks( period_clim="1981-2010",
                       period_out_clim="clim",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tx",
                       varunit="degC",
                       var_longname="maximum daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_clim)


create_stacks( period_clim="1998-2018",
                       period_out_clim="9818",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tx",
                       varunit="degC",
                       var_longname="maximum daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_9818)




#################################################################################################
# Minimum temperature
#################################################################################################

create_stacks( period_clim="1981-2010",
                       period_out_clim="clim",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tn",
                       varunit="degC",
                       var_longname="minimum daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_clim)


create_stacks( period_clim="1998-2018",
                       period_out_clim="9818",
                       eobs_version="v20",
                       minimal_days=1,
                       varname="tn",
                       varunit="degC",
                       var_longname="minimum daily temperature",
                       wt_index="san09",
                       listWTS=list_WTS_9818)

############################################################################################################
