##########################################################################################

library(raster)
library(ncdf4)

##########################################################################################
# Naming directories

dirwk="/home/salute/seasonal/outcomes"
outdir="/home/salute/data/output/clima/v20_9818/"

dir_eobs_statbayes="/home/salute/seasonal/procedure_data/stats_eobs/stats_eobs_wt_v20/stats_bayes"
period_wt="9818"

##########################################################################################
# functions & vars definition


var=c("rr")

estrai_wt=function(x) as.numeric(sub(".nc.rds","",sub(".*wt.*_","",x)))

weighted.sum=function(x,w){ sum(x*w,na.rm=T) }

##########################################################################################
# Call working directory

setwd(dirwk)

mask=readRDS("data/mask_crop.rds")  # mask




stat_bayes_wtclim_rr=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_rr_9818.rds"))

################################################################l#########################
# read pesi already calculated and storing in R format

pct9_weights=read.table("/home/salute/seasonal/PESI-pct9.csv",header=T) 

saveRDS(pct9_weights,paste0("/home/salute/seasonal/archive/pct9_weights_",Sys.Date(),".rds"))


ls_forecast_rain=list(pct9_weights[1,2:10],pct9_weights[2,2:10],pct9_weights[3,2:10])
names(ls_forecast_rain)=pct9_weights$mese
mon_forecast_rain=names(ls_forecast_rain)


#####################################################################################################################################
# Creazione delle previsioni

res_rainocc=list()
res_rainint=list()


mon=1
mon_forecast=as.numeric(mon_forecast_rain)

for ( j in mon_forecast)  {
    
    weigths=as.numeric(ls_forecast_rain[[mon]])
    prain=stack(lapply(stat_bayes_wtclim_rr[[j]],function(x) 100*x[[1]]))
    prain_w=calc(prain,function(x) weighted.sum(x,weigths))
     names(prain_w)="Prain"
    clim33=stack(lapply(stat_bayes_wtclim_rr[[j]],function(x) 100*x[[5]]))
    clim66=stack(lapply(stat_bayes_wtclim_rr[[j]],function(x) 100*x[[7]]))  
  
    p33=100-clim33
    pl33=calc(p33,function(x) weighted.sum(x,weigths))
    pu66=calc(clim66,function(x) weighted.sum(x,weigths))
    p3366=(100-pu66)-pl33


    res=stack(pl33,p3366,pu66)
     names(res)=c("plTEns","pmTEns","puTEns")
    res_rainocc[[mon]]=prain_w
    res_rainint[[mon]]=res    
    mon=mon+1
  }


message("...fatto!")

#############################################################################################
message("Scrivo uscite...")

setwd(outdir)

e <- extent(-30,30,30,70) # final extent 

names_template=c("plTEns","pmTEns","puTEns")

#############################################################################################


for ( mon in 1:3) {

writeRaster(crop(res_rainocc[[mon]],e)*mask,
                  filename=paste0("forecast_","rainocc_",mon,"_bayes.nc"),
                  "CDF", 
                  overwrite=TRUE,
                  varname="Prainocc",
                  varunit="probability*100",
                  longname="Prain_occurence",
                  xname="lon",
                  yname="lat")
                                                         
names_template_occ = paste0(names_template,"_Pint")

for ( z in 1:length(names_template_occ)) {
                                                            writeRaster(crop(res_rainint[[mon]][[z]],e)*mask,
                                                                              filename=paste0("forecast_rainint_",names_template_occ[z],"_",mon,"_bayes.nc"),
                                                                              "CDF", 
                                                                              overwrite=TRUE,
                                                                              varname=names_template_occ[z],
                                                                              varunit="probability*100",
                                                                              longname=names_template_occ[z],
                                                                              xname="lon",
                                                                              yname="lat")
                                                         
                                                           }

                            
}

#############################################################################################

message("Forecast Rainfall 9818...fatto!")

#############################################################################################
