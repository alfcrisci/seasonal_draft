##########################################################################################
#  Code to create bayesian thermal seasonal forecasts by using eobs
# 
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

vars=c("tg","tx","tn")

estrai_wt=function(x) as.numeric(sub(".nc.rds","",sub(".*wt.*_","",x)))

weighted.sum=function(x,w){ sum(x*w,na.rm=T) }

##########################################################################################
# Call working directory

setwd(dirwk)

mask=readRDS("data/mask_crop.rds")  # mask



message("Reading bayesian information....")

tg_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tg_9818.rds")) 
tx_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tx_9818.rds")) 
tn_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tn_9818.rds"))

message("done!")


thermal_par_list=list(tg_full_bayes,tx_full_bayes,tn_full_bayes)


################################################################l#########################
# read pesi already calculated and storing in R format

message("Reading weigths....")

san9_weights=read.table("/home/salute/seasonal/PESI-san9.csv",header=T) 

saveRDS(san9_weights,paste0("/home/salute/seasonal/archive/san9_weights_",Sys.Date(),"_",period_wt,".rds"))

message("done!")
#####################################################################################################################################

ls_forecast_thermal=list(san9_weights[1,2:10],san9_weights[2,2:10],san9_weights[3,2:10])
names(ls_forecast_thermal)=san9_weights$mese
mon_forecast_thermal=names(ls_forecast_thermal)


#####################################################################################################################################
# Creazione delle previsioni

res_vars=list()
nvars=length(vars) # 3 thermal variables
ls_weigths=ls_forecast_thermal

for ( i in 1:nvars) {

mon=1
res_months=list()
mon_forecast=as.numeric(mon_forecast_thermal)
stat_bayes_wtclim=thermal_par_list[[i]]

for ( j in mon_forecast)  {
        weigths=as.numeric(ls_weigths[[mon]])
        wt_stack=stat_bayes_wtclim[[j]][which(weigths>0)]
        weigths=weigths[which(weigths>0)]


        clim33=stack(sapply(1:length(wt_stack),function(x) wt_stack[[x]][[3]]*100)) # return a rasterstack with not null weigths wt 
        clim66=stack(sapply(1:length(wt_stack),function(x) wt_stack[[x]][[5]]*100)) # return a rasterstack with not null weigths wt 
     

        p33=100-clim33
        pl33=calc(p33,function(x) weighted.sum(x,weigths))
        pu66=calc(clim66,function(x) weighted.sum(x,weigths))
        p3366=(100-pu66)-pl33
        res=stack(pl33,p3366,pu66)
        names(res)=c("plTEns","pmTEns","puTEns")
        res_months[[mon]]=res
        mon=mon+1
  }

res_vars[[i]]=res_months

}

# saveRDS(res_vars,paste0("archive/obs_bayes_temp_last_",Sys.Date(),"_",period_wt,".rds"))

message("...fatto!")

#############################################################################################

message("Scrivo uscite...")

setwd(outdir)

e <- extent(-30,30,30,70)  # final extent 
names_template=c("plTEns","pmTEns","puTEns")

#############################################################################################

for ( par in 1:nvars) {

names_template_var = paste0(names_template,"_",vars[par])

message("done!")

for ( mon in 1:3)      {


for ( z in 1:length(names_template_var)) {
                                                          writeRaster(crop(res_vars[[par]][[mon]][[z]],e)*mask,
                                                                           filename=paste0("forecast_",names_template_var[z],"_",mon,"_bayes.nc"),
                                                                           "CDF", 
                                                                            overwrite=TRUE,
                                                                            varname=names_template_var[z],
                                                                            varunit="probability*100",
                                                                            longname=names_template_var[z],
                                                                            xname="lon",
                                                                            yname="lat")
                                                         
                                                           }

                            
                              }
                              }

#############################################################################################

message("Forecast Temperatures 9818...fatto!")

#############################################################################################
