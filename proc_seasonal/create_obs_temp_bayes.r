
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least 3 arguments must be supplied!\n a)  name san09 files \n c) numerical year integer \n d) numerical month \n")}

message(paste(args[1],"...!"))
message(paste(args[2],"...!"))
message(paste(args[3],"...!"))

##########################################################################################

library(raster)
library(ncdf4)


##########################################################################################
# Naming directories

dirwk="/home/salute/seasonal/outcomes"
outdir="/home/salute/data/output/clima/"

dir_eobs_statbayes="/home/salute/seasonal/procedure_data/stats_eobs/stats_eobs_wt/stats_bayes"


##########################################################################################
# functions & vars definition

vars=c("tg","tx","tn")

estrai_wt=function(x) as.numeric(sub(".nc.rds","",sub(".*wt.*_","",x)))

weighted.sum=function(x,w){ sum(x*w,na.rm=T) }

##########################################################################################
# Call working directory

setwd(dirwk)

idna=readRDS("data/index_na_eobs.rds")  # index of grid point for masking data

tg_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tg.rds")) 
tx_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tx.rds")) 
tn_full_bayes=readRDS(paste0(dir_eobs_statbayes,"/","fullstat_bayes_wtclim_tn.rds")) 


thermal_par_list=list(tg_full_bayes,tx_full_bayes,tn_full_bayes)



##########################################################################################
# debug # san9_weights=read.table("/home/salute/seasonal/PESIOBSaprile_2019SAN9.txt",header=T) [,2:10]
# debug # mon_forecast=read.table("/home/salute/seasonal/PESIOBSaprile_2019SAN9.txt",header=T) [,1]


san9_weights=read.table(args[1],header=T) [,2:10]
mon_forecast=read.table(args[1],header=T) [,1]
names(san9_weights)=c("WT1","WT2","WT3","WT4","WT5","WT6","WT7","WT8","WT9")
saveRDS(san9_weights,paste0("/home/salute/seasonal/archive/san9_weights_",args[2],"_",args[3],".rds"))

ls_forecast_thermal=list(san9_weights)
names(ls_forecast_thermal)=mon_forecast



#####################################################################################################################################
# Creazione delle previsioni

res_vars=list()
vars=c("tg","tx","tn")
nvars=length(vars)
ls_weigths=ls_forecast_thermal


############################################################################################################################

for ( i in 1:nvars) {

mon=1
res_months=list()
mon_forecast=as.numeric(names(ls_forecast_thermal))
stat_bayes_wtclim=thermal_par_list[[i]]

for ( j in mon_forecast)  {
        weigths=as.numeric(ls_weigths[[mon]])
        wt_stack=stat_bayes_wtclim[[j]][which(weigths>0)]
        weigths=weigths[which(weigths>0)]
        clim33=stack(lapply(wt_stack,function(x) 100*x[[3]])) # return a rasterstack with not null weigths wt 
        clim66=stack(lapply(wt_stack,function(x) 100*x[[5]]))  # return a rasterstack with not null weigths wt 
        p33=100-clim33
        pl33=calc(p33,function(x) weighted.sum(x,weigths))
        pu66=calc(clim66,function(x) weighted.sum(x,weigths))
        p3366=(100-pu66)-pl33
        res=stack(pl33,p3366,pu66)
        res[idna]=NA
        names(res)=c("plTEns","pmTEns","puTEns")
        res_months[[mon]]=res
        mon=mon+1
  }

res_vars[[i]]=res_months

}

saveRDS(res_vars,paste0("obs_bayes_temp_last_",args[2],"_",args[3],".rds"))

message("...fatto!")

#############################################################################################
message("Scrivo uscite...")

setwd(outdir)

e <- extent(-30,30,30,70) # final extent 

vars=c("tg","tx","tn") # predicted vars 

nvars=length(vars)

names_template=c("plTEns","pmTEns","puTEns")

#############################################################################################

for ( par in 1:nvars) {

names_template_var = paste0(names_template,"_",vars[par])

temp_mon_var=res_vars[[par]][[1]]

for ( z in 1:length(names_template_var)) {
                                                          writeRaster(crop(temp_mon_var[[z]],e),
                                                                           filename=paste0("obs_",names_template_var[z],"_",par,"_",args[2],"_",args[3],"_bayes.nc"),
                                                                           "CDF", 
                                                                            overwrite=TRUE,
                                                                            varname=names_template_var[z],
                                                                            varunit="probability*100",
                                                                            longname=names_template_var[z],
                                                                            xname="lon",
                                                                            yname="lat")
                                                         
                                                           }

                            
}

#############################################################################################

message("...fatto!")

#############################################################################################
