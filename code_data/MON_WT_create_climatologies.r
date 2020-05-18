library(raster)
library(ncdf4)



##########################################################################################
# Setup VARS scommentare le variabili per fare anomalie clima o anomalie 1998-2018
#periodmon=".*1981-2010.*"
periodmon=".*1998-2018.*"

#periodmonout="1981-2010"
periodmonout="1998-2018"
	
#periodwt=".*clim.*"
periodwt=".*9818.*"

#periodwtout="clim"
periodwtout="9818"
##########################################################################################
# Setup directories

dirdataeobs="/home/salute/seasonal/procedure_data/stack_eobs"
dirdataWT_stacks="/home/salute/seasonal/procedure_data/stack_eobs/stacks_eobs_wt_v20"
dirdataMON_stacks="/home/salute/seasonal/procedure_data/stack_eobs/stacks_monthly/eobs_v20.0e"
dirstatsWT="/home/salute/seasonal/procedure_data/stats_eobs/stats_eobs_wt_v20.0e"
dirstatsMON="/home/salute/seasonal/procedure_data/stats_eobs/stats_eobs_month_v20.0e"
setwd(dirdataeobs)

##########################################################################################


calc_climatology_basic=function(temp,names=c("MeanClim","SdClim","Q33Clim","MedianClim","Q66Clim")) {
                                             res=list()
                                             if (nlayers(temp) == 1 ) {res[[1]]=stack(temp)*1;
                                                                                 res[[2]]=stack(temp)*0;
                                                                                 res[[3]]=stack(temp)*NA;
                                                                                 res[[4]]=stack(temp)*NA;
                                                                                 res[[5]]=stack(temp)*NA;
                                                                                 res=stack(res)
                                                                                 names(res)=names
                                                                                 return(stack(res))
                                                              }

                                              res[[1]]=calc(temp,mean,na.rm=T);
                                              res[[2]]=calc(temp,sd,na.rm=T);
                                              res[[3]]=calc(temp,fun=function(x) quantile(x,probs=c(0.33),na.rm=T));
                                              res[[4]]=calc(temp,fun=function(x) quantile(x,probs=c(0.5),na.rm=T));
                                              res[[5]]=calc(temp,fun=function(x) quantile(x,probs=c(0.66),na.rm=T));
  
                                              res=stack(res)
                                              names(res)=names
                                              return(stack(res))
}

calc_climatology_more=function(temp,names=c("MeanClim","SdClim","Q33Clim","MedianClim","Q66Clim","Q10Clim","Q90Clim")) {
                                             res=list()
                                             if (nlayers(temp) == 1 ) {res[[1]]=stack(temp)*1;
                                                                                 res[[2]]=stack(temp)*0;
                                                                                 res[[3]]=stack(temp)*NA;
                                                                                 res[[4]]=stack(temp)*NA;
                                                                                 res[[5]]=stack(temp)*NA;
                                                                                 res[[6]]=stack(temp)*NA;
                                                                                 res[[7]]=stack(temp)*NA;
                                                                                 res=stack(res)
                                                                                 names(res)=names
                                                                                 return(stack(res))
                                                              }

                                              res[[1]]=calc(temp,mean,na.rm=T);
                                              res[[2]]=calc(temp,sd,na.rm=T);
                                              res[[3]]=calc(temp,fun=function(x) quantile(x,probs=c(0.33),na.rm=T));
                                              res[[4]]=calc(temp,fun=function(x) quantile(x,probs=c(0.5),na.rm=T));
                                              res[[5]]=calc(temp,fun=function(x) quantile(x,probs=c(0.66),na.rm=T));
                                              res[[6]]=calc(temp,fun=function(x) quantile(x,probs=c(0.1),na.rm=T));
                                              res[[7]]=calc(temp,fun=function(x) quantile(x,probs=c(0.9),na.rm=T));
  
                                              res=stack(res)
                                              names(res)=names
                                              return(stack(res))
}

clim_normalize_day=function(temp,monthnumber,names=c("MeanClim","SdClim","Q33Clim","MedianClim","Q66Clim")) {
                                            res=list()
                                            daysM=c(31,28,31,
                                                          30,31,30,
                                                          31,31,30,
                                                          31,30,31)
                                            res[[1]]=temp[[1]]*daysM[monthnumber]
                                            res[[2]]=temp[[2]]
                                            res[[3]]=temp[[3]]*daysM[monthnumber]
                                            res[[4]]=temp[[4]]*daysM[monthnumber]
                                            res[[5]]=temp[[5]]*daysM[monthnumber]
                                            res=stack(res)
                                            names(res)=names
                                            return(stack(res))
}
  
clim_normalize_day_more=function(temp,monthnumber,names=c("MeanClim","SdClim","Q33Clim","MedianClim","Q66Clim","Q10Clim","Q90Clim")) {
                                            res=list()
                                            daysM=c(31,28,31,
                                            30,31,30,
                                            31,31,30,
                                            31,30,31)
                                            res[[1]]=temp[[1]]*daysM[monthnumber]
                                            res[[2]]=temp[[2]]
                                            res[[3]]=temp[[3]]*daysM[monthnumber]
                                            res[[4]]=temp[[4]]*daysM[monthnumber]
                                            res[[5]]=temp[[5]]*daysM[monthnumber]
                                            res[[6]]=temp[[6]]*daysM[monthnumber]
                                            res[[7]]=temp[[7]]*daysM[monthnumber]
                                            res=stack(res)
                                            names(res)=names
                                            return(stack(res))
}


##########################################################################################
# define parameters

e <- extent(-30,30,30,70) # final extent 

vars=c("rr","tg","tx","tn","gp") 

########################################################################

outcomes_n_clim=c("mean","sd","q33","q50","q66","q10","q90","anom")

names_template_month=c("MeanClim","SdClim","Q33Clim","MedianClim","Q66Clim","Q10Clim","Q90Clim")

names_template_WT=c("MeanClim_WT","SdClim_WT","Q33Clim_WT","MedianClim_WT","Q66Clim_WT","Q10Clim_WT","Q90Clim_WT","AnomClim_WT")

daysM=c(31,28,31,
               30,31,30,
               31,31,30,
               31,30,31)


unit_list=list(var_units_rr=c("millimeters","millimeters","millimeters","millimeters","probability","probability","probability","cat","probability","probability","millimeters"),
                  var_units_tg=c("celsius degree","celsius degree","celsius degree","celsius degree","probability","probability","probability","cat","probability","probability","celsius degree"),
                  var_units_tx=c("celsius degree","celsius degree","celsius degree","celsius degree","probability","probability","probability","cat","probability","probability","celsius degree"),
                  var_units_tn=c("celsius degree","celsius degree","celsius degree","celsius degree","probability","probability","probability","cat","probability","probability","celsius degree"),
                  var_units_gp=c("days","days","days","days","probability","probability","probability","cat","probability","probability","days")
)


###############################################################################################################################################à
# to create directory under stats dir wt 

for ( i in 1:length(vars)) {
                                   if ( !dir.exists(paste0(dirstatsWT,"/",paste0(vars[i],"_wt_",periodwtout)))) 
                                                                                                                               { dir.create(paste0(dirstatsWT,"/",paste0(vars[i],"_wt_",periodwtout)));
                                                                                                                               }
                                 }
#########################################################################################


for ( i in 1:length(vars)) {
                       
                                    ##############################################################################################################
                                   # Vars data

                                    for ( j in 1:12) {
                                                         # Monthly data
		 
                                                         namem=list.files(path=dirdataMON_stacks,pattern=paste0(vars[i],periodmon,"_",j,".nc"),recursive = T,full.names = T)
                                                         namem_rel=list.files(path=dirdataMON_stacks,pattern=paste0(vars[i],periodmon,"_",j,".nc"),recursive = T,full.names = F)
                                                         message(namem)
                                                         nameoutm=gsub("_0.1deg","", namem_rel)

                                                         res_mon=brick(namem) 
                                                         res_mon=calc_climatology_more(res_mon)
                                                         temp_month=crop(res_mon,e)
                      
                                                          if  ( vars[i]=="rr" || vars[i]=="gp" ) {temp_month=clim_normalize_day_more(temp_month,monthnumber=j)};
                       
                                      ##############################################################################################################
                                      # Save in netCDF format

                                                          for ( zz in 1:length(names_template_month)) {
                                                                                                                            writeRaster(temp_month[[zz]],
                                                                                                                            filename=paste0(dirstatsMON,"/",names_template_month[zz],"_",vars[i],"_",periodmonout,"_",j,".nc"),
                                                                                                                            "CDF", 
                                                                                                                            overwrite=TRUE,
                                                                                                                            varname=names_template_month[zz],
                                                                                                                            varunit=unit_list[[i]][[1]],
                                                                                                                            longname=paste0(names_template_month[zz],"_",vars[i]),
                                                                                                                            xname="lon",
                                                                                                                            yname="lat");
                                                                                                                            }
                       
                                     ##############################################################################################################
                                     # WT monthly data

		         for ( z in 1:9) {

                                          name=list.files(path=dirdataWT_stacks,pattern=paste0("*",vars[i],periodwt,"_",j,"_",z),recursive = T,full.names = T);
                                          name_rel=list.files(path=dirdataWT_stacks,pattern=paste0(vars[i],periodwt,"_",j,"_",z),recursive = T,full.names = F);

                                          message(name)
                                          message(name_rel)

                                          if ( length(name)==0) {next};
                                          
                                          tempWT=brick(name)           

                                          WT_stack=calc_climatology_more(tempWT) 

                                          temp_month_WT=crop(WT_stack,e)

                                          if ( vars[i]=="rr" || vars[i]=="gp" )  {temp_month_WT=clim_normalize_day_more(temp_month_WT,monthnumber=j);}

                                          for (zz in 1:length(names_template_month)) { 
                                                                                                           writeRaster(temp_month_WT[[zz]],
                                                                                                           filename=paste0(dirstatsWT,"/",vars[i],"_wt_",periodwtout,"/",names_template_month[zz],"_",vars[i],"_",j,"_",z,".nc"),
                                                                                                           "CDF", 
                                                                                                           overwrite=TRUE,
                                                                                                           varname=names_template_month[zz],
                                                                                                           varunit=unit_list[[i]][[1]],
                                                                                                           longname=paste0(names_template_month[zz],"_",j,"_",vars[i],"_",z),
                                                                                                           xname="lon",
                                                                                                           yname="lat");
                                                                                                           }


                                          writeRaster(temp_month_WT[[1]]-temp_month[[1]],
                                                            filename=paste0(dirstatsWT,"/",vars[i],"_wt_",periodwtout,"/","AnomMean","_",vars[i],"_",j,"_",z,".nc"),
                                                            "CDF", 
                                                            overwrite=TRUE,
                                                            varname=paste0("AnomMean","_",j,"_",vars[i],"_",z),
                                                            varunit=unit_list[[i]][[1]],
                                                            longname=paste0("Anomalies of Means ","_",j,"_",vars[i],"_",z),
                                                            xname="lon",
                                                            yname="lat");

                                          rm( temp_month_WT);
                                                                                     
                              		} # wt loop z
                                    rm(temp_month)
                                           
                  } # months loop j
   } # vars loop  i



#################################################################################################
