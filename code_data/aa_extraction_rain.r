library(raster)
library(ncdf4)


#####################################################################################################################################
# Directories

setwd("/home/salute/seasonal/procedure_data/stack_eobs/stacks_eobs_wt_v20/elab_data/")

dir.create("climatologies_rr")

all_wt=c("/home/salute/seasonal/procedure_data/stack_eobs/stacks_eobs_wt_v20")
stack_eobs_month=c("/home/salute/seasonal/procedure_data/stack_eobs/stacks_monthly/eobs_v20.0e")

files_all=list.files(path=all_wt,pattern ="nc",recursive=T,full.names = T)
files_all_month=list.files(path=stack_eobs_month,pattern ="nc",recursive=T,full.names = T)

# stats_eobs_month=c("/home/salute/seasonal/procedure_data/stats_eobs/stats_eobs_month")
# files_clim=list.files(path=stats_eobs_month,pattern ="rds",recursive=T,full.names = T)

idna=readRDS("data/index_na_eobs_v20.rds")

#####################################################################################################################################
# Functions and namimg

e <- extent(-30,30,30,70) # final extent 

estrai_wt=function(x) as.numeric(sub(".nc.rds","",sub(".*wt.*_","",x)))

prain=function(x,val=0) {length(which(x>val))/length(x)}

qp_on_rain=function(x) { res=x[which(x>0)];
                                    p_rain=(length(which(x>0))/length(x))*100;
                                    p_norain=100-p_rain;
                                    c(p_rain,p_norain,as.numeric(quantile(res,probs=c(0.05,0.10,0.33,0.50,0.66,0.75,0.90,0.95),na.rm=T)))
}

qp_on_temp=function(x) { 
  c(as.numeric(quantile(x,probs=c(0.05,0.10,0.33,0.50,0.66,0.75,0.90,0.95),na.rm=T)))
}

names_qp=c("Prain","Pnorain","Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95")
names_qt=c("Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95")

#####################################################################################################################################
# rainfall 

var="rr"

#####################################################################################################################################


res_quant=list(list(),list(),list(),
                     list(),list(),list(),
                     list(),list(),list(),
                     list(),list(),list())

for ( mese in 1:12) {

  
# stack monthly

  clim_stack=brick(grep(paste0(".*",var,".*stack_",mese,".nc"),files_all_month,value=T))

  message(paste("Reading climatology of ",mese," ", grep(paste0(".*",var,".*stack_",mese,".nc"),files_all_month,value=T),"...."))


  temp_month_clim=calc(clim_stack,qp_on_rain)

  temp_month_clim=readAll( temp_month_clim)

  names(temp_month_clim)=c("Prain","Pnorain","Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95")

  temp_month_clim[idna]=NA

  saveRDS(temp_month_clim,file=paste0("statsbayes_clim_rr_",mese,".rds"))

  message(paste("\n..fatto!"))

  wt_stack=lapply(grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T),stack)
 

  message(paste("Reading WT of ",mese," ", grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T),"....\n"))

#####################################################################################################################################

  temp_wt_rain=lapply(wt_stack,function(x) {res=calc(x,qp_on_rain);
                                                                       names(res)=c("Prain","Pnorain","Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95");
                                                                       return(res)
                                                                 }
                                )

  temp_wt_rain=stack(temp_wt_rain)

  temp_wt_rain[idna]=NA

  
  saveRDS(temp_wt_rain,file=paste0("statsbayes_wt_clim_rr_",mese,".rds"))

  message(paste("\n..fatto!"))

#####################################################################################################################################

  id_wt=as.numeric(gsub(".nc","",gsub(paste0(".*",var,".*_",mese,"_"),"",grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T))))
 


  for ( j in id_wt) { 

  reswtq=list()  

  zzindex=1


  for ( z in 3:10) {pp=stack(wt_stack[[j]],temp_month_clim[[z]])
                         reswtq[[zzindex]]=calc(pp,fun=function(x,...) { ind=length(x)-1;
                                                                                            res=sum(as.numeric(x[1:ind])>as.numeric(x[length(x)]))
                                                                                            Lna=length(which(is.na(x[1:ind])))
                                                                                            res=ifelse(is.na(res/ind),0,res/ind)
                                                                                            res=ifelse(Lna==ind,NA,res)
                                                                                            return(res)
                                                                  });
                                                                      
                         zzindex=zzindex+1
                    }

   prain_temp=calc(wt_stack[[j]],prain)

   finwtq=stack(prain_temp,temp_month_clim[[1]],stack(reswtq))

   names(finwtq)=c("Prainwt","Prainclim","Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95")

 
     finwtq[idna]=NA
   
    saveRDS(finwtq,file=paste0("statsbayes_wtProb_rr_",mese,"_",j,".rds"))

    ##############################################################################################################
    # Save in netCDF format and rds

    names_prob_rain=c("Prainwt","Prainclim","PQ_5","PQ_10","PQ_33","PQ_50","PQ_66","PQ_75","PQ_90","PQ_95")

    for ( zz in 1:length(names_prob_rain)) {
                                                             writeRaster( finwtq[[zz]],
                                                                               filename=paste0("climatologies_rr","/",names_prob_rain[zz],"_rr_",mese,"_",j,".nc"),
                                                                               "CDF", 
                                                                               overwrite=TRUE,
                                                                               varname=names_prob_rain[zz],
                                                                               varunit="ProbOverTresh" ,
                                                                               longname=paste0(names_prob_rain[zz],"_rr"),
                                                                               xname="lon",
                                                                               yname="lat");
                                                           }
  ##############################################################################################################
   
    res_quant[[mese]][[j]]=finwtq;

    } # loop in wt
    
   
    message(paste("Mese ",mese,"..fatto!"))

    } # loop in mese

saveRDS(res_quant,file=paste0("fullstat_bayes_wtclim_rr.rds"))

#####################################################################################################################

