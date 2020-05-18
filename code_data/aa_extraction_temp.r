library(raster)
library(ncdf4)


##################################################################################################
# Directories


setwd("/home/salute/seasonal/procedure_data/stack_eobs/elab_data/")


all_wt=c("/home/salute/seasonal/procedure_data/stack_eobs/stacks_eobs_wt")

stack_eobs_month=c("/home/salute/seasonal/procedure_data/stack_eobs/stacks_monthly")

files_all=list.files(path=all_wt,pattern ="nc",recursive=T,full.names = T)
files_all_month=list.files(path=stack_eobs_month,pattern ="nc",recursive=T,full.names = T)

idna=readRDS("data/index_na_eobs.rds")

e <- extent(-30,30,30,70) # final extent 

#####################################################################################################################################
# Functions and namimg

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


#####################################################################################################################
# thermal parameters

vars=c("tg","tx","tn")
i=0 # index vars

#####################################################################################################################################



for ( var in vars) {
i=i+1;
dir_CLIM=paste0("climatologies_",var)
dir.create(dir_CLIM)

res_quant=list(list(),list(),list(),
                     list(),list(),list(),
                     list(),list(),list(),
                     list(),list(),list())


for ( mese in 1:12) {


 #####################################################################################################################################
 # stack monthly

  clim_stack=brick(grep(paste0(".*",var,".*stack_",mese,".nc"),files_all_month,value=T))
  
  message(paste("Reading climatology of ",mese," ", grep(paste0(".*",var,".*stack_",mese,".nc"),files_all_month,value=T),"...."))

  temp_month_clim=calc(clim_stack,qp_on_temp)
  
  names(temp_month_clim)=c("Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95")
  
  temp_month_clim=readAll(temp_month_clim)
  temp_month_clim[idna]=NA
 
  saveRDS(temp_month_clim,file=paste0("statsbayes_clim_",var,"_",mese,".rds"))

  message(paste("\n..fatto!"))

   #####################################################################################################################################

   wt_stack=lapply(grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T),stack)

   message(paste("Reading WT of ",mese," ", grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T),"....\n"))
   
  #####################################################################################################################################
  # calcolo delle soglie dei quantili per tipo di tempo
  
   temp_wt_temp=lapply(wt_stack,
                                   FUN=function(x) { if (nlayers(x)==1) {  res=stack(x[[1]],x[[1]],x[[1]],x[[1]],x[[1]],x[[1]],x[[1]],x[[1]]);
                                                                                        names(res)=c("Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95");
                                                                                        res[idna]=NA
                                                                                        return(res)
                                                                                      } 
                                                             else             
                                                                                      {  res=calc(x,qp_on_temp);
                                                                                         names(res)=c("Q_5","Q_10","Q_33","Q_50","Q_66","Q_75","Q_90","Q_95");
                                                                                         res[idna]=NA
                                                                                         return(res)
                                                                                      }
                                                            }
                                  ) # fine lapply

   id_wt=as.numeric(gsub(".nc","",gsub(paste0(".*",var,".*_",mese,"_"),"",grep(paste0(".*",var,".*_",mese,"_"),files_all,value=T))))
 
   names(temp_wt_temp)=id_wt

   saveRDS(temp_wt_temp,file=paste0("statsbayes_wt_clim_",var,"_",mese,".rds"))

   message(paste("\n..fatto!"))

   #####################################################################################################################################

  
   for ( j in 1:length(id_wt)) { 
                                     reswtq=list() 
                                     zzindex=1
                                     jj=id_wt[j]
                                     for ( z in 1:8) { pp=stack(wt_stack[[j]],temp_month_clim[[z]])
                                                          reswtq[[zzindex]]=calc(pp,
                                                                                           fun=function(x,...) { ind=length(x)-1;
                                                                                                                        Lna=length(which(is.na(x[1:ind])))
                                                                                                                        res=sum(as.numeric(x[1:ind])>=as.numeric(x[length(x)]))
                                                                                                                        res=ifelse(Lna>ind*0.3,NA,res)
                                                                                                                        res=ifelse(is.na(res/ind),NA,res/ind)
                                                                                                                        return(res)
                                                                                                                       }       
                                                                                            );
    
                                                          zzindex=zzindex+1
                                                        }
                                     ##############################################################################################################
                                     # Save in netCDF format and rds
                                     names_prob_temp=c("PQ_5","PQ_10","PQ_33","PQ_50","PQ_66","PQ_75","PQ_90","PQ_95")
                                     finwtq=stack(reswtq)
                                     names(finwtq)=c("PQ_5","PQ_10","PQ_33","PQ_50","PQ_66","PQ_75","PQ_90","PQ_95")
                                     finwtq[idna]=NA
                                     saveRDS(finwtq,file=paste0("statsbayes_wtProb_",var,"_",mese,"_",j,".rds"))
                                     for ( zz in 1:length(names_prob_temp)) {
                                                                                                  writeRaster( finwtq[[zz]],
                                                                                                                     filename=paste0(dir_CLIM,"/",names_prob_temp[zz],"_",vars[i],"_",mese,"_",jj,".nc"),
                                                                                                                     "CDF", 
                                                                                                                     overwrite=TRUE,
                                                                                                                     varname=names_prob_temp[zz],
                                                                                                                     varunit="ProbOverTresh" ,
                                                                                                                     longname=paste0(names_prob_temp[zz],"_",vars[i]),
                                                                                                                     xname="lon",
                                                                                                                     yname="lat");
                                                                                                 }

 ##############################################################################################################

    res_quant[[mese]][[jj]]=finwtq;
 
  } # loop wt
 }  # loop mese

   message(paste("Mese ",var,"_",mese,"..fatto!"))

   saveRDS(res_quant,file=paste0("fullstat_bayes_wtclim_",var,".rds"))

} # loop var



 
 







