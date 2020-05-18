library(raster)
library(ncdf4)


dirstatsWT="/home/salute/seasonal/procedure_data/stack_eobs/stats_eobs_wt/"
dir_eobs_stacksR="/home/salute/seasonal/procedure_data/stack_eobs/stack_R"


setwd(dir_eobs_stacksR)



##################################################################################################################

# index WT var & mese

##################################################################################################################
list_rr_WT=list();

for ( jj in 1:12) {
  list_rr_WT[[jj]]=list(lapply(list.files(path=dirstatsWT,pattern=paste0("*rr.*wt_",jj,"_"),recursive = T,full.names = T),readRDS))
}

saveRDS(list_rr_WT,"list_rr_WT.rds")

list_gp_WT=list();
for ( jj in 1:12){
  list_gp_WT[[jj]]=list(lapply(list.files(path=dirstatsWT,pattern=paste0("*gp0.*wt_",jj,"_"),recursive = T,full.names = T),readRDS))
}
saveRDS(list_gp_WT,"list_gp0_WT.rds")

list_tx_WT=list();
for ( jj in 1:12){
  list_tx_WT[[jj]]=list(lapply(list.files(path=dirstatsWT,pattern=paste0("*tx.*wt_",jj,"_"),recursive = T,full.names = T),readRDS))
}
saveRDS(list_tx_WT,"list_tx_WT.rds")

list_tg_WT=list();
for ( jj in 1:12){
  list_tg_WT[[jj]]=list(lapply(list.files(path=dirstatsWT,pattern=paste0("*tg.*wt_",jj,"_"),recursive = T,full.names = T),readRDS))
}
saveRDS(list_tg_WT,"list_tg_WT.rds")

list_tn_WT=list();
for ( jj in 1:12) {
  list_tn_WT[[jj]]=list(lapply(list.files(path=dirstatsWT,pattern=paste0("*tn.*wt_",jj,"_"),recursive = T,full.names = T),readRDS))
}
saveRDS(list_tn_WT,"list_tn_WT.rds")

##################################################################################################################
# Define variables

# index mese e WT


list_months_stacks_full=list(readRDS(paste0(dir_eobs_stacksR,"/stack_rr_81_10.rds")),
                             readRDS(paste0(dir_eobs_stacksR,"/stack_tg_81_10.rds")),
                             readRDS(paste0(dir_eobs_stacksR,"/stack_tx_81_10.rds")),
                             readRDS(paste0(dir_eobs_stacksR,"/stack_tn_81_10.rds")),
                             readRDS(paste0(dir_eobs_stacksR,"/stack_gp0_81_10.rds")))

saveRDS(list_months_stacks_full,"list_months_stacks_full.rds")

# list_months_stacks_full=readRDS("list_months_stacks_full.rds")

#################################################################################################à
