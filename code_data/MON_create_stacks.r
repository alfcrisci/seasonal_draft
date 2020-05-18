library(raster)
library(ncdf4)


setwd("/home/salute/seasonal/procedure_data/stack_eobs/stacks_monthly")

############################################################################################

clim_tx=list.files(pattern=".*tx.*nc")
clim_tx=clim_tx[c(4:12,1:3)]
stack_tx=sapply(clim_tx,brick)
#stack_tx=lapply(stack_tx,readAll)
saveRDS(stack_tx,"../stacks_R/stack_tx_81_10.rds")

clim_tn=list.files(pattern=".*tn.*nc")
clim_tn=clim_tn[c(4:12,1:3)]
stack_tn=sapply(clim_tn,brick)
#stack_tn=lapply(stack_tn,readAll)
saveRDS(stack_tn,"../stacks_R/stack_tn_81_10.rds")

clim_tg=list.files(pattern=".*tg.*nc")
clim_tg=clim_tg[c(4:12,1:3)]
stack_tg=sapply(clim_tg,brick)
#stack_tg=lapply(stack_tg,readAll)
saveRDS(stack_tg,"../stacks_R/stack_tg_81_10.rds")

clim_rr=list.files(pattern=".*rr.*nc")
clim_rr=clim_rr[c(4:12,1:3)]
stack_rr=sapply(clim_rr,brick)
#stack_rr=lapply(stack_rr,readAll)
saveRDS(stack_rr,"../stacks_R/stack_rr_81_10.rds")

clim_gp=list.files(pattern=".*gp.*nc")
clim_gp=clim_gp[c(4:12,1:3)]
stack_gp=sapply(clim_gp,brick)
#stack_gp=lapply(stack_gp,readAll)
saveRDS(stack_gp,"../stacks_R/stack_gp0_81_10.rds")

#clim_pp=list.files(pattern=".*pp.*nc")
#clim_pp=clim_gpm1[c(4:12,1:3)]
stack_pp=sapply(clim_pp,brick)
#stack_pp=lapply(stack_pp,readAll)
#saveRDS(stack_pp,"../stacks_R/stack_ppsan09_81_10.rds")
#saveRDS(stack_pp,"../stacks_R/stack_pppct09_81_10.rds")

############################################################################################




