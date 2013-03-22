library(shiny)
library(flowWorkspace)
path <- ("/home/wjiang2/rglab/workspace/ShinyApp_flowWorkspace_devel")
gs_HVTN_small <- flowWorkspace:::load_gs(path=file.path(path,"HVTN-080-small"))
# gs_RV144 <- flowWorkspace:::load_gs(path=file.path(path,"RV144"))
#preprocess pdata
# pd <- pData(gs_small)
# #make Stim column for negctrl unqiue
# pd <- by(pd,list(pd$PTID,pd$VISITNO),function(cur_group){
# #   browser()
#   cur_group[,"Stim"] <- as.character(cur_group[,"Stim"])
#   neg_ind <- grep("negctrl",cur_group[,"Stim"])
#   cur_group[neg_ind,"Stim"] <- paste(cur_group[neg_ind,"Stim"],c(1,2))
#   cur_group
# })
# pd <- do.call(rbind,pd)
# # 
# # #factorize study variables
# pd$Stim <- factor(pd$Stim)
# pd$PTID <- factor(pd$PTID)
# pd$VISITNO <- factor(pd$VISITNO)
# pd$SampleOrder <- factor(pd$SampleOrder)
# pd$METHOD <- factor(pd$METHOD)
# 
# 
# pData(gs_small) <- pd

