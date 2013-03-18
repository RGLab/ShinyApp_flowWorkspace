library(shiny)
library(flowWorkspace)
setwd("/home/wjiang2/rglab/workspace/gh/")
gs <- unarchive("gs_shiny_test.tar")
#preprocess pdata
pd <- pData(gs)
#make Stim column for negctrl unqiue
pd <- by(pd,list(pd$PTID,pd$VISITNO),function(cur_group){
#   browser()
  cur_group[,"Stim"] <- as.character(cur_group[,"Stim"])
  neg_ind <- grep("negctrl",cur_group[,"Stim"])
  cur_group[neg_ind,"Stim"] <- paste(cur_group[neg_ind,"Stim"],c(1,2))
  cur_group
})
pd <- do.call(rbind,pd)

#factorize study variables
pd$Stim <- factor(pd$Stim)
pd$PTID <- factor(pd$PTID)
pd$VISITNO <- factor(pd$VISITNO)
pd$SampleOrder <- factor(pd$SampleOrder)
pd$METHOD <- factor(pd$METHOD)


pData(gs) <- pd
