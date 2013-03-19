library(shiny)
library(flowWorkspace)
path <- ("/home/wjiang2/rglab/workspace/ShinyApp_flowWorkspace/gs")
unarchive_new<-function(path){
  
  if(!file.exists(path))
    stop(file,"' not found!")
#   browser()
  files<-list.files(path)
  
  dat.file<-file.path(path,files[grep(".dat$",files)])
  rds.file<-file.path(path,files[grep(".rds$",files)])
  
  nc.file<-file.path(path,files[grep(".nc$|.nc.trans$",files)])
  #	browser()
  if(length(dat.file)==0)
    stop(".dat file missing in ",file)
  if(length(dat.file)>1)
    stop("multiple .dat files found in ",file)
  if(length(rds.file)==0)
    stop(".rds file missing in ",file)
  if(length(rds.file)>1)
    stop("multiple .rds files found in ",file)
  
  message("loading R object...")
  gs<-readRDS(rds.file)
  
  message("loading tree object...")
  gs@pointer<-.Call("R_loadGatingSet",dat.file)
  #update the pointer in each gating hierarchy
  for(i in 1:length(gs@set))
  {
    gs@set[[i]]@pointer<-gs@pointer
  }
  if(flowWorkspace:::isNcdf(gs[[1]]))
  {
    if(length(nc.file)==0)
      stop(".nc file missing in ",file)
    ncFlowSet(gs)@file<-nc.file
    
  }
  
  #clean up the intermediate files
  message("Done")
  return (gs)
  
}
gs <- unarchive_new(path)
#preprocess pdata
pd <- pData(gs)
# #make Stim column for negctrl unqiue
# pd <- by(pd,list(pd$PTID,pd$VISITNO),function(cur_group){
# #   browser()
#   cur_group[,"Stim"] <- as.character(cur_group[,"Stim"])
#   neg_ind <- grep("negctrl",cur_group[,"Stim"])
#   cur_group[neg_ind,"Stim"] <- paste(cur_group[neg_ind,"Stim"],c(1,2))
#   cur_group
# })
# pd <- do.call(rbind,pd)
# 
# #factorize study variables
# pd$Stim <- factor(pd$Stim)
# pd$PTID <- factor(pd$PTID)
# pd$VISITNO <- factor(pd$VISITNO)
# pd$SampleOrder <- factor(pd$SampleOrder)
# pd$METHOD <- factor(pd$METHOD)


# pData(gs) <- pd
