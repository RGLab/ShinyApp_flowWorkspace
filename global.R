library(shiny)
studies_path <- "~/rglab/workspace/ShinyApp_flowWorkspace_devel/study"
studies <- list.dirs(studies_path,recursive=FALSE)
studies <- basename(studies)[-7]

#select test set
studies <- studies[1]

#exclude test set
#studies <- studies[-1]