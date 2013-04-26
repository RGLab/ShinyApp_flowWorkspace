# library(googleVis)
# library(flowWorkspace)
library(flowIncubator)

#pre-load gatingset,pdata and stats

#test set
 gs <- list()
pd <- list()
stats <- list()

for(this_study in studies){
  
  this_path <- file.path(studies_path,this_study)
  nDir <- length(list.dirs(file.path(studies_path,this_study),recursive = F))
  if(nDir>0){
    gs[[this_study]] <- load_gslist(path=this_path)    
  }else{
    gs[[this_study]] <- load_gs(path=this_path)
  }

 pd[[this_study]]  <-pData(gs[[this_study]] )
 stats[[this_study]]  <- getPopStats(gs[[this_study]])

}


shinyServer(function(input, output) {
#   browser()
      study_selected <- reactive({
        input$study  
      })
      
      output$titleCntrol <- renderUI({
        
        headerPanel(study_selected(),"flowWorkspace & flowViz")
      })
      
      gs_preloaded <- reactive({
        this_study <- study_selected()
      gs[[this_study]]
	 })
      pd_preloaded <- reactive({
        this_study <- study_selected()
       pd[[this_study]]

      })
      stats_preloaded <- reactive({
        this_study <- study_selected()
       stats[[this_study]]

      })
      
      output$gh_plot <- renderPlot({
        plot(gs_preloaded()[[1]], input$root_selected)
      })
      output$FilterControls <- renderUI({
        
            this_pd <- pd_preloaded()
            study_variables <- colnames(this_pd)
            name_ind <- match("name",study_variables)
           study_variables <- study_variables[-name_ind]
            lapply(study_variables,function(this_variable){
              this_choices <- unique(as.character(this_pd[,this_variable]))
             if(this_variable == "PTID"){
                this_selected <- this_choices[1]  
             }else{
               this_selected <- this_choices[1:length(this_choices)]
             }
              
              
              selectInput(this_variable, this_variable, 
                          choices = this_choices
                          ,selected = this_selected
                          ,multiple = TRUE
              )
            })
        #     browser()
                
        
      })
      
      

      pop_filtered <-reactive({
        gh <- gs_preloaded()[[1]]
        populations <- getNodes(gh,isPath=TRUE)
        pop_ind <- 1:length(populations)
        names(pop_ind) <- populations
        pop_ind <- pop_ind[-1]
        #exclude bool gate
#         bool_ind <- unlist(lapply(pop_ind,function(cur_ind){flowWorkspace:::.isBoolGate(gh,cur_ind)}))
#         pop_ind[!bool_ind]
      })
      output$popCntrol <- renderUI({
#         browser()
        pop_ind <- pop_filtered()
        selectInput("pops", "Populations:", 
                      choices = pop_ind
                      ,selected = names(pop_ind[5])
                      ,multiple = FALSE)
      })
      output$overlayPopCntrol <- renderUI({
        #         browser()
        pop_ind <- pop_filtered()
        
        selectInput("overlay_pops", "", 
                    choices = pop_ind
                    )
      })
      
     output$groupCntrol <- renderUI({
       this_choices <- colnames(pd_preloaded())
       selectInput("group", "Group by:", 
                    choices = this_choices
                    ,selected = this_choices[1]
                    ,multiple = TRUE
       )
     })
      
      output$rootCntrol <- renderUI({
        gh <- gs_preloaded()[[1]]
        allNodes <- getNodes(gh)
        isLeaf <- sapply(allNodes, function(this_node){
              length(getChildren(gh,this_node))==0
        })
        selectInput("root_selected", "select root", 
                    choices = allNodes[!isLeaf]
                    ,selected = "root"
                    ,multiple = FALSE)
      })
      output$axisCntrol <- renderUI({
    
        selectInput("x_axis", "X-axis", 
                    choices = colnames(pd_preloaded())
                    ,selected = "Stim"
                    ,multiple = FALSE)
      })
      
     output$condCntrol <- renderUI({
#         browser()
         group_v <-  input$group
         cond_type <- ifelse(input$oneLevel,":","+")
         group_v <- paste(group_v,collapse=cond_type)
         textInput("cond", "group selected:", value = group_v)  
       })
      
      selected_samples <- reactive({
#         browser()
        this_pd <- pd_preloaded()
        study_variables <- colnames(this_pd)
        name_ind <- match("name",study_variables)
        study_variables <- study_variables[-name_ind]
        this_samples <- this_pd
        for(this_variable in study_variables){
          uu <- eval(substitute(as.symbol(u),list(u=this_variable)))
          this_filter <- substitute(subset(this_samples, u%in%input[[v]]),list(u=uu,v=this_variable))
          this_samples <- eval(this_filter)
        }
        
          
 	    this_samples <- as.character(this_samples[,"name"])
      #	this_samples <- as.character(pd_preloaded()$name)
          if(length(this_samples) == 0)this_samples = 1
            this_samples
      })
      
      cur_pd <- reactive({
#         browser()
        pd_preloaded()[selected_samples(),,drop=FALSE]
      })
      # Reactive expression
       gs_input <- reactive({
         gs_preloaded()[selected_samples()]
    
      })
      
      pop_stats_selected <- reactive({
#         browser()
        pop_ind <- as.integer(input$pops)
        p_stat <- stats_preloaded()[pop_ind,,drop=FALSE]
        t(p_stat)
      })
        
     
      nSamples <- reactive({
        nrow(cur_pd())
      })
      
      output$rowsControl <- renderUI({
          #reset rows when unchecked
            numericInput("rows","rows:",value=0,min=0)    
      })
#       output$widthControl <- renderUI({
#         #reset rows when unchecked
#         numericInput("w_width","width:",value=0,min=0)    
#       })
      output$heightControl <- renderUI({
        #reset rows when unchecked
        numericInput("w_height","height:",value=400,min=0)    
      })
      #calculate the appropriate nrow* ncol for lattice
      #always set page as 1 since shiny does not know
      #how to render multi-page pdf yet
      auto_layout <- function(studyVarsArray){
        
        subpd <- cur_pd()
        #           newpd <- as.data.frame(
        #             lapply(
        #               subpd,
        #               function(x) {
        #                 if( ! all( is.na(x) ) ) {
        #                   gdata:::drop.levels(x);
        #                 } else {
        #                   x;
        #                 }
        #               }
        #             )
        #           );
        #           rownames( newpd ) <- rownames( subpd );
        #           colnames( newpd ) <- colnames( subpd );
        #           subG <- gs_input()
        #           pData( subG ) <- newpd;
        #           
        #           newpd <- newpd[ , gsub( "`", "", (studyVarsArray) ) ];
        #           npanels <- prod( do.call( c, lapply( newpd, nlevels) ) );
        #           # set the number of columns to the number of levels in the first study variable
        #           dim <- do.call( c, lapply( newpd, nlevels) )[[1]];
        #           browser()
        dim <- nlevels(factor(subpd[,studyVarsArray[1]]))
        c( dim, NA, 1 );
        
      }
      layout <- reactive({
        
        if(input$rows==0||length(input$rows)==0||!input$custlayout){
          #           browser()
          cond <- input$cond
          cond_variables <- strsplit(split = "\\+", cond)[[1]]
          auto_layout(cond_variables)
        }else{
          #           c(this_columns(),input$rows,1)  
          c(NA,input$rows,1)  
        }
      })
      
      w_height <- reactive({
        this_height <- input$w_height
        if(this_height==0||length(this_height)==0||!input$custWinSize){
          this_height <- 400
        }
        this_height
      })
      get_w_height_stats <- function(){
#         browser()
        if (input$actPlotStats == 0)
          return(400)
        isolate({w_height()})
      }
      get_w_height_gate <- function(){
#         browser()
        if (input$actPlotGate == 0)
          return(400)
        isolate({w_height()})
      }
      # pdata output
     output$summary <- renderTable({
        
        to_display <- cur_pd()[,-1]
          
         to_display
     })
    output$stats_plot <- renderPlot({
     if (input$actPlotStats == 0)
       return()
#       browser()
     isolate({
       df <- cbind(cur_pd()[rownames(pop_stats_selected()),],pop_stats_selected())
        y_axis <- getNodes(gs_input()[[1]],isPath=TRUE)[as.integer(input$pops)]
        x_axis <- input$x_axis
        f1 <- paste("`",y_axis,"`~`",x_axis,"`",sep="")
        
       cond <- input$cond
#         browser()
        if(length(cond)>0&&cond!="name"&&nchar(cond)>0){
         f1 <- paste(f1,cond,sep="|")
        }
        
       f1 <- gsub("\\\\","\\\\\\\\",f1)
       f1 <- as.formula(f1)
#               browser()
        if(input$boxplot){
         print(bwplot(f1
                    ,data=df
                    ,scales=list(x=list(rot=45))
                    ,ylab="pop %"
                    ,layout=layout()
                    ,panel=function(...){
                      panel.bwplot(...)
                      panel.xyplot(...,jitter.x=TRUE)
                     }
                  )
                )
          
       }else{
         print(xyplot(as.formula(f1)
                ,data=df
                ,scales=list(x=list(rot=45))
                ,ylab="pop %"
                ,jitter.x=TRUE
                 ,layout=layout()))
         
       }
     }) 
    }
     ,height = get_w_height_stats
   )
 
    
  
      
  
  output$gate_plot <- renderPlot({
      xbin <- input$xbin
    if (input$actPlotGate == 0)
      return()
    
   isolate({
      xbin <- 64
      cur_data <-cur_pd()
     group_v <-  input$group
      pop_ind <- as.integer(input$pops)
      
      #     if(pops == "all")pops = NULL
#       smooth <- input$smooth
      stats <- input$stats
#       digits <- input$digits
      digits <- 2
     cond <- input$cond
     
      
      
#                                       browser()
#       check if there are multiple flowFrames per panel
#       cat FCS file name to cond if so
     cur_factors <- lapply(group_v,function(cur_group_v){factor(cur_data[,cur_group_v])})
     if(length(cur_factors) > 0){
       multi_frames <- unlist(by(cur_data,cur_factors,function(cur_df){
         nrow(cur_df) != 1
       }))
       if(any(multi_frames)){
         cond <- gsub("\\+",":",cond)
         cond <- paste(cond,"name",sep=":")
       }  
      }
      
#       browser()
      if(length(cond)==0||cond=="name"||nchar(cond)==0){
        cond <- NULL
     }
#       browser()
      overlay <-  input$overlay_pops
      if(length(overlay) == 0||!input$isOverlay){
        overlay <- NULL
      }else{
        overlay <- as.numeric(overlay)
      }
        
      
     
#         browser()
      print(
        plotGate(x = gs_input()
                 , y = pop_ind
                 , xbin = xbin
                 , stats = stats
                 , smooth = FALSE
                 , digits = digits
                 , cond = cond
                 , margin = TRUE
                 , bool = TRUE
                 , overlay = overlay
                 ,layout=layout()
        )
      )      
   })  
    
  }
  ,height = get_w_height_gate
  )
})
