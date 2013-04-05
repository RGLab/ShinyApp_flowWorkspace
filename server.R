# library(googleVis)
library(flowWorkspace)
library(flowIncubator)
path <- ("/home/wjiang2/rglab/workspace/ShinyApp_flowWorkspace_devel")
#pre-load gatingset,pdata and stats
# gs_HVTN_small <- load_gs(path=file.path(path,"HVTN_small"))
# pd_HVTN_small <-pData(gs_HVTN_small)
# stats_HVTN_small <- getPopStats(gs_HVTN_small)

gs_HVTN <- load_gs(path=file.path(path,"HVTN"))
pd_HVTN <-pData(gs_HVTN)
stats_HVTN <- getPopStats(gs_HVTN)


gs_RV144 <- load_gs(path=file.path(path,"RV144"))
pd_RV144 <- pData(gs_RV144)
stats_RV144 <- getPopStats(gs_RV144)



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
        if(this_study == "HVTN"){
          gs_HVTN
        }else if(this_study == "RV144"){
          gs_RV144
        }else{
          stop("not valid study!")
        }
      })
      pd_preloaded <- reactive({
        this_study <- study_selected()
        if(this_study == "HVTN"){
          pd_HVTN
        }else if(this_study == "RV144"){
          pd_RV144
        }else{
          stop("not valid study!")
        }
      })
      stats_preloaded <- reactive({
        this_study <- study_selected()
        if(this_study == "HVTN"){
          stats_HVTN
        }else if(this_study == "RV144"){
          stats_RV144
        }else{
          stop("not valid study!")
        }
      })
      
      output$gh_plot <- renderPlot({
        plot(gs_preloaded()[[1]] )
      })
      output$FilterControls <- renderUI({
            this_pd <- pd_preloaded()
            study_variables <- colnames(this_pd)
            name_ind <- match("name",study_variables)
            study_variables <- study_variables[-name_ind]
            lapply(study_variables,function(this_variable){
              this_choices <- unique(as.character(this_pd[,this_variable]))
              selectInput(this_variable, this_variable, 
                          choices = this_choices
                          ,selected = this_choices[1]
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
        bool_ind <- unlist(lapply(pop_ind,function(cur_ind){flowWorkspace:::.isBoolGate(gh,cur_ind)}))
        pop_ind[!bool_ind]
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
        selectInput("group", "Group by:", 
                     choices = colnames(pd_preloaded())
                     ,selected = "VISITNO"
                     ,multiple = TRUE
        )
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
#           group_v <- unlist(lapply(group_v,function(cur_group){paste("factor(",cur_group,")",sep="")}))
          cond_type <- ifelse(input$oneLevel,":","+")
          group_v <- paste(group_v,collapse=cond_type)
          textInput("cond", "group selected:", value = group_v)  
        })
      
      selected_samples <- reactive({
        this_samples <- as.character(subset(pd_preloaded()
               , PTID%in%input$PTID&Stim%in%input$Stim&VISITNO%in%input$VISITNO)$name
          )
          if(length(this_samples) == 0)this_samples = 1
            this_samples
      })
      
      cur_pd <- reactive({
#         browser()
        pd_preloaded()[selected_samples(),]
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
      
      layout <- reactive({
#         browser()
        
        if(input$rows==0||length(input$rows)==0||!input$custlayout){
          NULL
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
        
#         if (input$actSummary == 0)
#           return()
# 
#         isolate({
          to_display <- cur_pd()
          
          to_display
          #              gvisTable(to_display,list(page="disable"),chartid="name")  
#         })      
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
#       ,width = get_w_width() 
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
      #check if there are multiple flowFrames per panel
      #cat FCS file name to cond if so
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
