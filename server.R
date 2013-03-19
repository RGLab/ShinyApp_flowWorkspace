library(googleVis)
shinyServer(function(input, output) {
#   browser()
  
#     output$submitCntrol <- renderUI({
#       if(input$plotType == "Stats"){
#         helpText("")  
#       }else{
#         submitButton("update gate")
#       }
#       
#       
#     })
    output$condCntrol <- renderUI({
      group_v <-  input$group
      cond_type <- ifelse(input$oneLevel,":","+")
      group_v <- paste(group_v,collapse=cond_type)
      textInput("cond", "group selected:", value = group_v)  
    })
  
  
  # Reactive expression
   gs_input <- reactive({
     selected_samples <- as.character(
       subset(pd
              , PTID%in%input$PTID&Stim%in%input$Stim&VISITNO%in%input$VISITNO)$name
     )
     
     gs[selected_samples]

  })
    
  pop_stats <- reactive({
    pop_ind <- as.integer(input$pops)
    p_stat <- getPopStats(gs_input())[pop_ind,,drop=FALSE]
    t(p_stat)
  })
    
  cur_pd <- reactive({
    pData(gs_input())
  })
    
  # pdata output
  output$summary <- renderGvis({
#     browser()
      to_display <- cur_pd()
#       to_display <- cbind(to_display,pop_stats())
#       to_display$TESTDT <- NULL
       gvisTable(to_display,list(page="disable"),chartid="name")  
  })
  #gate plot
  gate_plot <- reactive({
        xbin <- input$xbin
        cur_data <-cur_pd()
        group_v <-  input$group
        pop_ind <- as.integer(input$pops)
        
    #     if(pops == "all")pops = NULL
        smooth <- input$smooth
        stats <- input$stats
        digits <- input$digits
        cond <- input$cond
#                                 layout <- c(input$col,input$row,1)
#                                 browser()
        #check if there are multiple flowFrames per panel
        #cat FCS file name to cond if so
        cur_factors <- lapply(group_v,function(cur_group_v){factor(cur_data[,cur_group_v])})
        multi_frames <- unlist(by(cur_data,cur_factors,function(cur_df){
          nrow(cur_df) != 1
        }))
        if(any(multi_frames)){
          cond <- gsub("\\+",":",cond)
          cond <- paste(cond,"name",sep=":")
        }
       plotGate(x = gs_input()
                         , y = pop_ind
                         , xbin = xbin
                         , stats = stats
                         , smooth = smooth
                         , digits = digits
                         , cond = cond
                         , margin = input$margin 
    #                                              , layout = layout
                        , bool = TRUE
                        )
      
                                
    })
    
    stat_plot <- reactive({
      df <- cbind(cur_pd()[rownames(pop_stats()),],pop_stats())
      y_axis <- getNodes(gs_input()[[1]],isPath=TRUE)[as.integer(input$pops)]
      x_axis <- input$x_axis
      f1 <- paste("`",y_axis,"`~`",x_axis,"`",sep="")
      
      cond <- input$cond
      if(cond!="name"){
        f1 <- paste(f1,cond,sep="|")
      }
      #       browser()
      f1 <- gsub("\\\\","\\\\\\\\",f1)
      f1 <- as.formula(f1)
      if(input$boxplot){
        bwplot(f1
               ,data=df
               ,scales=list(x=list(rot=45))
               ,ylab="pop %")
        
      }else{
        xyplot(as.formula(f1)
               ,data=df
               ,scales=list(x=list(rot=45))
               ,ylab="pop %")
         
      }
    })
      
    
    
    output$plot <- renderPlot({
      plotType <- input$plotType
#       browser ()
      if (plotType == "Stats"){
        print(stat_plot())
      }else
      {
        print(gate_plot())
      }
      
      
    })
  
})
