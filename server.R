library(googleVis)
shinyServer(function(input, output) {
#   browser()
      study_selected <- reactive({
        input$study  
      })
      
      output$titleCntrol <- renderUI({
        
        headerPanel(study_selected(),"flowWorkspace & flowViz")
      })
      
      gs_selected <- reactive({
        this_study <- study_selected()
        if(this_study == "HVTN-080-small"){
          gs_small
        }else if(this_study == "HVTN-080-big"){
          gs_big
        }else{
          stop("not valid study!")
        }
      })
      
      pd_selected <- reactive({
        pData(gs_selected())
      })
      
      output$PTIDCntrol <- renderUI({
    #     browser()
        PTID <- unique(as.character(pd_selected()$PTID))
        selectInput("PTID", "Subjects:", 
                    choices = PTID
                    ,selected = PTID[1]
                    ,multiple = TRUE
        )
      })
      
      
      output$visitCntrol <- renderUI({
        VISITNO <- unique(as.character(pd_selected()$VISITNO))
        selectInput("VISITNO", "Visits:", 
                     choices = VISITNO
                     ,selected = VISITNO[1]
                     ,multiple = TRUE
        )
      })
      output$stimCntrol <- renderUI({
        
        Stim <- unique(as.character(pd_selected()$Stim))
        
        selectInput("Stim", "Stimulation:", 
                     choices = Stim
                     ,selected = Stim
                     ,multiple = TRUE
        )
      })
      
      output$popCntrol <- renderUI({
        populations <- getNodes(gs_selected()[[1]],isPath=TRUE)
        pop_ind <- 1:length(populations)
        names(pop_ind) <- populations
        selectInput("pops", "Populations:", 
                      choices = pop_ind[-1]
                      ,selected = names(pop_ind[74])
                      ,multiple = FALSE)
      })
      
      output$groupCntrol <- renderUI({
        selectInput("group", "Group by:", 
                     choices = colnames(pd_selected())
                     ,selected = "VISITNO"
                     ,multiple = TRUE
        )
      })
      output$axisCntrol <- renderUI({
    
        selectInput("x_axis", "X-axis", 
                    choices = colnames(pd_selected())
                    ,selected = "Stim"
                    ,multiple = FALSE)
      })
      
      output$condCntrol <- renderUI({
#         browser()
          group_v <-  input$group
          group_v <- unlist(lapply(group_v,function(cur_group){paste("factor(",cur_group,")",sep="")}))
          cond_type <- ifelse(input$oneLevel,":","+")
          group_v <- paste(group_v,collapse=cond_type)
          textInput("cond", "group selected:", value = group_v)  
        })
      
      
      # Reactive expression
       gs_input <- reactive({
    #      browser()
         selected_samples <- as.character(
           subset(pd_selected()
                  , PTID%in%input$PTID&Stim%in%input$Stim&VISITNO%in%input$VISITNO)$name
         )
         if(length(selected_samples) == 0)selected_samples = 1
         gs_selected()[selected_samples]
    
      })
        
      pop_stats <- reactive({
        pop_ind <- as.integer(input$pops)
        p_stat <- getPopStats(gs_input())[pop_ind,,drop=FALSE]
        t(p_stat)
      })
        
      cur_pd <- reactive({
    #     browser()
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
      nSamples <- reactive({
        nrow(cur_pd())
      })
      
      output$rowsControl <- renderUI({
          #reset rows when unchecked
            numericInput("rows","rows:",value=0,min=0)    
      })
      this_columns <- reactive({
        this_row <- input$rows
        if(this_row == 0){
          0
        }else{
          ceiling(nSamples()/input$rows) 
        }
        
      })
      
      output$columnsControl <- renderUI({
#             browser()
            helpText(paste("columns",this_columns(),sep=":"))
      })
      layout <- reactive({
#         browser()
        
        if(input$rows==0||length(input$rows)==0||!input$custlayout){
          NULL
        }else{
          c(this_columns(),input$rows,1)  
        }
      })     
    output$stats_plot <- renderPlot({
        df <- cbind(cur_pd()[rownames(pop_stats()),],pop_stats())
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
                 ,layout=layout()))
          
        }else{
          print(xyplot(as.formula(f1)
                 ,data=df
                 ,scales=list(x=list(rot=45))
                 ,ylab="pop %"
                 ,jitter.x=TRUE
                  ,layout=layout()))
          
        }
      
    })
 
    
#   
#       
  
  output$gate_plot <- renderPlot({
#       xbin <- input$xbin
    if (input$actPlot == 0)
      return()
    
   isolate({
      xbin <- 64
      cur_data <-cur_pd()
      group_v <-  input$group
      pop_ind <- as.integer(input$pops)
      
      #     if(pops == "all")pops = NULL
      smooth <- input$smooth
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
      
     
#         browser()
      print(
        plotGate(x = gs_input()
                 , y = pop_ind
                 , xbin = xbin
                 , stats = stats
                 , smooth = smooth
                 , digits = digits
                 , cond = cond
                 , margin = input$margin 
                 , bool = TRUE
                 ,layout=layout()
        )
      )      
   })  
    
  })
})
