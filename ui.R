##/loc/no-backup/remote_fred_hvtn/RV144/CD4\ Cytokine\
##100 subjects 20 placebo (need to get study variables csv)
##query by and condition on study variables and plot sepcific populations
library(shinyIncubator)

shinyUI(pageWithSidebar(
    
  # Application title
  uiOutput("titleCntrol")  
  ,sidebarPanel(
    
#     h4("Filters:")
    
    selectInput("study", "Studies:", 
                 choices = c("HVTN","RV144")
                 ,selected = "HVTN"
              )
      ,uiOutput("FilterControls")
    ,uiOutput("popCntrol")  
    
    
    ,uiOutput("groupCntrol")  
    
    ,checkboxInput("oneLevel", "Convert to one level:"
                   ,value = FALSE)
    , uiOutput("condCntrol")
     , checkboxInput("custlayout", "Custom grid layout", value = FALSE)
    ,conditionalPanel(condition = "input.custlayout == true"
                      ,uiOutput("rowsControl")
                      ,uiOutput("columnsControl")
                    )
    , checkboxInput("custWinSize", "Custom plot size", value = FALSE)
    ,conditionalPanel(condition = "input.custWinSize == true"
#                       ,uiOutput("widthControl")
                      ,uiOutput("heightControl")
                    )
    
      
    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Summary"
#                ,actionButton("actSummary","update")  
               , htmlOutput("summary")
               )
      ,tabPanel("Gate Hierarchy"
                , plotOutput("gh_plot")
      )
      ,tabPanel("Stats"
                ,checkboxInput("boxplot","boxplot",value=TRUE)
                ,uiOutput("axisCntrol")
                ,actionButton("actPlotStats","plot")  
               , plotOutput("stats_plot"
                            ,height = "auto"
                            )
               )
      ,tabPanel("Gates"
               , checkboxInput("stats", "show proportions", value = TRUE)
                , checkboxInput("isOverlay", "Overlay", value = FALSE)
                ,conditionalPanel(condition="input.isOverlay == true"
                                  ,uiOutput("overlayPopCntrol")  
                                    )
              ,actionButton("actPlotGate","plot")  
               , plotOutput("gate_plot")
            )
    )
  )
))
