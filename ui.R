##/loc/no-backup/remote_fred_hvtn/RV144/CD4\ Cytokine\
##100 subjects 20 placebo (need to get study variables csv)
##query by and condition on study variables and plot sepcific populations


shinyUI(pageWithSidebar(
    
  # Application title
  uiOutput("titleCntrol")  
  ,sidebarPanel(
#     uiOutput("submitCntrol")  
#     h4("Filters:")
    
    selectInput("study", "Studies:", 
                 choices = c("HVTN-080-small")
                 ,selected = "HVTN-080-small"
    )
    ,uiOutput("PTIDCntrol")  
    ,uiOutput("visitCntrol")
    ,uiOutput("stimCntrol")  
    ,uiOutput("popCntrol")  
    
    ,uiOutput("groupCntrol")  
    
    ,checkboxInput("oneLevel", "Convert to one level:"
                   ,value = FALSE)
    , uiOutput("condCntrol")
    
    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("summary"
               , htmlOutput("summary")
               )
      ,tabPanel("Stats"
                ,checkboxInput("boxplot","boxplot",value=FALSE)
                ,uiOutput("axisCntrol")
               , plotOutput("stats_plot")
               )
      ,tabPanel("Gates"
               , checkboxInput("stats", "show proportions", value = TRUE)
               , checkboxInput("smooth", "smooth", value = FALSE)
               , checkboxInput("margin", "margin", value = FALSE)
               , plotOutput("gate_plot")
            )
    )
  )
))
