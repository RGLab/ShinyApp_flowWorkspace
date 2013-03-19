##/loc/no-backup/remote_fred_hvtn/RV144/CD4\ Cytokine\
##100 subjects 20 placebo (need to get study variables csv)
##query by and condition on study variables and plot sepcific populations





shinyUI(pageWithSidebar(
    
  # Application title
  uiOutput("titleCntrol")  
  ,sidebarPanel(
#     uiOutput("submitCntrol")  
    h4("Filters:")
    ,selectInput("study", "Studies:", 
                  choices = c("HVTN-080-small","HVTN-080-big")
                  ,selected = "HVTN-080-small"
    )
    ,uiOutput("PTIDCntrol")  
    ,uiOutput("visitCntrol")
    ,uiOutput("stimCntrol")  
    ,uiOutput("popCntrol")  
  
    
    ,h4("Grouping:")
    ,uiOutput("groupCntrol")  
   
    ,checkboxInput("oneLevel", "Convert to one level:"
                  ,value = FALSE)
   , uiOutput("condCntrol")
    
    ,h4("plot Settings:")  
    ,radioButtons("plotType",label = "Plot type",choices = c("Stats","Gate"),selected= "Stats")
    
    #stats setting
    , conditionalPanel(
              condition = "input.plotType == 'Stats'"
              ,checkboxInput("boxplot","boxplot",value=TRUE)
              , selectInput("x_axis", "X-axis", 
                            choices = colnames(pd)
                            ,selected = "Stim"
                            ,multiple = FALSE)
      )

  
   
    #gate setting
    , conditionalPanel(
          condition = "input.plotType == 'Gate'"
        , checkboxInput("stats", "show %", value = TRUE)
        , conditionalPanel(
                          condition = "input.stats == true",
                          sliderInput("digits" 
                                      , "digits:" 
                                      , value = 3
                                      , step= 1
                                      , min = 2
                                      , max = 6
                                     )
                        )
#           
#           ##smooth args
        , checkboxInput("smooth", "smooth", value = TRUE)
        , conditionalPanel(
          condition = "input.smooth == false",
          sliderInput("xbin", 
                      "significant digits:" 
                      , value = 64
                      , step= 64
                      , min = 64
                      , max = 256
                      
                    )
        )
        , checkboxInput("margin", "margin", value = TRUE)
         
    )
    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("summary", htmlOutput("summary")), 
      tabPanel("plot", plotOutput("plot"))
    )
  )
))
