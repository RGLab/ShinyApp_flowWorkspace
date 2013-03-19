##/loc/no-backup/remote_fred_hvtn/RV144/CD4\ Cytokine\
##100 subjects 20 placebo (need to get study variables csv)
##query by and condition on study variables and plot sepcific populations

PTID <- unique(as.character(pd$PTID))
Stim <- unique(as.character(pd$Stim))
VISITNO <- unique(as.character(pd$VISITNO))
populations <- getNodes(gs[[1]],isPath=TRUE)
pop_ind <- 1:length(populations)
names(pop_ind) <- populations

shinyUI(pageWithSidebar(
    
  # Application title
  headerPanel("flowWorkspace & flowViz"),
  
  sidebarPanel(
#     submitButton("update")
    h4("Filters:")
    ,selectInput("PTID", "Subjects:", 
                choices = PTID
                ,selected = PTID[1]
                ,multiple = TRUE
    )
    ,selectInput("VISITNO", "Visits:", 
                choices = VISITNO
                ,selected = VISITNO
                ,multiple = TRUE
    )
    ,selectInput("Stim", "Stimulation:", 
                choices = Stim
                ,selected = Stim
                ,multiple = TRUE
    )

    , selectInput("pops", "Populations:", 
                  choices = pop_ind[-1]
                  ,selected = names(pop_ind[74])
                  ,multiple = FALSE)
    
    ,h4("Grouping:")
    ,selectInput("group", "Group by:", 
                 choices = colnames(pd)
                 ,selected = "VISITNO"
                 ,multiple = TRUE
    )
    ,checkboxInput("oneLevel", "Convert to one level:"
                  ,value = FALSE)
   , uiOutput("condCntrol")
    
    ,h4("Stats plot Settings:")  
    ,checkboxInput("boxplot","boxplot",value=TRUE)
    , selectInput("x_axis", "X-axis", 
                  choices = colnames(pd)
                  ,selected = "Stim"
                  ,multiple = FALSE) 
    
  
   
    ,h4("Gate Plot Settings:")  
    # stats args
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
      
      ##smooth args
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
    
    
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("summary", htmlOutput("summary")), 
      tabPanel("stats plot", plotOutput("plot")), 
#       tabPanel("Stats table", tableOutput("Statistics")),
      tabPanel("Gates", plotOutput("Gates"))
      
    )
  )
))
