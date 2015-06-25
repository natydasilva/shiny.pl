library(shiny)


shinyUI(fluidPage(
  
  
  titlePanel("Projection Pursuit Plots"),
  fluidRow(
  sidebarLayout(
    sidebarPanel(width=3,
          conditionalPanel(condition="input.conditionedPanels==1",
                 numericInput("meanx1", "Pop. 1 mean x1", -1),
                 numericInput("meanx2", "Pop. 1 mean x1", .6),
                 numericInput("meany1", "Pop. 2 mean y1", 1),
                 numericInput("meany2", "Pop. 2 mean y2", -.6),
                 
                 sliderInput("cor",  "Correlation",value = .95,min = -1, max = 1,step=.01),
                 
                 
                 submitButton("Update View")
          ), #cond panel 1
          
          conditionalPanel(condition="input.conditionedPanels==2",
                           checkboxGroupInput(inputId = "var",
                                              label = "Select class:",
                                              choices = levels(crab$Type) )
                           #, selected=c("BlueMale","BlueFemale","OrangeMale")
                           
                           
                           
                           
          ) #cond panel 2
    ),
    
      
    mainPanel(
      tabsetPanel(
        tabPanel("DATA",plotOutput('plot1'), value=1),
        tabPanel("LDA index",plotOutput("plot3"),plotOutput("plot4"),plotOutput("plot2"), value=1),
        tabPanel("Votes",plotOutput("plot5"), value=2),
        id="conditionedPanels")
      
      # tableOutput("table1")
     
    )
  )#sidebar
  )#fluid page
))