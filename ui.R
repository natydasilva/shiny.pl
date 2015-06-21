library(shiny)


shinyUI(fluidPage(
  
  
  titlePanel("Projection Pursuit Plots"),
  
  sidebarLayout(
    sidebarPanel(width=3,
                 numericInput("meanx1", "Pop. 1 mean x1", -1),
                 numericInput("meanx2", "Pop. 1 mean x1", .6),
                 numericInput("meany1", "Pop. 2 mean y1", 1),
                 numericInput("meany2", "Pop. 2 mean y2", -.6),
                 
                 sliderInput("cor",  "Correlation",value = .95,min = -1, max = 1,step=.01),
                 
                 
                 submitButton("Update View")
    ),
    
      
    mainPanel(
      tabsetPanel(
        tabPanel("DATA",plotOutput('plot1')),
        tabPanel("LDA index",plotOutput("plot3"),plotOutput("plot4"),plotOutput("plot2")) 
       )
      
      # tableOutput("table1")
     
    )
  )
))