library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Statistical Process Control For Continuos Variables"),
  
  sidebarLayout(
    
    sidebarPanel( 
      h3("Specifications:"),
      numericInput("nominal",label="Insert the nominal value of specification",value=2),
      br(),
      numericInput("LSL",label="Insert the lower specification limit",value=1),
      br(),
      numericInput("USL",label="Insert the upper specification limit",value=3),
      br(),
      h3("Settings for the control chart"),
      selectInput("type",label="Select the type of chart:",choices=list("(X,R) Chart"="xr",
                  "(X,S) Chart"="xs", "IMR Chart"="imr"),selected="xr"),
      fileInput("dataset",label="Upload your file with the samples:")
      ),
    
    mainPanel(tableOutput("message"),
              tableOutput("stat"),
              plotOutput("chart"))
              
    )
  )
          
        )