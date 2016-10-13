library(shiny)
library(plotly)
library(quantmod)
library(DT)
library(D3TableFilter)
library(reshape)

shinyUI(fluidPage(
  titlePanel("stockVis"),

  sidebarLayout(
    sidebarPanel(
      

      
        # numericInput(inputId="num1", label="Left limit", value = 4.09,step=0.0001,width='50%'),
        # numericInput(inputId="num2", label="Right limit", value = 4.035,step=0.0001,width='50%'),
        # 
      actionButton("save_results", label = "Save results"),
      actionButton("save_profile", label = "Save profile"),
      actionButton("autorun", label = "autorun"),
      
      actionButton("action", label = "Action"),
      selectInput("select", label = h3("Select box"), 
        choices = select_options, 
        selected = 1),
      
      # D3TableFilter::d3tfOutput('x1'),
      DT::dataTableOutput('x1'),
      D3TableFilter::d3tfOutput('mtcars'),
      D3TableFilter::d3tfOutput('mtcars2'),
      D3TableFilter::d3tfOutput('mtcars3')
      
      
    ),
    

    mainPanel(plotlyOutput("plot")))
  
))
