library(shiny)
library(plotly)
library(quantmod)
library(DT)
library(D3TableFilter)
library(reshape)
# data(mtcars)


# numericInput3<-function (inputId, label, value = "",...) 
# {
#   div(style="display:inline-block",
#     tags$label(label, `for` = inputId), 
#     tags$input(id = inputId, type = "numeric", value = value,...))
# }

shinyUI(fluidPage(
  titlePanel("stockVis"),

  sidebarLayout(
    sidebarPanel(
      

      
        # numericInput(inputId="num1", label="Left limit", value = 4.09,step=0.0001,width='50%'),
        # numericInput(inputId="num2", label="Right limit", value = 4.035,step=0.0001,width='50%'),
        # 
      actionButton("save_results", label = "Save results"),
      
      actionButton("action", label = "Action"),
      selectInput("select", label = h3("Select box"), 
        choices = select_options, 
        selected = 1),
      
    DT::dataTableOutput('x1'),
      d3tfOutput('mtcars'),
      d3tfOutput('mtcars2')
      
    ),
    

    mainPanel(plotlyOutput("plot")))
  
))
