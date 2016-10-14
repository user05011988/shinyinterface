library(shiny)
library(plotly)
library(quantmod)
library(DT)
library(D3TableFilter)
library(reshape)

shinyUI(fluidPage(
  title = 'Interactive features',
  tabsetPanel(
    tabPanel("Editing and filtering",

  sidebarLayout(
    sidebarPanel(
      
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
    ),
  tabPanel("Row selection",
    fluidRow(column(width = 12, h4("Row selection"))),
    fluidRow(
      column(width = 12,
        d3tfOutput('fit_error')
      )
    
  )
  
),
    tabPanel("Selection",
      fluidRow(column(width = 12, h4("Selection"))),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("quant_selection")
          )
        
      )
      
    ),
    
    tabPanel("p_value",
  fluidRow(column(width = 12, h4("Row selection"))),
  fluidRow(
    column(width = 12,
      mainPanel(plotlyOutput("plot_p_value")))    ),
  fluidRow(
    column(width = 12,
      mainPanel(DT::dataTableOutput("p_value_final")))    ),

  fluidRow(
    column(width = 12,
      mainPanel(plotlyOutput("plot_p_value_2")))    )
    
  )
  


    
    )))
