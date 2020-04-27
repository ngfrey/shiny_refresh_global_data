shiny::shinyUI(fluidPage(    
  
  ## Give the page a title
  titlePanel("Testing Streaming with Files Directory and Small Files"),
  
  ## Generate a row with a sidebar
  sidebarLayout(      
    
    ## Define the sidebar with one input
    sidebarPanel(
      actionButton(inputId = 'newdata',
                   label = 'Generate new data'), 
      actionButton(inputId = "write_fake_data", 
                   label = "Write New Sample Data"),
      actionButton(inputId = "reset_fake_data", 
                   label = "Reset Data Directory")
    ),
    
    ## Create a spot for the barplot
    mainPanel(
      #plotOutput("plot1"),
      highcharter::highchartOutput(outputId = "hc_stream", width = "100%", height = "450px"),
      shiny::verbatimTextOutput(outputId = "values_summary"),
      DT::dataTableOutput(outputId = "values_df")
    )
    
  )
))