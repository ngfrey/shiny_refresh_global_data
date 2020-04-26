
#shiny::observe()

# Define the required logic
shiny::shinyServer(function(input, output, session) {
  
  current_time<-reactiveValues()
  current_time$time<- as.numeric(format(as.numeric(Sys.time()))) # reactive Values List for the current time
  #cat("Current Time Server=", str(reactiveValuesToList(current_time)), "\n")
  # tmp<- shiny::reactiveValuesToList(current_time)
  # tmp<- tmp$time
  # cat("Current Time Server:", tmp, "\n")
  # observeEvent(current_time$time, {
  #   cat("Current Time Server:", current_time$time, "\n")
  # })
  
  
  output$values_summary<- shiny::renderText({
    tmp<- base::nrow(values$df)
    tmp
  })
  
  output$values_df<- DT::renderDataTable(values$df)
  
  observeEvent(input$write_fake_data, {
    cat("Writing more fake data to simulate stream...", "\n")
    new_data_list()
  })
  
  observeEvent(input$reset_fake_data, ignoreNULL = TRUE, ignoreInit = TRUE, {
    reset_data_list()
  })
  
  # Setup observer to automatically update data (if new data is present) every 10 seconds
  # Note: this will only allow the first user session to update the data at the given interval. 
  # Once that session ends, the session with the next closest difference to the first session's interval takes over.
  shiny::observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(update_time_interval, session)
    #new_data_list()
    #as.numeric(format(as.numeric(Sys.time()))) - max(as.numeric(as.character(sapply(strsplit(base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE )), split = "_"), `[[`, 7))))
    isolate(if(abs(as.numeric(current_time$time) - as.numeric(format(last_time_updated_rv$last_time_updated))) >= (update_time_interval/1000)){
      cat("Automatically checking for data updates...")
      the_time<- as.numeric(format(as.numeric(Sys.time())))
      current_time$time<- the_time
      updateData(current_time =the_time)
      
    })
    
    
    
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    #print(paste("The value of input$n is", isolate(input$n)))

  })
  


})