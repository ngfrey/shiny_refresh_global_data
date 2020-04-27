

shiny::observeEvent(current_time$time, priority = 0, ignoreNULL = TRUE, ignoreInit = TRUE, {
  the_current_time<- as.numeric(current_time$time)
  the_last_time_updated<- as.numeric(format(last_time_updated_rv$last_time_updated))
  abs_difference<- base::abs(the_current_time - the_last_time_updated)
  if(abs_difference >= (update_time_interval/1000) ){
    updateData(current_time =current_time$time)
    last_time_updated_rv$last_time_updated<-current_time$time
  }
  #last_time_updated_rv$last_time_updated<-as.numeric(format(as.numeric(Sys.time())))
  

})

# shiny::observe(priority = 0, {
#   # Re-execute this reactive expression after 15000 milliseconds
#   shiny::invalidateLater(millis = update_time_interval)
#   
#   the_current_time<- shiny::isolate(as.numeric(current_time$time))
#   the_last_time_updated<- shiny::isolate(as.numeric(format(last_time_updated_rv$last_time_updated)))
#   abs_difference<- shiny::isolate(abs(the_current_time - the_last_time_updated))
#   shiny::isolate(if(abs_difference >= (update_time_interval/1000) ){
#     updateData(current_time =current_time$time)
#   })
# }) 

# Define the required logic
shiny::shinyServer(function(input, output, session) {
  
  # current_time<-shiny::reactiveValues()
  # current_time$time<- as.numeric(format(as.numeric(Sys.time()))) # reactive Values List for the current time

  
  output$values_summary<- shiny::renderText({
    tmp<- base::nrow(values$df)
    tmp
  })
  
  output$values_df<- DT::renderDataTable(values$df)
  
  observeEvent(write_fake_data_reactive_debounce(), {
    cat("Writing more fake data to simulate stream...", "\n")
    new_data_list()
  })
  
  write_fake_data_reactive<- shiny::reactive(input$write_fake_data)
  
  write_fake_data_reactive_debounce<- write_fake_data_reactive %>% shiny::debounce(1000)
  
  
  observeEvent(input$reset_fake_data, ignoreNULL = TRUE, ignoreInit = TRUE, {
    reset_data_list()
  })
  
  output$hc_stream<- highcharter::renderHighchart({
    tmp<- values$df %>% dplyr::group_by(time) %>%
      dplyr::summarise(the_mean=mean(b)*100) %>% 
      base::as.data.frame()
    colnames(tmp)<- c("Index", "Value")
    tmp$change<- c(0,diff(tmp$Value))
    
    the_variable_name<- "Value"
    the_pretty_var_name<- the_variable_name
    
    hc<- highcharter::highchart(type = "stock") %>%
      #highcharter::hc_title(text=paste0(the_offering_name))  %>%
      hc_yAxis_multiples(
        create_yaxis(2, height = c(3, 1), turnopposite = TRUE)
      ) %>%
      highcharter::hc_add_series(data=tmp, type = "line", lineWidth=3,eval(parse(text=paste0("highcharter::hcaes(x=Index,y=", the_variable_name, ")"))),
                                 yaxis=0, name=paste0("Total ", the_pretty_var_name),
                                 tooltip=list( pointFormat=paste0('Total ',the_pretty_var_name,': {point.',the_variable_name,':,.0f}'))) %>%
      highcharter::hc_add_series(data = tmp, type="column", highcharter::hcaes(x=Index, y= change),
                                 yAxis=1, name="Daily Change",
                                 tooltip=list(pointFormat='Index: {point.Index} <br> Change: {point.change:,.0f}')) %>%
      highcharter::hc_colors(cols)  %>%
      highcharter::hc_exporting(enabled=TRUE)
    hc
  })
  
  # #Setup observer to automatically update data (if new data is present) every 15 seconds
  # #Note: this will only allow the first user session to update the data at the given interval.
  # #Once that session ends, the session with the next closest difference to the first session's interval takes over.
  # shiny::observe({
  #   # Re-execute this reactive expression after 1000 milliseconds
  #   invalidateLater(update_time_interval, session)
  #   #new_data_list()
  #   #as.numeric(format(as.numeric(Sys.time()))) - max(as.numeric(as.character(sapply(strsplit(base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE )), split = "_"), `[[`, 7))))
  #   isolate(if(abs(as.numeric(current_time$time) - as.numeric(format(last_time_updated_rv$last_time_updated))) >= (update_time_interval/1000)){
  #     cat("Automatically checking for data updates...")
  #     the_time<- as.numeric(format(as.numeric(Sys.time())))
  #     current_time$time<- the_time
  #     updateData(current_time =the_time)
  # 
  #   })
  # 
  # }) # with observe
  


})