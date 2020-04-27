cat("**************** STARTING NEW GLOBAL SESSION *****************", "\n")
options(scipen = 999)

maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages<- c("shiny", "shinydashboard", "shinydashboardPlus", "data.table", "magrittr","highcharter",
            "dplyr", "tidyr","stringr","stringi", "ggplot2", "future",  "httr", "visNetwork", "future")


suppressPackageStartupMessages(ipak(packages))
future::plan(multisession)

# Set some options for highcharter to make the plots easier to read
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep<- ","
options(highcharter.lang = hcoptslang)

# Allow supression of warnings in the magrittr pipe chain operations
new_is_pipe = function (pipe)
{
  identical(pipe, quote(`%>%`)) || identical(pipe, quote(`%T>%`)) ||
    identical(pipe, quote(`%W>%`)) ||
    identical(pipe, quote(`%<>%`)) || identical(pipe, quote(`%$%`))
}
assignInNamespace("is_pipe", new_is_pipe, ns="magrittr", pos="package:magrittr")
`%W>%` = magrittr::`%>%`

is_W = function(pipe) identical(pipe, quote(`%W>%`))
environment(is_W) = asNamespace('magrittr')

new_wrap_function = function (body, pipe, env)
{
  w <- options()$warn
  if (magrittr:::is_tee(pipe)) {
    body <- call("{", body, quote(.))
  }
  else if (magrittr:::is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }
  else if (is_W(pipe)) {
    body <- as.call(c(as.name("{"), expression(options(warn=-1)), parse(text=paste0('on.exit(options(warn=', w, '))')), body))
  }
  eval(call("function", as.pairlist(alist(. = )), body), env, env)
}
assignInNamespace("wrap_function", new_wrap_function, ns="magrittr", pos="package:magrittr")


######### Functions ##########

lu<- function(x){length(unique(x))}

'%!in%' <- function(x,y)!('%in%'(x,y))


####################### START OF APP SPECIFIC GLOBAL CODE HERE ##########################

# Also borrowing a method from: https://stackoverflow.com/questions/14902188/update-a-data-frame-in-shiny-server-r-without-restarting-the-app

## Setting up an update interval, say once every 15 seconds
update_time_interval<-15
update_time_interval<- update_time_interval * 1000 # milliseconds for 'invalidateLater' function in server.R
cat("Update Time Interval is:", update_time_interval, "\n")


# Sample, setup the fake 'large' data set to read in
all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))

## Make a variable to prevent mass data refresh calls if we have multiple sessions up and running
last_time_checked<- max(as.numeric(as.character(sapply(strsplit(all_files, split = "_"), `[[`, 7))))
#last_time_updated<- last_time_updated[length(last_time_updated)-1] # the actual last time it was updated
last_time_updated_rv<- reactiveValues(last_time_updated=as.numeric(format(as.numeric(last_time_updated))))


current_time<-shiny::reactiveValues()
current_time$time<- as.numeric(format(as.numeric(Sys.time()))) # reactive Values List for the current time


# An observer to update the current time, runs 2x for every single run of the update Data observer
shiny::observe(priority = 1, {
  shiny::invalidateLater(millis = (update_time_interval/2))
  current_time$time<- as.numeric(format(as.numeric(Sys.time())))
})


df<- lapply(seq_along(all_files), function(x){
  base::readRDS(file = all_files[x]) 
}) %>% do.call(rbind, .)
# update all files read AFTER successfully readin in the data
all_files_read<- all_files
all_files_read_rv<- reactiveValues()
all_files_read_rv$files_read<- all_files_read

# Now make a function to make some fake data and add it to the tmp_data_list directory (simulate streaming)
new_data_list<- function(){
  time<- base::as.numeric(Sys.time())
  d<- data.frame(a=1:10, b=rnorm(10), time=as.numeric(format(time))*1000)
  saveRDS(d,file=paste0('./tmp_data_list/data_',format(time),'_.rds'))
}
new_data_list() # Add some data to the app from the start

reset_data_list<- function(){
  all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))
  first_file<-all_files[1]
  delete_these<- setdiff(all_files, first_file)
  if(length(delete_these)>=1){
    base::unlink(delete_these)
  }
}
#reset_data_list()
#as.numeric(format(as.numeric(Sys.time()))) - max(as.numeric(as.character(sapply(strsplit(base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE )), split = "_"), `[[`, 7))))
harvard_colors<- c("#FD9F25", "#C21F1F", "#1A8F1A")
hc_green<- "#21908C"
cols<- c(harvard_colors[x], hc_green)



values <- shiny::reactiveValues(df=df)

# Trying this without making any changes to the last time updated
updateData <- function(current_time) {
  cat("Current Time:", current_time, "\n")
  cat("Last Time Checked:", last_time_updated_rv$last_time_updated, "\n")
  cat("The abs of the difference:", abs(current_time-last_time_updated_rv$last_time_updated), "\n")
  cat("Update Time Interval divided by 1000:", (update_time_interval/1000), "\n")
  if(abs(current_time-last_time_updated_rv$last_time_updated) >= (update_time_interval/1000)){
    cat("Time Conditions Indicate We Need To Check The Directory For New Files...", "\n")
    all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))
    files_to_read<- setdiff(all_files,  all_files_read_rv$files_read)
    files_to_remove<- setdiff(all_files_read_rv$files_read, all_files)
    # if(length(files_to_read)>=1){
    #   cat("files to read is...", str(files_to_read), "...")
    # }
    if(length(files_to_read)>=1){
      cat("Looks Like NEW Files Have Been Found. Processing...")
      tmp_res<- lapply(seq_along(files_to_read), function(x){
        tmpx<- base::readRDS(file = files_to_read[x]) 
      }) %>% do.call(rbind, .)
      # Now update the dataframe and reactive values list
      #df<- rbind(df, tmp_res)
      cat("Binding To Global Reactive (values$df)...")
      values$df<- rbind(values$df, tmp_res)
      all_files_read_rv$files_read <- all_files
      cat("Complete...Updating Last Time Updated Value...")
      #last_time_updated_rv$last_time_updated<- as.numeric(format(as.numeric(gsub(pattern="\\.", "", as.numeric(Sys.time())))))
      #last_time_updated_rv$last_time_updated<-as.numeric(format(as.numeric(Sys.time())))
      cat("Complete!", "\n")
      #cat("All files Read and dataframes updated.", "\n")
    } else {
      cat("No new files present. Will check again soon!", "\n")
    }
  } else {
    cat("No new files present. Will check again soon outside of inner else!", "\n")
  }
  
  if(length(files_to_remove)>=1){
    cat("It looks like some files have been removed from the streaming directory.", "\n")
    cat("Removing those files from the application's data as well", "\n")
    # The lazy way
    all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))
    tmp_res<- lapply(seq_along(all_files), function(x){
      tmpx<- base::readRDS(file = all_files[x]) 
    }) %>% do.call(rbind, .)
    cat("Resetting Global DF", "\n")
    values$df<-  tmp_res
    all_files_read_rv$files_read <- all_files
    cat("Complete...Updating Last Time Updated Value...", "\n")
    #last_time_updated_rv$last_time_updated<- as.numeric(format(as.numeric(gsub(pattern="\\.", "", as.numeric(Sys.time())))))
    #last_time_updated_rv$last_time_updated<-as.numeric(format(as.numeric(Sys.time())))
    cat("Application Global Data Successfully Reset. Resuming Normal Monitoring", "\n")
  }
  cat("*******************************************", "\n")
}
#updateData()   # also call updateData() whenever you want to reload the data




