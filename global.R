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

#Sys.setenv(CENSUS_KEY="fae7c00138d0686b63f92c5f4e05653496731c0b")
#tidycensus::census_api_key("fae7c00138d0686b63f92c5f4e05653496731c0b")
#options(tigris_use_cache = FALSE)

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
make_chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
mround <- function(x,base){ 
  base*round(x/base) 
} 
# Usage:
#mround(14, 5)
decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}
generate_correlation_pairs<- function(data, lower_cutoff=.3, upper_cutoff=1, add_plot=TRUE){
  require(corrplot, quietly = TRUE)
  corx<- stats::cor(df[, which(sapply(df, class)=="numeric")], use = "pairwise.complete.obs") # using Pearson's correlation
  # Print the correplot 
  if(add_plot == TRUE){
    corrplot::corrplot(corx)
  }
  
  index <- which(abs(corx) > lower_cutoff & abs(corx) < upper_cutoff, # your criteria
                 arr.ind = T) # the result of the which function is now in rows & columns
  cor_res<- cbind.data.frame(var1 = rownames(corx)[index[,1]], # get the row name 
                             var2 = colnames(corx)[index[,2]]) # get the column name
  #View(cor_res)
  tmp<- t(apply(cor_res, 1, function(x){sort(paste(x))}))
  tmp<- as.data.frame(unique(tmp))
  colnames(tmp)<- c("Var1", "Var2")
  tmp$Var1<- base::as.character(tmp$Var1)
  tmp$Var2<- base::as.character(tmp$Var2)
  tmp$correlation_val<- 0
  
  ## Put in the correlation values
  
  for(i in 1:base::nrow(tmp)){
    row_idx<- which(rownames(corx) == tmp$Var1[i])
    col_idx<- which(colnames(corx) == tmp$Var2[i])
    tmp$correlation_val[i]<- base::round(corx[row_idx, col_idx], digits = 3)
  } 
  #View(tmp)
  drop_correlations_df<- tmp[rev(order(tmp$correlation_val)),]
  rownames(drop_correlations_df)<- NULL
  
  ## Let's put in some summary information about each variable in the drop correlations so we can decide on which variable to drop
  drop_correlations_df$Var1_LU<- 0
  drop_correlations_df$Var2_LU<- 0
  
  for(i in 1:base::nrow(tmp)){
    drop_correlations_df$Var1_LU[i] <- length(unique(df[, which(colnames(df)==drop_correlations_df$Var1[i])]))
    drop_correlations_df$Var2_LU[i] <- length(unique(df[, which(colnames(df)==drop_correlations_df$Var2[i])]))
  } 
  
  completeness_df<- data.frame(percent_complete=1-sapply(data, function(x){length(which(is.na(x)))/length(x)}), stringsAsFactors = FALSE)
  
  drop_correlations_df$Var1_Completeness<- completeness_df$percent_complete[match(drop_correlations_df$Var1, table = rownames(completeness_df))]
  drop_correlations_df$Var2_Completeness<- completeness_df$percent_complete[match(drop_correlations_df$Var2, table = rownames(completeness_df))]
  drop_correlations_df$Var1Var2Diff<- drop_correlations_df$Var1_Completeness - drop_correlations_df$Var2_Completeness
  drop_correlations_df$Var1Var2AbsDiff<- abs(drop_correlations_df$Var1_Completeness - drop_correlations_df$Var2_Completeness)
  
  return(drop_correlations_df)
  
}

# Usage:
#drop_correlations_df<- generate_correlation_pairs(data = df[, which(sapply(df, class)=="numeric")], lower_cutoff = 0.3, upper_cutoff = 1)


geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.5)  # API only allows 5 requests per second
  out
}
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

vsplit <- function(v, n) {
  l = length(v)
  r = l/n
  return(lapply(1:n, function(i) {
    s = max(1, round(r*(i-1))+1)
    e = min(l, round(r*i))
    return(v[s:e])
  }))
}
# Ex: vsplit(v=latlonBins2Search, n=3)

lu<- function(x){length(unique(x))}

'%!in%' <- function(x,y)!('%in%'(x,y))


saveas <- function(map, file){
  class(map) <- c("saveas",class(map))
  attr(map,"filesave")=file
  map
}

print.saveas <- function(x, ...){
  class(x) = class(x)[class(x)!="saveas"]
  htmltools::save_html(x, file=attr(x,"filesave"))
}




####################### START OF APP SPECIFIC GLOBAL CODE HERE ##########################

# Also borrowing a method from: https://stackoverflow.com/questions/14902188/update-a-data-frame-in-shiny-server-r-without-restarting-the-app

## Setting up an update interval, say once every 15 seconds
update_time_interval<-15
update_time_interval<- update_time_interval * 1000 # milliseconds for 'invalidateLater' function in server.R
cat("Update Time Interval is:", update_time_interval, "\n")


# Sample, setup the fake 'large' data set to read in
all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))

## Make a variable to prevent mass data refresh calls if we have multiple sessions up and running
last_time_updated<- max(as.numeric(as.character(sapply(strsplit(all_files, split = "_"), `[[`, 7))))
#last_time_updated<- last_time_updated[length(last_time_updated)-1] # the actual last time it was updated
last_time_updated_rv<- reactiveValues(last_time_updated=as.numeric(format(as.numeric(last_time_updated))))



df<- lapply(seq_along(all_files), function(x){
  base::readRDS(file = all_files[x]) 
}) %>% do.call(rbind, .)
# update all files read AFTER successfully readin in the data
all_files_read<- all_files
all_files_read_rv<- reactiveValues()
all_files_read_rv$files_read<- all_files_read

# Now make a function to make some fake data and add it to the tmp_data_list directory (simulate streaming)
new_data_list<- function(){
  d<- data.frame(a=1:10, b=rnorm(10))
  time<- base::as.numeric(Sys.time())
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


values <- shiny::reactiveValues(df=df)

updateData <- function(current_time) {
  if(abs(current_time-last_time_updated_rv$last_time_updated) >= (update_time_interval/1000)){
    cat("Time Conditions Met...")
    all_files<- base::normalizePath(base::list.files(path = "./tmp_data_list/", pattern =".rds", full.names = TRUE ))
    files_to_read<- setdiff(all_files,  all_files_read_rv$files_read)
    cat("files to read is...", str(files_to_read), "...")
    if(length(files_to_read)>=1){
      cat("Running against all files...")
      tmp_res<- lapply(seq_along(files_to_read), function(x){
        tmpx<- base::readRDS(file = files_to_read[x]) 
      }) %>% do.call(rbind, .)
      # Now update the dataframe and reactive values list
      #df<- rbind(df, tmp_res)
      values$df<- rbind(values$df, tmp_res)
      all_files_read_rv$files_read <- all_files
      last_time_updated_rv$last_time_updated<- as.numeric(format(as.numeric(gsub(pattern="\\.", "", as.numeric(Sys.time())))))
      cat("All files Read and dataframes updated.", "\n")
    } else {
      cat("No new files read.", "\n")
      #cat("No new files read, updating last time updated...", "\n")
      #last_time_updated_rv$last_time_updated<- as.numeric(format(as.numeric(gsub(pattern="\\.", "", as.numeric(Sys.time())))))
    }
  } else {
    cat("None Found...", "\n")
    #last_time_updated_rv$last_time_updated<- as.numeric(format(as.numeric(gsub(pattern="\\.", "", as.numeric(Sys.time())))))
  }

  
  # vars <- load(file = "my_data_frame.RData", envir = .GlobalEnv)
  # for (var in vars)
  #   values[[var]] <- get(var, .GlobalEnv)
}
#updateData()   # also call updateData() whenever you want to reload the data

# For server.R
# output$foo <- reactivePlot(function() {
#   # Assuming the .RData file contains a variable named mydata
#   plot(values$mydata)
# }

new_data <- function() {
  d <- list(a=1:100, b=rnorm(100))
  saveRDS(file='./tmp/data.rds', d)
}
#new_data()



