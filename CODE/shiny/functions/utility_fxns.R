############################################
# code author: erin stearns
# script objective: functions to simplify and/or streamline utility-type actions
# date: 25 april 2019
###########################################

make_time_stamp <- function(time_stamp = TRUE) {
  # Function to standardize time stamp format 
  #   Input args:
  #           time_stamp = TRUE or FALSE, defaults to TRUE
  #   Output: character string of date and time
  #           precise output: "<year>_<month>_<day>_<hour>_<minute>_<second>"
  
  if(time_stamp == TRUE){
    
    run_date <- gsub("-","_",Sys.time())
    run_date <- gsub(":","_",run_date)
    run_date <- gsub(" ","_",run_date)  
    
  } else {
    run_date <- 'scratch'
  }
  
  return(run_date)
  
}

save_profvis2html <- function(input, outdir, outname = NA){
  # Function to grab previous profvis output and save as HTML
  #   Input args:
  #           input   = profvis output file you would like to convert to an html -- include full path & file name w/extension
  #           outdir  = where do you want the html saved?
  #           outname = what do you want the html file named? defaults to the same name as profvis input if undefined
  #   Output: html file in designated dir
  
  if (is.na(outname)){
    outname <- gsub(".*/(.*)\\..*","\\1", (input), perl = T)
  } else {
    outname <- (outname)
  }
  
  #read in profvis output
  p1 <- profvis(prof_input = (input))
  
  #save as html
  htmlwidgets::saveWidget(p1, (outdir)) 
}
