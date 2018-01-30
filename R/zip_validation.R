csv_validation <- function(pointfile, ...){
  if(substring(tolower(pointfile), nchar(pointfile)-3) != ".csv"){
    # check if input is .CSV
    stop('Uploaded data needs to be CSV file. ');
  }else{
    fname = pointfile$Name
  }
}
