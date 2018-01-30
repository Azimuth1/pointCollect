csv_validation <- function(pointfile, ...){
  if(substring(tolower(pointfile), nchar(pointfile)-3) != ".zip"){
    stop('Uploaded data needs to be CSV file. ');
  }else{
    fname = pointfile$Name
  }
}
