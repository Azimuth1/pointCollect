csv_validation <- function(pointfile, ...){
  if(substring(tolower(pointfile), nchar(pontfile)-3) != ".zip"){
    stop('Uploaded data needs to be CSV file. ');
  }else{
    fname = pointfile$Name
  }
}
