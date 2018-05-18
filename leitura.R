leitura <- function(url){
  download <- NULL
  temp <- tempfile(fileext = ".zip")
  temp2 <- tempfile()
  try({
    drive_download(file = url, temp)
    download <- "ok"
  })
  if(!is.null(download)){ 
    zip <- unzip(temp,exdir = temp2)
    data <- fromJSON(zip)
    unlink(temp)
    unlink(temp2)
    return(data)
  } 
  else{
    return(download)
  }
  
  
}