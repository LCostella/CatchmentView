findDNS <- function(measurement,root,v){
  if(root == "K")
    dataframe <- k
  if(root == "C")
    dataframe <- cc
  if(root == "D")
    dataframe <- dd
  if(root == "E")
    dataframe <- e
  if(root == "F")
    dataframe <- f
  if(root == "G")
    dataframe <- g
  if(root == "I")
    dataframe <- i
  if(root == "J")
    dataframe <- j
  if(root == "L")
    dataframe <- l
  if(root == "M")
    dataframe <- m
  if(root == "B")
    dataframe <- bb
  
  if(v=="v6")
    dataframe <- dataframe[which(dataframe$IPv6 =="Yes"),]
  else
    dataframe <- dataframe[which(dataframe$IPv4 =="Yes"),]
  
  
  if(measurement[[1]] == "undefined")
    return("undefined")
  
  lat <- measurement[[1]]
  lon <- measurement[[2]]
  
  min <- distm (c(lon,lat), c(dataframe$lon[[1]], dataframe$lat[[1]]), fun = distHaversine)
  mirror <- as.character(dataframe$Name[[1]])
  
  for (i in 2:length(dataframe$Name)){
    try(
        dis <- distm (c(lon, lat), c(dataframe$lon[[i]], dataframe$lat[[i]]), fun = distHaversine),
      
      if(dis <  min){
        min = dis
        mirror = as.character(dataframe$Name[[i]])
        
      }
    )
  }
  
  return(mirror)
  
}


findMirrors <- function(data,timee,root,v){
  listip <- data.frame()
 
  for ( i in 1:length(data$ProbeID)){
    data$resultado[[i]][[1]]
    ind <- length(data$resultado[[i]][[1]]$ip)-1
    ip <- data$resultado[[i]][[1]]$ip[[ind]]
  
    if(ip == "*"){
      cord <- c("undefined", "undefined")
    }
    else{
      if(ip %in% listip$ip){
        cord <- listip$cord
      }
      else{
        #url <- paste("http://ip-api.com/json/",ip,sep ="")
        #json <- fromJSON(url)
        #cord <- c(json$lat,json$lon)
        cord <- NULL
        try({
          url <- paste("https://stat.ripe.net/data/geoloc/data.json?resource=",ip,"&timestamp=",timee,sep="")
          json <- fromJSON(url)
          cord <- c(json$data$locations$latitude,json$data$locations$longitude)
        })
        if(is.null(cord) || is.na(cord))
          cord <-c("undefined", "undefined")
        
   
        aux  <- data.frame(ip,cord)
        listip <- rbind(listip,aux)
        
      }
      
    }
    data$mirrorCord[[i]] <- cord
    print(data$ProbeID[[i]])
    if(length(cord) > 2)   #ANYCAST IP
      dns <-"undefined"
    else
      dns <- findDNS(cord,root,v)
    
    data$mirror[[i]] <- dns
  }
  
  return(data)
  
}

