
modalHopsMap <- function(datatime,hopsMap){
  #datatime <- 1514764800
  local = c("10.","127.","172.16.","192.168.")
  cordList <- c()
  cord <- c()
  hopsMap <- unlist(hopsMap)
  for ( j in 1:(length(hopsMap)-1)){
    l <- (startsWith(as.character(hopsMap[j]), local ))
    if(as.character(hopsMap[j]) != "*" && (!is.element(TRUE, l)) ){
      cord <- NULL
      try({
        url <- paste("https://stat.ripe.net/data/geoloc/data.json?resource=",hopsMap[j],"&timestamp=",datatime,sep="")
        json <- fromJSON(url)
        cord <- c(json$data$locations$latitude,json$data$locations$longitude)
      })
    }
    if(!is.null(cord))
      cordList <- append(cordList,cord)
  }
  indHop <-1
  mapData = data.frame()
  
  for(k in seq(from=1,to=(length(cordList)-3),by = 2)){
    x<- data.frame(hop = paste("hop",indHop,sep =" "),lonp1=cordList[k+1],latp1=cordList[k],lonp2=cordList[k+3],latp2=cordList[k+2])
    mapData<- rbind(mapData,x)
    indHop = indHop +1
  }
  
  n <-length(mapData$hop)
  color <- rep("blue", n)
  size  <- rep(1,n)
  color2 <- rep("blue", n)
  size2 <- rep(1,n)
  color[1] <- "green"
  size[1] <- 3
  color2[n] <- "yellow"
  size2[n] <- 3
  mapData <- cbind(color,color2,mapData,size,size2)
  
  curved.lines <-
    gcIntermediate(
      p1 = as.matrix( x = mapData[ , 4:5] )
      , p2 = as.matrix( x = mapData[ ,6:7 ] )
      , breakAtDateLine = TRUE
      , n = 1000
      , addStartEnd = TRUE
      , sp = TRUE
    ) 
  
  # create leaflet
  airport <-
    leaflet( options = leafletOptions( minZoom = 1) ) %>%
    setMaxBounds( lng1 = -180
                  , lat1 = -89.98155760646617
                  , lng2 = 180
                  , lat2 = 89.99346179538875 ) %>%
    addTiles() %>% 
    
    addCircleMarkers( data = mapData
                      , lng = ~lonp1
                      , lat = ~latp1
                      , radius = size
                      , color = ~color
                      , label =~hop
                      
    ) %>% 
    addCircleMarkers( data = mapData
                      , lng = ~lonp2
                      , lat = ~latp2
                      , radius = size2
                      , color = ~color2
                      , label =~hop
                      
    ) %>% 
    
    
    
    addPolylines( data = curved.lines
                  , weight = 2
    )
  
  
  #airport
  shinyalert::closeAlert()
  showModal(modalDialog(
    title = "Resource not implemented",
    renderLeaflet(airport),
    easyClose = TRUE
  ))
  
  
  
}