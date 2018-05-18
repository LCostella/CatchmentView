map <- function(datatime, hopsMap) {
  local = c("10.", "127.", "172.16.", "192.168.")
  cordList <- c()
  cord <- c()
  hopsMap <- unlist(hopsMap)
  for (j in 1:(length(hopsMap) - 1)) {
    l <- (startsWith(as.character(hopsMap[j]), local))
    if (as.character(hopsMap[j]) != "*" && (!is.element(TRUE, l))) {
      cord <- NULL
      try({
        url <-
          paste(
            "https://stat.ripe.net/data/geoloc/data.json?resource=",
            hopsMap[j],
            "&timestamp=",
            datatime,
            sep = ""
          )
        json <- fromJSON(url)
        cord <-
          c(json$data$locations$latitude,
            json$data$locations$longitude)
      })
    }
    if (!is.null(cord))
      cordList <- append(cordList, cord)
  }
  indHop <- 1
  mapData = data.frame()
  
  for (k in seq(from = 1,
                to = (length(cordList) - 3),
                by = 2)) {
    x <-
      data.frame(
        hop = paste("hop", indHop, sep = " "),
        lonp1 = cordList[k + 1],
        latp1 = cordList[k],
        lonp2 = cordList[k + 3],
        latp2 = cordList[k + 2]
      )
    mapData <- rbind(mapData, x)
    indHop = indHop + 1
  }
  
  n <- length(mapData$hop)
  color <- rep("blue", n)
  size  <- rep(1, n)
  color2 <- rep("blue", n)
  size2 <- rep(1, n)
  color[1] <- "green"
  size[1] <- 3
  color2[n] <- "yellow"
  size2[n] <- 3
  mapData <- cbind(color, color2, mapData, size, size2)
  
  curved.lines <-
    gcIntermediate(
      p1 = as.matrix(x = mapData[, 4:5])
      ,
      p2 = as.matrix(x = mapData[, 6:7])
      ,
      breakAtDateLine = TRUE
      ,
      n = 1000
      ,
      addStartEnd = TRUE
      ,
      sp = TRUE
    )
  
  # create leaflet
  airport <-
    leaflet(options = leafletOptions(minZoom = 1)) %>%
    setMaxBounds(
      lng1 = -180
      ,
      lat1 = -89.98155760646617
      ,
      lng2 = 180
      ,
      lat2 = 89.99346179538875
    ) %>%
    addTiles() %>%
    
    addCircleMarkers(
      data = mapData
      ,
      lng = ~ lonp1
      ,
      lat = ~ latp1
      ,
      radius = size
      ,
      color = ~ color
      ,
      label =  ~ hop
      
    ) %>%
    addCircleMarkers(
      data = mapData
      ,
      lng = ~ lonp2
      ,
      lat = ~ latp2
      ,
      radius = size2
      ,
      color = ~ color2
      ,
      label =  ~ hop
      
    ) %>%
    
    
    
    addPolylines(data = curved.lines
                 , weight = 2)
  hide("divLoad")
  hide("divLoadv6")
  airport
  
}


modalProbe <- function(hour, probe, idResult, data, module) {
  for (i in 1:length(probes$ProbeID)) {
    if (probes$ProbeID[[i]] == probe) {
      idResult <- idResult
      id <- probes$ProbeID[[i]]
      pipv4 <- probes$IPv4prefix[[i]]
      pipv6 <- probes$IPv6prefix[[i]]
      ipv4 <- probes$IPv4[[i]]
      ipv6 <- probes$IPv6[[i]]
      asnv4 <- probes$asnv4[[i]]
      asnv6 <- probes$asnv6[[i]]
      country <- probes$Country[[i]]
      lat <- probes$Latitude[[i]]
      long <- probes$Longitude[[i]]
      found <- 1
      break
    }
  }
  if (!is.null(found)) {
    from <- list()
    tmin <- list()
    tmax <- list()
    
    for (i in 1:length(data$id)) {
      if (idResult == data$id[[i]]) {
        for (j in 1:length(data$result[[i]])) {
          from <- append(from, data$result[[i]][[j]]$ip)
          tmin <- append(tmin, data$result[[i]][[j]]$tmin)
          tmax <- append(tmax, data$result[[i]][[j]]$tmax)
        }
        break
        
      }
    }
    
    hopsMapv4 <- from
    hopsv4 <-
      data.frame(
        hop = 1:length(from),
        from = cbind(from),
        tmin = cbind(tmin),
        tmax = cbind(tmax)
      )
    
    a <- strsplit(idResult, "v")[[1]]
    if (a[2] == "4")
      idResult <- paste0(a[1], "v6")
    else
      idResult <- paste0(a[1], "v4")
    
    if (module == 1 && is.element(idResult, data$id)) {
      #if(is.element(idResult,data$id)){
      
      from <- list()
      tmin <- list()
      tmax <- list()
      
      for (i in 1:length(data$id)) {
        if (idResult == data$id[[i]]) {
          for (j in 1:length(data$result[[i]])) {
            from <- append(from, data$result[[i]][[j]]$ip)
            tmin <- append(tmin, data$result[[i]][[j]]$tmin)
            tmax <- append(tmax, data$result[[i]][[j]]$tmax)
          }
          break
          
        }
      }
      
      hopsMapv6 <- from
      hopsv6 <-
        data.frame(
          hop = 1:length(from),
          from = cbind(from),
          tmin = cbind(tmin),
          tmax = cbind(tmax)
        )
      
      if (a[2] == "6") {
        auxmap <- hopsMapv6
        auxhops <- hopsv6
        hopsMapv6 <- hopsMapv4
        hopsv6 <- hopsv4
        hopsv4 <- auxhops
        hopsMapv4  <- auxmap
        
      }
      showModal(
        modalDialog(
          size = "l",
          footer = NULL,
          title = paste("Probe", id, sep = " "),
          HTML(
            "<h3> General Informations </h3> <hrstyle='margin: 0px;'>"
          ),
          fluidRow(column(4,
                          HTML(
                            paste0(
                              "<b> Probe </b>",
                              id,
                              "<hr style='margin: 0px;' >",
                              "<br>",
                              "<b>Country </b>",
                              country,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Latitude </b>",
                              lat,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Longitude </b>",
                              long,
                              "<hr style='margin: 0px;'>",
                              "<br>"
                            )
                          )),
                   
                   column(
                     8,
                     box(
                       title = "Geolocation",
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       collapsed = TRUE,
                       width = NULL,
                       renderLeaflet({
                         m <- leaflet() %>%
                           setView(lng = long,
                                   lat = lat,
                                   zoom = 3)
                         addTiles(m) %>%
                           
                           
                           addMarkers(
                             lng = long,
                             lat = lat,
                             popup = paste("Probe", id, sep = " ")
                           )
                       })
                     )
                   )),
          fluidRow(column(6,
                          HTML(
                            paste0(
                              "<h2> IPv4 </h2> <hr>",
                              "<b>Prefix </b>",
                              pipv4,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Address </b>",
                              ipv4,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>ASN </b> ",
                              asnv4,
                              "<hr style='margin: 0px;'>",
                              "<br>"
                            )
                          )),
                   column(6,  HTML(
                     paste0(
                       "<h2> IPv6 </h2> <hr>",
                       "<b>Prefix </b>",
                       pipv6,
                       "<hr style='margin: 0px;'>",
                       "<br>",
                       "<b>Address </b>",
                       ipv6,
                       "<hr style='margin: 0px;'>",
                       "<br>",
                       "<b>ASN </b>",
                       asnv6,
                       "<hr style='margin: 0px;'>",
                       "<br>"
                     )
                   ))),
          fluidRow(column(
            6,
            box(
              title = "Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              div(style = 'overflow-x: scroll', renderTable(
                hopsv4, hover = TRUE, striped = TRUE
              ))
            )
          ),
          column(
            6,
            box(
              title = "Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              div(style = 'overflow-x: scroll', renderTable(
                hopsv6, hover = TRUE, striped = TRUE
              ))
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Geolocation Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              tags$div(id="divLoad",tags$img(id="loadImg",src = "https://thumbs.gfycat.com/RadiantCheerfulBigmouthbass-max-1mb.gif")),
              renderLeaflet(map(hour, hopsMapv4))
            )
          ),
          column(
            6,
            box(
              title = "Geolocation Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              tags$div(id="divLoadv6",tags$img(id="loadImg",src = "https://thumbs.gfycat.com/RadiantCheerfulBigmouthbass-max-1mb.gif")),
              renderLeaflet(map(hour, hopsMapv6))
            )
          )),
          easyClose = TRUE
        )
      )
    }
    else{
      showModal(
        modalDialog(
          size = "l",
          footer = NULL,
          title = paste("Probe", id, sep = " "),
          HTML("<h3> General Informations </h3> <hr>"),
          fluidRow(column(4,
                          HTML(
                            paste0(
                              "<b> Probe </b>",
                              id,
                              "<hr style='margin: 0px;' >",
                              "<br>",
                              "<b>Country </b>",
                              country,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Latitude </b>",
                              lat,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Longitude </b>",
                              long,
                              "<hr style='margin: 0px;'>",
                              "<br>"
                            )
                          )),
                   
                   column(
                     8,
                     box(
                       title = "Geolocation",
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       collapsed = TRUE,
                       width = NULL,
                       renderLeaflet({
                         m <- leaflet() %>%
                           setView(lng = long,
                                   lat = lat,
                                   zoom = 3)
                         addTiles(m) %>%
                           
                           
                           addMarkers(
                             lng = long,
                             lat = lat,
                             popup = paste("Probe", id, sep = " ")
                           )
                       })
                     )
                   )),
          fluidRow(column(6,
                          HTML(
                            paste0(
                              "<h2> IPv4 </h2> <hr>",
                              "<b>Prefix </b>",
                              pipv4,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>Address </b>",
                              ipv4,
                              "<hr style='margin: 0px;'>",
                              "<br>",
                              "<b>ASN </b> ",
                              asnv4,
                              "<hr style='margin: 0px;'>",
                              "<br>"
                            )
                          )),
                   column(6,  HTML(
                     paste0(
                       "<h2> IPv6 </h2> <hr>",
                       "<b>Prefix </b>",
                       pipv6,
                       "<hr style='margin: 0px;'>",
                       "<br>",
                       "<b>Address </b>",
                       ipv6,
                       "<hr style='margin: 0px;'>",
                       "<br>",
                       "<b>ASN </b>",
                       asnv6,
                       "<hr style='margin: 0px;'>",
                       "<br>"
                     )
                   ))),
          
          HTML("<br>"),
          fluidRow(column(
            6,
            box(
              title = "Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              div(style = 'overflow-x: scroll', renderTable(
                hopsv4, hover = TRUE, striped = TRUE
              ))
            )
          ),
          column(
            6,
            box(
              title = "Geolocation Hops",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              tags$div(id="divLoad",tags$img(id="loadImg",src = "https://thumbs.gfycat.com/RadiantCheerfulBigmouthbass-max-1mb.gif")),
              renderLeaflet(map(hour, hopsMapv4))
            )
          )),
          #renderTable(hops),
          HTML("<br>"),
          HTML("<br>"),
          
          
          
          easyClose = TRUE
        )
      )
      
      
    }
    
  }
  else{
    showModal(modalDialog(
      title = paste("Probe", probe, sep = " "),
      HTML(paste(
        "<b>There aren't informations about this probe </b>"
      )),
      easyClose = TRUE
    ))
  }
}