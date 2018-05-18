# library(devtools)
# library(shinydashboard)
# library(jsonlite)
# library(triebeard)
# library(leaflet)
# library(doParallel)
# library(geosphere)
# library(DT)
# library(shiny)
# library(shinyalert)
# library(shinyBS)
# library(curl)
# library(devtools)
# library(networkD3)
# library(googledrive)
# library(ggplot2)
# library(plotly)
#shinyalert(title = "Loading...", className = "alert",closeOnClickOutside = FALSE,animation = "pop",closeOnEsc = FALSE,showConfirmButton = FALSE,imageWidth = 120, imageHeight =  120,imageUrl ="http://superstorefinder.net/support/wp-content/uploads/2018/01/4colors.gif", text = "Loading necessary datas. Wait a moment, please!",html = TRUE  )
#shinyalert::closeAlert()
#loadMirrors <- dget("loadMirrors.R")
#loadM <- loadMirrors()
#source('global.R', local = TRUE)

#devtools::install_github("LCostella/NetworkD3")
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#drive_auth_config(api_key = "473423191387-pa9gh8i7041gccu336pb1guhcbqc5l57.apps.googleusercontent.com")
indTemp <- 0
countries <<- NULL
coordenates <-
  list(
    c(90,-180, -90, 180),
    c(45, 45,-90, 180),
    c(90, 45,	45, 180),
    c(30,-30,-90, 45),
    c(90,-30,	30, 45),
    c(90,-180,-90,-30)
  )
#imageUrl ="http://gem-advertising.com/images/new-icons/network.gif"

shinyServer(function(input, output, session) {

  find <- dget("findAsn.R")
  printer <- dget("printer.R")
  modalProbe <- dget("modalProbe.R")
  modalAS <- dget("modalASR.R")
  modalMirror <- dget("modalMirror.R")
  totalProbesMap <- dget("totalProbesMap.R")
  findMirrors <- dget("findMirrors.R")

  performGraph <- dget("performGraph.R")
  modalHopsMap <- dget("modalHopsMap.R")
  nodes <- dget("nodes.R")
  links <- dget("links.R")
  leitura <- dget("leitura.R")
  stat <- dget("stat.R")
  values <-
    reactiveValues(
      probeResult = 0,
      module = 0,
      listRows = 0,
      listOfMirrors = 0,
      probesDisplayed = 0,
      Tasnv4 = 0,
      Tasnv6 = 0,
      displayedIDs = 0,
      datav4 = 0,
      datav6 = 0,
      data = 0,
      datav4Download = 0,
      datav6Download = 0,
      dataTemp = 0,
      firstclick = 0,
      firstclickTemp = 0,
      dataset = 0,
      datasetTemp = 0,
      hopsMapv4 = 0,
      hopsMapv6 = 0,
      hour = 0,
      data = 0,
      version = 0,
      TasnT = 0,
      perform = 0,
      root = 0
    )
  
  values$firstclick <- NULL
  values$firstclickTemp <- NULL
  values$module <- NULL  #1 to 6x4, 2 to Temp
  values$dataset <- c()
  values$datasetTemp <- c()
  hide(id = "all")
  hide(id = "v4")
  hide(id = "v6")
  
  observeEvent(input$action6x4, {
    values$firstclickTemp <- NULL
    hide(id = "about", anim = TRUE)
    hide(id = "all")
    show(id = "v4")
    show(id = "v6")
    hide(id = "space")
    listIDsProbe <- input$captionProbe
    listIDsAsn <- input$captionAsn

    shinyalert(
      title = "Filtering data...",
      className = "alert",
      closeOnClickOutside = FALSE,
      animation = "pop",
      closeOnEsc = FALSE,
      showConfirmButton = FALSE,
      imageWidth = 120,
      imageHeight =  120,
      imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
      text = "Wait a moment, please!"
    )
    
    if (grepl("[^[:digit:] + ;]", listIDsProbe) ||
        grepl("[^[:digit:] + ;]", listIDsAsn)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      
    }
    else{
      values$root    <- as.character(input$select6x4)
      temp_ini <- Sys.time()
      hour    <-
        as.numeric(as.POSIXct(input$date6x4, format = "%Y-%m-%d", tz = "utc")) +
        ((input$slider_hours6x4) * 3600)
      aux  <- as.POSIXct(hour, origin = "1970-01-01", tz = "GMT")
      date <-
        strsplit(strsplit(as.character(aux), " ")[[1]][1] , "-")[[1]]
      year <- as.character(date[1])
      month <- as.character(date[2])
      day <- as.character(date[3])
      
      values$hour <- hour
      
      if (is.null(values$firstclick) ||
          values$dataset[1] != values$hour || values$dataset[2] != values$root) {
        values$datav6Download <- NULL
        values$datav4Download  <- NULL
        url <- paste(values$root, "datav6_", values$hour, ".zip", sep = "")
        values$datav6Download  <- leitura(url)
        
        url <- paste(values$root, "datav4_", values$hour, ".zip", sep = "")
        values$datav4Download  <- leitura(url)
        
        datav6 <- values$datav6Download
        datav4 <- values$datav4Download
      }
      
      else{
        datav6 <- values$datav6Download
        datav4 <- values$datav4Download
      }
      if (length(datav4) < 1 || length(datav6) < 1) {
        shinyalert::closeAlert()
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "There are not corresponding IPv6 and IPv4 data for this parameters",
          type = "error"
        )
      }
      else{
        if ((is.null(listIDsAsn) ||
             listIDsAsn == "") && (is.null(listIDsProbe) || listIDsProbe == "")) {
          usedProbes <- datav6$ProbeID
          datav4 <-
            datav4[which(is.element(datav4$ProbeID, usedProbes)), ]
          usedProbes <- datav4$ProbeID
          datav6 <-
            datav6[which(is.element(datav6$ProbeID, usedProbes)), ]
        }
        else{
          if (!(is.null(listIDsProbe) || listIDsProbe == "")) {
            listIDsProbe <- strsplit(listIDsProbe, ";")
            listIDsProbe <- unique(listIDsProbe)
            datav4 <-
              datav4[which(is.element(
                as.numeric(datav4$ProbeID),
                as.numeric(listIDsProbe[[1]])
              )), ]
            datav6 <-
              datav6[which(is.element(
                as.numeric(datav6$ProbeID),
                as.numeric(listIDsProbe[[1]])
              )), ]
            
          }
          if (!(is.null(listIDsAsn) || listIDsAsn == "")) {
            listIDsAsn <- strsplit(listIDsAsn, ";")
            listIDsAsn <- unique(listIDsAsn)
            datav4 <-
              datav4[which(is.element(as.numeric(datav4$as), as.numeric(listIDsAsn[[1]]))), ]
            datav6 <-
              datav6[which(is.element(as.numeric(datav6$as), as.numeric(listIDsAsn[[1]]))), ]
          }
        }
        
        if (input$selectLocation == 2) {
          coord <- as.numeric(input$selectRegion)
          latmax <- coordenates[[coord]][1]
          latmin <- coordenates[[coord]][3]
          lonmin <- coordenates[[coord]][2]
          lonmax <- coordenates[[coord]][4]
          
          datav4 <-
            datav4[which(
              as.numeric(datav4$lat) >= latmin &
                as.numeric(datav4$lat) <= latmax &
                as.numeric(datav4$lon) >= lonmin &
                as.numeric(datav4$lon) <= lonmax
            ), ]
          datav6 <-
            datav6[which(
              as.numeric(datav6$lat) >= latmin &
                as.numeric(datav6$lat) <= latmax &
                as.numeric(datav6$lon) >= lonmin &
                as.numeric(datav6$lon) <= lonmax
            ), ]
        }
        if (input$selectLocation == 3) {
          countriesList <- input$countries
          countriesInitials <-
            countries[which(is.element(countries$name, countriesList)), ]
          countriesList <- countriesInitials$countries
          prbCon <-
            probes[which(is.element(probes$Country, countriesList)), ]
          prbCountryList <- prbCon$ProbeID
          datav4 <-
            datav4[which(is.element(
              as.numeric(datav4$ProbeID),
              as.numeric(prbCountryList)
            )), ]
          datav6 <-
            datav6[which(is.element(
              as.numeric(datav6$ProbeID),
              as.numeric(prbCountryList)
            )), ]
          
        }
        
        if (length(datav4$ProbeID) == 0 ||
            length(datav6$ProbeID) == 0) {
          shinyalert::closeAlert()
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "There are not corresponding IPv6 and IPv4 data for this parameters",
            type = "error"
          )
        }
        else{
          ve4 <- datav4
          ve6 <- datav6
          aux <-
            ve4[which(!is.element(ve4$ProbeID, ve6$ProbeID)), ]
          aux2 <-
            ve6[which(!is.element(ve6$ProbeID, ve4$ProbeID)), ]
          aux3 <-
            rbind(ve6[which(is.element(ve6$ProbeID, ve4$ProbeID)), ], ve4[which(is.element(ve4$ProbeID, ve6$ProbeID)), ])
          aux3 <- aux3[!duplicated(aux3$ProbeID), ]
          
          if (length(aux$ProbeID) > 0) {
            aux$IpV4 <- "Yes"
            aux$IpV6 <- "No"
          }
          
          if (length(aux2$ProbeID) > 0) {
            aux2$IpV6 <- "Yes"
            aux2$IpV4 <- "No"
          }
          
          if (length(aux3$ProbeID) > 0) {
            aux3$IpV4 <- "Yes"
            aux3$IpV6 <- "Yes"
          }
          
          
          probesList <- rbind(aux, aux2, aux3)
          probesList$resultado <- NULL
          
          if (length(datav4$ProbeID) > 15 ||
              length(datav6$ProbeID) > 15) {
            datav4 <- datav4[1:15, ]
            datav6 <- datav6[1:15, ]
            #datav4 <- datav4[sample(nrow(datav4), 5), ]
            #prb <- datav4$ProbeID
            #datav6 <- datav6[which(is.element(datav6$ProbeID,prb)),]
          }
          
          probesDisplayed <- rbind(datav4, datav6)
          probesDisplayed <-
            probesDisplayed[!duplicated(probesDisplayed$ProbeID),]
          probesDisplayed <-
            data.frame(
              ProbeID = probesDisplayed$ProbeID,
              as =  probesDisplayed$as,
              Longitude =  probesDisplayed$lon,
              Latitude =  probesDisplayed$lat
            )
          values$probesDisplayed <- probesDisplayed
          displayedIDs <- c()
          for (i in probesDisplayed$ProbeID) {
            displayedIDs <- append(displayedIDs, which(probesList$ProbeID == i))
            
          }
          values$displayedIDs <- displayedIDs
          listOfMirrors <- NULL
          datav4 <- findMirrors(datav4, values$hour, values$root, "v4")
          datav6 <- findMirrors(datav6, values$hour, values$root, "v6")
          listOfMirrors <-
            append(as.character(datav6$mirror),
                   as.character(datav4$mirror))
          values$listOfMirrors <- listOfMirrors
          
          datav4 <- find(datav4, 4)
          datav6 <- find(datav6, 6)
          Tasnv4 <- unlist(datav4$asn)
          Tasnv6 <- unlist(datav6$asn)
          values$Tasnv4 <- Tasnv4
          values$Tasnv6 <- Tasnv6
          values$perform <- performGraph(datav4, datav6, "6x4", "")
          
          nodesv6 <- nodes(datav6, "v6", values$root)
          linkv6  <- links(datav6, "v6", values$root)
          rotav6  <- nodesv6$globalRota[[1]]
          nodesv6$globalRota <- NULL
          
          nodesv4 <- nodes(datav4, "v4", values$root)
          linkv4  <- links(datav4, "v4", values$root)
          rotav4  <- nodesv4$globalRota[[1]]
          nodesv4$globalRota <- NULL
          leorotav6 <<- rotav6
          leonodesv4 <<- nodesv4
          if ((is.null(listIDsProbe) ||
               listIDsProbe == "" ||
               listIDsProbe == " ") &&
              (is.null(listIDsAsn) || listIDsAsn == "" || listIDsAsn == " ")) {
            nodesv4$group  <- paste(nodesv4$group, rotav6, sep = "**")
            nodesv6$group <-
              paste(nodesv6$group, rotav4, sep = "**")
          }
          
          subset <- nodesv6[1:length(datav6$ProbeID), "name"]
          auxMe <- data.frame(id = subset)
          auxMe$result <- datav6$resultado
          probesResult <- auxMe
          
          subset <- nodesv4[1:length(datav4$ProbeID), "name"]
          auxMe <- data.frame(id = subset)
          auxMe$result <- datav4$resultado
          
          values$probesResult <- rbind(probesResult, auxMe)
          shinyalert::closeAlert() ## remove load 6x4 modal
          
          values$dataset <- c(values$hour, values$root)
          values$firstclick <- 1
          values$module <- 1
          values$datav4 <- datav4
          values$datav6 <- datav6
          
          output$v4 <- renderForceNetwork({
            printer(linkv4, nodesv4)
          })
          
          output$v6 <- renderForceNetwork({
            printer(linkv6, nodesv6)
          })
          
          observeEvent(input$button, {
            mapa <- values$probesDisplayed
            totalProbesMap(values$probesDisplayed)
          })
          
          observeEvent(input$buttonStat, {
            stat(
              values$datav4,
              values$datav6,
              values$listOfMirrors,
              values$Tasnv4,
              values$Tasnv6,
              values$perform
            )
          })
          
          output$map <- renderValueBox({
            valueBox(
              value = "Map",
              subtitle = HTML(
                "<b>Probes Used</b> <button id=\"button\" style=\"background-color:Transparent;width:100%;
                height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                class=\"btn btn-default action-button\"> </button>"
              ),
              icon = icon("globe")
            )
          })
          
          output$ProbesStat <- renderValueBox({
            valueBox(
              value = "Probes",
              subtitle = HTML(
                "<b>Statistics</b> <button id=\"buttonStat\" style=\"background-color:Transparent;width:100%;
                height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                class=\"btn btn-default action-button\"> </button>"
              ),
              icon = icon("signal")
            )
          })
          
          output$groupslist <- DT::renderDataTable(
            probesList,
            selection = list(mode = 'multiple', selected = values$displayedIDs),
            rownames = FALSE,
            escape = FALSE
          )
          
          output$set <- renderValueBox({
            valueBox(
              value = "Settings",
              subtitle = HTML(
                paste0(
                  length(values$displayedIDs),
                  " ",
                  "<b>Probes Displayed</b> <button id=\"reload\" style=\"background-color:Transparent;width:100%;
                  height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                  class=\"btn btn-default action-button\"> </button>"
                )
              ),
              #subtitle =  paste("Date", " ",input$temp,"   ",input$slider_hoursTemp,"h","00min",sep=""),
              icon = icon("cogs")
            )
          })
          
          output$Refresh1 <- renderText({
            paste(
              "RIPE Atlas measurement to Root" ,
              " ",
              values$root,
              " ",
              "on:",
              " ",
              input$date6x4,
              "   ",
              input$slider_hours6x4,
              "h",
              "00min",
              sep = ""
            )
          })
          
          
          
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          
          
          
          observeEvent(input$reload, {
            showModal(
              modalDialog(
                size = "m",
                title = "Probes Displayed",
                footer = NULL,
                easyClose = TRUE,
                DT::dataTableOutput("groupslist"),
                bsButton("attlist", style = "primary", label = "OK"),
                bsButton("clearAttlist",style = "default",label = "CLEAR")
              )
            )
          })
          
          observeEvent(input$attlist, {
            values$listRows <- input$groupslist_rows_selected
            listRows <- values$listRows
            if (length(listRows) == 0 || is.null(listRows) || length(listRows) > 10) {
              sendSweetAlert(session = session,
                             title = "Error",
                             text = "Invalid number of Probes. Please, select only 10 Probes",
                             type = "error")            
            }
            else{
              shinyalert(
                title = "Filtering data...",
                className = "alert",
                closeOnClickOutside = FALSE,
                animation = "pop",
                closeOnEsc = FALSE,
                showConfirmButton = FALSE,
                imageWidth = 120,
                imageHeight =  120,
                imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
                text = "Wait a moment, please!"
              )
              
              displayedIDs <- listRows
              probesDisplayed <- probesList[listRows, ]
              datav4 <-
                values$datav4Download[which(is.element(
                  as.numeric(values$datav4Download$ProbeID),
                  as.numeric(probesDisplayed$ProbeID)
                )), ]
              datav6 <-
                values$datav6Download[which(is.element(
                  as.numeric(values$datav6Download$ProbeID),
                  as.numeric(probesDisplayed$ProbeID)
                )), ]
              datav4 <- datav4[order(datav4$ProbeID), ]
              datav6 <- datav6[order(datav6$ProbeID), ]
              values$datav4 <- datav4
              values$datav6 <- datav6
              
              probesDisplayed <- rbind(datav4, datav6)
              probesDisplayed <-
                probesDisplayed[!duplicated(probesDisplayed$ProbeID),]
              probesDisplayed <-
                data.frame(
                  ProbeID = probesDisplayed$ProbeID,
                  as =  probesDisplayed$as,
                  Longitude =  probesDisplayed$lon,
                  Latitude =  probesDisplayed$lat
                )
              values$probesDisplayed <- probesDisplayed
              displayedIDs <- c()
              print(displayedIDs)
              print(probesDisplayed$ProbeID)
              for (i in probesDisplayed$ProbeID) {
                displayedIDs <- append(displayedIDs, which(probesList$ProbeID == i))
              }
              values$displayedIDs <- displayedIDs
              listOfMirrors <- NULL
              datav4 <-
                findMirrors(datav4, values$hour, values$root, "v4")
              datav6 <-
                findMirrors(datav6, values$hour, values$root, "v6")
              listOfMirrors <-
                append(as.character(datav6$mirror),
                       as.character(datav4$mirror))
              values$listOfMirrors <- listOfMirrors
              datav4 <- find(datav4, 4)
              datav6 <- find(datav6, 6)
              Tasnv4 <- unlist(datav4$asn)
              Tasnv6 <- unlist(datav6$asn)
              values$Tasnv4 <- Tasnv4
              values$Tasnv6 <- Tasnv6
              values$perform <- performGraph(datav4, datav6, "6x4", "")
              
              nodesv6 <- nodes(datav6, "v6", values$root)
              linkv6  <- links(datav6, "v6", values$root)
              rotav6  <- nodesv6$globalRota[[1]]
              nodesv6$globalRota <- NULL
              nodesv4 <- nodes(datav4, "v4", values$root)
              linkv4  <- links(datav4, "v4", values$root)
              rotav4  <- nodesv4$globalRota[[1]]
              nodesv4$globalRota <- NULL
              
              if ((is.null(listIDsProbe) ||
                   listIDsProbe == "" ||
                   listIDsProbe == " ") &&
                  (is.null(listIDsAsn) || listIDsAsn == "" || listIDsAsn == " ")) {
                nodesv4$group  <- paste(nodesv4$group, rotav6, sep = "**")
                nodesv6$group <-
                  paste(nodesv6$group, rotav4, sep = "**")
              }
              
              subset <-
                nodesv6[1:length(datav6$ProbeID), "name"]
              auxMe <- data.frame(id = subset)
              auxMe$result <- datav6$resultado
              probesResult <- auxMe
              
              subset <-
                nodesv4[1:length(datav4$ProbeID), "name"]
              auxMe <- data.frame(id = subset)
              auxMe$result <- datav4$resultado
              #probesResult <- rbind(probesResult,auxMe)
              values$probesResult <- rbind(probesResult, auxMe)
              
              output$v4 <- renderForceNetwork({
                printer(linkv4, nodesv4)
              })
              
              output$v6 <- renderForceNetwork({
                printer(linkv6, nodesv6)
              })
              
              output$groupslist <-
                DT::renderDataTable(
                  probesList,
                  selection = list(
                    mode = 'multiple',
                    selected = values$displayedIDs
                  ),
                  rownames = FALSE,
                  escape = FALSE
                )
              
              values$dataset <- c(values$hour, values$root)
            
            shinyalert::closeAlert()
            removeModal()
            }
          }) ##Att lista of probes displayed
          
        }
      }
    }
  }) ### 6 vs 4 MODULE
  
  observeEvent(input$actionTemp, {
    values$firstclick <- NULL
    hide(id = "about", anim = TRUE)
    hide(id = "v4")
    hide(id = "v6")
    hide(id = "space")
    show(id = "all")
    listIDsProbe <- input$captionProbeTemp
    listIDsAsn <- input$captionAsnTemp
    
    shinyalert(
      title = "Filtering data...",
      className = "alert",
      closeOnClickOutside = FALSE,
      animation = "pop",
      closeOnEsc = FALSE,
      showConfirmButton = FALSE,
      imageWidth = 120,
      imageHeight =  120,
      imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
      text = "Wait a moment, please!"
    )
    
    if (grepl("[^[:digit:] + ;]", listIDsProbe) ||
        grepl("[^[:digit:] + ;]", listIDsAsn)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      
    }
    else{
      values$root    <- as.character(input$selectTemp)
      version <- input$radioTemp
      hour    <-
        as.numeric(as.POSIXct(input$temp, format = "%Y-%m-%d", tz = "utc")) +
        ((input$slider_hoursTemp) * 3600)
      values$version <- version
      values$hour <- hour
      
      if (is.null(values$firstclickTemp) ||
          values$datasetTemp[1] != hour ||
          values$datasetTemp[2] != values$root  ||
          values$datasetTemp[3] != version) {
        if (version == 6)
          url <- paste(values$root, "datav6_", hour, ".zip", sep = "")
        else
          url <- paste(values$root, "datav4_", hour, ".zip", sep = "")
        
        values$dataTemp <- leitura(url)
        data <- values$dataTemp
      }
      
      else
        values$data <- dataTemp
      
      if (length(data) < 1 || nrow(data) < 1) {
        shinyalert::closeAlert()
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "There are not corresponding IPv6 and IPv4 data for this parameters",
          type = "error"
        )
      }
      else{
        if ((is.null(listIDsAsn) ||
             listIDsAsn == "") &&
            (is.null(listIDsProbe) || listIDsProbe == "")) {
          data <- data[order(as.integer(data$ProbeID)), ]
        }
        else{
          if (!(is.null(listIDsProbe) || listIDsProbe == "")) {
            listIDsProbe <- strsplit(listIDsProbe, ";")
            listIDsProbe <- unique(listIDsProbe)
            data <-
              data[which(is.element(
                as.numeric(data$ProbeID),
                as.numeric(listIDsProbe[[1]])
              )), ]
          }
          if (!(is.null(listIDsAsn) || listIDsAsn == "")) {
            listIDsAsn <- strsplit(listIDsAsn, ";")
            listIDsAsn <- unique(listIDsAsn)
            data <-
              data[which(is.element(as.numeric(data$as), as.numeric(listIDsAsn[[1]]))), ]
            
          }
        }
        if (input$selectLocationTemp == 2) {
          coord <- as.numeric(input$selectRegionTemp)
          latmax <- coordenates[[coord]][1]
          latmin <- coordenates[[coord]][3]
          lonmin <- coordenates[[coord]][2]
          lonmax <- coordenates[[coord]][4]
          data <-
            data[which(
              as.numeric(data$lat) >= latmin &
                as.numeric(data$lat) <= latmax &
                as.numeric(data$lon) >= lonmin & as.numeric(data$lon) <= lonmax
            ), ]
        }
        if (input$selectLocationTemp == 3) {
          countriesList <- input$countriesTemp
          countriesInitials <-
            countries[which(is.element(countries$name, countriesList)), ]
          countriesList <- countriesInitials$countries
          prbCon <-
            probes[which(is.element(probes$Country, countriesList)), ]
          prbCountryList <- prbCon$ProbeID
          data <-
            data[which(is.element(
              as.numeric(data$ProbeID),
              as.numeric(prbCountryList)
            )), ]
        }
        
        if (length(data$ProbeID) == 0) {
          shinyalert::closeAlert()
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "There are not corresponding IPv6 and IPv4 data for this parameters",
            type = "error"
          )
        }
        else{
          probesList <- data
          probesList$resultado <- NULL
          
          if (length(data$ProbeI) > 10)
            data <- data[1:2, ]
          
          probesDisplayed <- data
          probesDisplayed <-
            data.frame(
              ProbeID = probesDisplayed$ProbeID,
              as =  probesDisplayed$as,
              Longitude =  probesDisplayed$lon,
              Latitude =  probesDisplayed$lat
            )
          values$probesDisplayed <- probesDisplayed
          
          displayedIDs <- c()
          for (i in probesDisplayed$ProbeID) {
            displayedIDs <- append(displayedIDs, which(probesList$ProbeID == i))
            
          }
          values$displayedIDs <- displayedIDs
          if (version == 4)
            v <- "v4"
          else
            v <- "v6"
          
          listOfMirrors <- NULL
          data <- findMirrors(data, values$hour, values$root, v)
          listOfMirrors <- as.character(data$mirror)
          values$listOfMirrors <- listOfMirrors
          
          data <- find(data, version)
          values$TasnT <- unlist(data$asn)
          
          values$perform <- performGraph(data, data, "Temp", version)
          
          
          if (version == 4) {
            nodes <- nodes(data, "v4", values$root)
            link  <- links(data, "v4", values$root)
            nodes$globalRota <- NULL
            v <- "v4"
          }
          else {
            nodes <- nodes(data, "v6", values$root)
            link  <- links(data, "v6", values$root)
            nodes$globalRota <- NULL
            v <- "v6"
            
          }
          subset <- nodes[1:length(data$ProbeID), "name"]
          auxMe <- data.frame(id = subset)
          auxMe$result <- data$resultado
          values$probesResult <- auxMe
          
          shinyalert::closeAlert() ## remove load Temp modal
          
          values$datasetTemp <- c(values$hour, values$root, values$version)
          values$irstclickTemp <- 1
          
          values$module <- 2
          
          output$all <- renderForceNetwork({
            printer(link, nodes)
          })
          
          observeEvent(input$button, {
            mapa <- values$probesDisplayed
            totalProbesMap(values$probesDisplayed)
          })
          
          observeEvent(input$buttonStat, {
            resT  <- values$listOfMirrors
            
            res <- as.data.frame(table(resT))
            res$per <- (res$Freq / length((resT))) * 100
            
            pDNS <-
              plot_ly(
                res,
                labels =  ~ resT ,
                values = ~ ~Freq,
                type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent'
              ) %>%
              layout(
                title = 'DNS Root Server',
                xaxis = list(
                  showgrid = FALSE,
                  zeroline = FALSE,
                  showticklabels = FALSE
                ),
                yaxis = list(
                  showgrid = FALSE,
                  zeroline = FALSE,
                  showticklabels = FALSE
                )
              )
            
            res <- as.data.frame(table(values$TasnT))
            res$per <- (res$Freq / length((res$Var1))) * 100
            
            as <- data.frame(AS=res$Var1, Frequency=res$Freq, Percentage= res$per)
            as <- as[order(as$Frequency),]
           
            
            
            showModal(modalDialog(
              title = paste("Probes Displayed Statistics"),
              fluidRow(column(2),
                       column(
                         8, renderTable(values$perform, rownames = TRUE)
                       ),
                       column(2)),
              fluidRow(
                column(6, renderPlotly(pDNS)),
                column(6, renderTable(as))
              ),
              
              easyClose = TRUE
            ))
            
          })
          output$count <- renderValueBox({
            valueBox(
              value = "Map",
              subtitle = HTML(
                "<b>Probes Used</b> <button id=\"button\" style=\"background-color:Transparent;width:100%;
                height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                class=\"btn btn-default action-button\"> </button>"
              ),
              icon = icon("globe")
            )
          })
          
          output$rate <- renderValueBox({
            valueBox(
              value = "Probes",
              subtitle = HTML(
                "<b>Statistics</b> <button id=\"buttonStat\" style=\"background-color:Transparent;width:100%;
                height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                class=\"btn btn-default action-button\"> </button>"
              ),
              icon = icon("signal")
            )
          })
          
          output$groupslist <- DT::renderDataTable(
            probesList,
            selection = list(mode = 'multiple', selected = values$displayedIDs),
            rownames = FALSE,
            escape = FALSE
          )
          
          output$users <- renderValueBox({
            valueBox(
              value = "Settings",
              subtitle = HTML(
                paste0(
                  length(values$displayedIDs),
                  " ",
                  "<b>Probes Displayed</b> <button id=\"reload\" style=\"background-color:Transparent;width:100%;
                  height:100%; position:absolute;top:0;left:0; z-index:11;\"type=\"button\"
                  class=\"btn btn-default action-button\"> </button>"
                )
              ),
              #subtitle =  paste("Date", " ",input$temp,"   ",input$slider_hoursTemp,"h","00min",sep=""),
              icon = icon("calendar")
            )
          })
          output$Refresh1 <- renderText({
            paste(
              "RIPE Atlas measurement to Root" ,
              " ",
              values$root,
              " ",
              "on:",
              " ",
              input$temp,
              "   ",
              input$slider_hoursTemp,
              "h",
              "00min",
              sep = ""
            )
          })
          
          
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          
          observeEvent(input$reload, {
            showModal(
              modalDialog(
                size = "m",
                title = "Probes Displayed",
                footer = NULL,
                easyClose = TRUE,
                DT::dataTableOutput("groupslist"),
                bsButton("attlist", style = "primary", label = "OK"),
                bsButton("clearAttlist",style = "default",label = "CLEAR")
              )
            )
          })
          
          
          observeEvent(input$attlist, {
            values$listRows <- input$groupslist_rows_selected
            listRows <- values$listRows
            if (length(listRows) == 0 || is.null(listRows) || length(listRows) > 10) {
              sendSweetAlert(session = session,
                             title = "Error",
                             text = "Invalid number of Probes. Please, select only 10 Probes",
                             type = "error")            
            }
            else{
              shinyalert(
                title = "Filtering data...",
                className = "alert",
                closeOnClickOutside = FALSE,
                animation = "pop",
                closeOnEsc = FALSE,
                showConfirmButton = FALSE,
                imageWidth = 120,
                imageHeight =  120,
                imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
                text = "Wait a moment, please!"
              )
              
              
              displayedIDs <- listRows
              probesDisplayed <- probesList[listRows, ]
              
              data <-
                values$dataTemp[which(is.element(
                  as.numeric(values$dataTemp$ProbeID),
                  as.numeric(probesDisplayed$ProbeID)
                )), ]
              data <- data[order(data$ProbeID), ]
              
              probesDisplayed <- data
              probesDisplayed <-
                data.frame(
                  ProbeID = probesDisplayed$ProbeID,
                  as =  probesDisplayed$as,
                  Longitude =  probesDisplayed$lon,
                  Latitude =  probesDisplayed$lat
                )
              values$probesDisplayed <- probesDisplayed
              displayedIDs <- c()
              for (i in probesDisplayed$ProbeID) {
                displayedIDs <- append(displayedIDs, which(probesList$ProbeID == i))
              }
              if (version == 4)
                v <- "v4"
              else
                v <- "v6"
              values$displayedIDs <- displayedIDs
              listOfMirrors <- NULL
              data <- findMirrors(data, values$hour, values$root, v)
              listOfMirrors <- as.character(data$mirror)
              values$listOfMirrors <- listOfMirrors
              
              data <- find(data, version)
              values$TasnT <- unlist(data$asn)
              values$perform <- performGraph(data, data, "Temp", version)
              
              if (version == 4) {
                nodes <- nodes(data, "v4", values$root)
                link  <- links(data, "v4", values$root)
                nodes$globalRota <- NULL
              }
              else{
                nodes <- nodes(data, "v6", values$root)
                link  <- links(data, "v6", values$root)
                nodes$globalRota <- NULL
              }
              subset <- nodes[1:length(data$ProbeID), "name"]
              auxMe <- data.frame(id = subset)
              auxMe$result <- data$resultado
              probesResult <- auxMe
              
              output$all <- renderForceNetwork({
                printer(link, nodes)
              })
              output$groupslist <- DT::renderDataTable(
                probesList,
                selection = list(
                  mode = 'multiple',
                  selected = values$displayedIDs
                ),
                rownames = FALSE,
                escape = FALSE
              )
              shinyalert::closeAlert()
              removeModal()
            }
           
          })
        }
      }
    }
    
  })
  
  
  observeEvent(input$hopsv4, {
    shinyalert(
      title = "Filtering data...",
      className = "alert",
      closeOnClickOutside = FALSE,
      animation = "pop",
      closeOnEsc = FALSE,
      showConfirmButton = FALSE,
      imageWidth = 120,
      imageHeight =  120,
      imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
      text = "Wait a moment, please!"
    )
    modalHopsMap(values$hour, values$hopsMapv4)
  })
  observeEvent(input$hopsv6, {
    shinyalert(
      title = "Filtering data...",
      className = "alert",
      closeOnClickOutside = FALSE,
      animation = "pop",
      closeOnEsc = FALSE,
      showConfirmButton = FALSE,
      imageWidth = 120,
      imageHeight =  120,
      imageUrl = "https://media.giphy.com/media/IPY93yWj5oKn6/giphy.gif",
      text = "Wait a moment, please!"
    )
    modalHopsMap(values$hour, values$hopsMapv6)
  })
  
  observeEvent(input$okProbe, {
    if (grepl("[^[:digit:] + ;]", input$captionProbe)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      updateButton(session, inputId = "btn_probe", style = "danger")
    }
    else{
      toggleModal(session, "modal_bnt_probe", toggle = "close")
      if (!input$captionProbe == "" && !input$captionProbe == " ")
        updateButton(session, inputId = "btn_probe", style = "success")
      else
        updateButton(session, inputId = "btn_probe", style = "bordered")
    }
  })
  observeEvent(input$okAsn, {
    if (grepl("[^[:digit:] + ;]", input$captionAsn)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      updateButton(session, inputId = "btn_asn", style = "danger")
    }
    else{
      toggleModal(session, "modal_bnt_asn", toggle = "close")
      if (!input$captionAsn == "" && !input$captionAsn == " ")
        updateButton(session, inputId = "btn_asn", style = "success")
      else
        updateButton(session, inputId = "btn_asn", style = "bordered")
    }
  })
  observeEvent(input$okLocation, {
    print(input$countries)
    toggleModal(session, "modal_bnt_location", toggle = "close")
    if (input$selectLocation == 1)
      updateButton(session, inputId = "btn_location", style = "bordered")
    else
      updateButton(session, inputId = "btn_location", style = "success")
    
  })
  
  observeEvent(input$clearAsn, {
    updateTextInput(session, "captionAsn", value = "")
    updateButton(session, inputId = "btn_asn", style = "bordered")
  })
  observeEvent(input$clearProbe, {
    updateTextInput(session, "captionProbe", value = "")
    updateButton(session, inputId = "btn_probe", style = "bordered")
  })
  observeEvent(input$clearLocation, {
    updateTextInput(session, "selectLocation", value = 1)
    updateButton(session, inputId = "btn_location", style = "bordered")
  })
  
  observeEvent(input$okProbeTemp, {
    if (grepl("[^[:digit:] + ;]", input$captionProbeTemp)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      updateButton(session, inputId = "btn_probeTemp", style = "danger")
    }
    else{
      toggleModal(session, "modal_bnt_probeTemp", toggle = "close")
      if (!input$captionProbeTemp == "" &&
          !input$captionProbeTemp == " ")
        updateButton(session, inputId = "btn_probeTemp", style = "success")
      else
        updateButton(session, inputId = "btn_probeTemp", style = "bordered")
    }
  })
  observeEvent(input$okAsnTemp, {
    if (grepl("[^[:digit:] + ;]", input$captionAsnTemp)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Invalid characters. Please insert only IDs separated by semicolon",
        type = "error"
      )
      updateButton(session, inputId = "btn_asnTemp", style = "danger")
    }
    else{
      toggleModal(session, "modal_bnt_asnTemp", toggle = "close")
      if (!input$captionAsnTemp == "" &&
          !input$captionAsnTemp == " ")
        updateButton(session, inputId = "btn_asnTemp", style = "success")
      else
        updateButton(session, inputId = "btn_asnTemp", style = "bordered")
    }
  })
  observeEvent(input$okLocationTemp, {
    toggleModal(session, "modal_bnt_locationTemp", toggle = "close")
    if (input$selectLocationTemp == 1)
      updateButton(session, inputId = "btn_locationTemp", style = "bordered")
    else
      updateButton(session, inputId = "btn_locationTemp", style = "success")
  })
  
  observeEvent(input$clearAsnTemp, {
    updateTextInput(session, "captionAsnTemp", value = "")
    updateButton(session, inputId = "btn_asnTemp", style = "bordered")
  })
  observeEvent(input$clearProbeTemp, {
    updateTextInput(session, "captionProbeTemp", value = "")
    updateButton(session, inputId = "btn_probeTemp", style = "bordered")
  })
  observeEvent(input$clearLocationTemp, {
    updateTextInput(session, "selectLocationTemp", value = 1)
    updateButton(session, inputId = "btn_locationTemp", style = "bordered")
    
  })
  observeEvent(input$clearAttlist,{
    values$listRows <- list()
    output$groupslist <- DT::renderDataTable(
      probesList,
      selection = list(mode = 'multiple', selected =  values$listRows),
      rownames = FALSE,
      escape = FALSE
    )
    
  })
  
  
  
  observe({
    data <- input$mydata
    if (!is.null(data)) {
      holder <- NULL
      node  <- strsplit(data, " ")
      ini <- substr(node[[2]][[1]], 1, 1)
      if (node[[1]][[1]] == "AS")
        modalAS(node[[1]][[2]])
      if (node[[1]][[1]] == "Probe")
        modalProbe(values$hour, node[[1]][[2]], data[[2]][[1]], values$probesResult, values$module)
      if (ini == "M")
        modalMirror(node[[1]][[1]], values$root)
    }
  })
  observeEvent(input$groupslist_rows_selected, {
    values$listRows <- input$groupslist_rows_selected
    
  })
  
  observeEvent(input$infoGlobeRegion,{
    showModal(
      modalDialog(
        size = "m",
        title = "Globe Region",
        
        HTML('<img style="width:100%" src = "https://www-static.ripe.net/static/rnd-ui/atlas/static/docs/area_controller_map.png">'),
        footer = NULL,
        easyClose = TRUE
        
        
      )
    )
    
  })
  observeEvent(input$home,{
    session$reload()
  })
  
  observeEvent(input$info,{
    showModal(
      modalDialog(
        size = "l",
        title = "CatchmentView",
        
        HTML('<p> CatchmentView is a ....</p>'),
        footer = NULL,
        easyClose = TRUE
        
        
      )
    )
  })
  
  observeEvent(input$contact,{
    showModal(
      modalDialog(
        size = "m",
        title = "Contact us",
        
        HTML('<p> Leonardo Costella - 134383(at) upf.br</p>'),
        HTML('<p> Marco Antonio Sandini Trentin - trentin(at) upf.br</p>'),
        HTML('<a href="http://www.ricardoschmidt.com" target="_blank"> Ricardo de Oliveira Schmidt </a> - rschmidt@upf.br '),
        footer = NULL,
        easyClose = TRUE
        
        
      )
    )
  })
  
 
  
})
