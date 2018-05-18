totalProbesMap <- function(data){

  showModal(modalDialog(
    #tags$head(tags$style(".modal-dialog{ width:95%}")),
    size = c("l"),
    title = paste("Probes Map"),
    renderLeaflet({
      
      m <-leaflet() %>%
        setView(lng=52.3875,lat=4.8875, zoom = 1)
      addTiles(m) %>%
       
      addMarkers(lng=as.double(as.character(data$Longitude)),lat=as.double(as.character(data$Latitude)),popup= paste("Probe", data$ProbeID, sep =" "))
     
       
    }),
    tags$head(tags$style(".leaflet-marker-icon leaflet-zoom-animated leaflet-clickable{ width:12px}")),   
    easyClose = TRUE
  ))
  
  
}