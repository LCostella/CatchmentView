
modalProbe <- function(mirror,root){
 if(root== "K"){
   dataMirror <- kModal
 }
  if(root== "L"){
    dataMirror <- lModal
  }
  if(root== "B"){
    dataMirror <- bModal
  }
  if(root== "C"){
    dataMirror <- cModal
  }
  if(root== "F"){
    dataMirror <- fModal
  }
  if(root== "I"){
    dataMirror <- iModal
  }
  

display <- dataMirror[which(as.character(dataMirror$id)==as.character(mirror)),]

showModal(modalDialog(
    title = paste0("Root Server"," ",root,"-",display$id),
      HTML(paste("<b>Local </b>",display$Town,",", display$Country,"<hr style='margin: 0px;'>","<br>",
                  "<b>IPv4 </b>",display$IPv4,"<hr style='margin: 0px;'>","<br>",
                  "<b>IPv6 </b> ",display$IPv6,"<hr style='margin: 0px;'>","<br>", 
                  "<b>Latitude </b>", display$Latitude,"<hr style='margin: 0px;'>", "<br>",
                  "<b>Longitude </b>", display$Longitude,"<hr style='margin: 0px;'>", "<br>")),
    
    tags$a(href=display$site,target="_blank", "Click for more informations!"),
    
    
    
    
    # #title = paste("Root Server", root,"-",display$id, sep =" "),
    # paste("Local:", display$Town,",", display$Country, sep =""),
    # HTML("<br>"),
    # paste("IPv4:", display$IPv4, sep =" "),
    # HTML("<br>"),
    # paste("IPv6:", display$IPv6, sep =" "),
    # HTML("<br>"),
    # paste("Latitude", display$Latitude,sep=" "),
    # HTML("<br>"),
    # paste("Longitude", display$Longitude,sep=" "),
    # HTML("<br>"),
    # tags$a(href=display$site, "Click for more informations!"),
    # 
    # HTML("<br>"),
    # 
    
    easyClose = TRUE
  ))
  
}