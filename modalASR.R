modalAS <- function(asn){
  for(i in 1:length(asnList$asn)){
    if(asnList$asn[[i]] == asn){
      holder<- asnList$holder[[i]]
      break
    }
  }
  if(is.null(holder) || is.na(holder)){
    showModal(modalDialog(
      title = paste("AS",asn, sep =" "),
      HTML(paste("<b>Holder:</b>", holder,"<hr style='margin: 0px;'>","<br>" )),
      easyClose = TRUE
    ))
  }
  else{
    
    showModal(modalDialog(
      title = paste("AS",asn, sep =" "),
      HTML(paste("<b>Holder:</b>", holder,"<hr style='margin: 0px;'>","<br>" )),
      tags$a(href=paste0("https://stat.ripe.net/",asn),target="_blank", "Click for more informations!"),
      easyClose = TRUE
    ))
    
  }
}
