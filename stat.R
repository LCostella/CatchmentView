stat <- function(datav4,datav6,listOfMirrors,Tasnv4,Tasnv6,perform){
  resv6  <- listOfMirrors[1:length(datav6$ProbeID)]
  resv4  <- listOfMirrors[length(datav6$ProbeID)+1:length(listOfMirrors)]
 

  res <- as.data.frame(table(resv6))
  res <- res[order(res$Freq), ]
  pDNSv6 <- plot_ly(res, labels =~resv6 , values = ~Freq, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent')%>%
    layout(title = 'DNS Root Servers IPv6',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  

  res <- as.data.frame(table(resv4))
  res <- res[order(res$Freq), ]
  
  pDNSv4 <- plot_ly(res, labels =~resv4, values = ~Freq, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent')%>%
    
    layout(title = 'DNS Root Servers IPv4',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  res <- as.data.frame(table(Tasnv4))
  res$per <- (res$Freq/length((Tasnv4)))* 100
  res <- res[order(res$Freq,decreasing = TRUE), ]
  asv4 <- data.frame(AS=res$Tasnv4, Frequency=res$Freq, Percentage= res$per)
 
  res <- as.data.frame(table(Tasnv6))
  res$per <- (res$Freq/length((Tasnv6)))* 100
  res <- res[order(res$Freq,decreasing = TRUE), ]
  asv6 <- data.frame(AS=res$Tasnv6, Frequency=res$Freq, Percentage= res$per)
  
  showModal(modalDialog(
    title = paste("Probes Displayed Statistics"),
    #size ="m",
    fluidRow(
      column(2),
      column(8, renderTable(perform, rownames = TRUE)),
      column(2)),
    fluidRow(
      column(6,renderPlotly(pDNSv4)),
      column(6, renderPlotly(pDNSv6))),
    fluidRow(
      column(6,renderTable(asv4)),
      column(6, renderTable(asv6))),
      #renderPlotly(pDNSv6),
      #renderPlotly(pDNSv4),
    #renderPlotly(pASNv6),
    #renderPlotly(pASNv4),
    easyClose = TRUE
  ))
  
  
  
}