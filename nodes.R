nodes <- function(data, v, root){
  nodes  <- c()
  labels <- c()
  for (i in data$ProbeID){
    nodes <- append(nodes,paste("Probe",i,v,sep="")) 
    labels <- append(labels,paste("Probe",i,sep=" "))
  
  }
  ind = 1
  for (i in data$asn){
    #
    dns <- data$mirror[[ind]]
    
    if(is.na(i[1])){
     data$caminho[[ind]] <- NA
      ind = ind + 1
    }
    else{
      rota <- c(paste("Probe", data$ProbeID[ind],v,sep=""))
      for (j in 1:length(i)){
        
        nodes  <- append (nodes,paste("AS",i[j], v, sep="")) 
        
        labels <- append(labels, paste("AS",i[j],sep=" "))
        
        rota <- append(rota,paste("AS",i[j], v,sep=""))
        if (j == length(i)){
          nodes  <- append(nodes,paste("M",dns,v,sep=""))
          labels <- append(labels,dns) 
          rota   <- append(rota,paste("M",dns,v,sep=""))
        }
        
        
      }
      
      data$caminho[[ind]] <- rota
      ind = ind + 1
    }
  }
  data$caminho <- sapply(data$caminho,paste,collapse="//")
  nodes <- append(nodes, paste("ROOT", root))
  labels <- append(labels,paste("ROOT", root))
  nodes <- unique(nodes)
  labels <- unique(labels)
  
  labels <- paste(labels,collapse="&&")
  rota <- paste(data$caminho,collapse="&&")
  globalRota <- rota
  group <- paste(rota,labels,sep="**")
  
  nodes <- data.frame(name = nodes, group = group,globalRota = globalRota)
  return(nodes)
  
  
}