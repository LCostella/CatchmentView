
link <- function(data, v, root){
  
  source <- c()
  target <- c()
 
  ind = 1
  
  for (i in data$asn){
    #print(measurement$ProbeID[[ind]])
    dns <- data$mirror[[ind]]
    
    if(is.na(i[1])){
      data$caminho[[ind]] <- NA
      ind = ind + 1
    }
    else{
     
      source <- append(source, paste("Probe", data$ProbeID[ind],v,sep=""))
      target <- append(target, paste("AS", data$asn[[ind]][[1]],v,sep=""))
      for (j in 1:length(i)){
        source <- append (source,paste("AS",i[j],v, sep=""))
       
        if (j == length(i)){
          
          target <- append(target,paste("M",dns,v,sep=""))
          source <- append (source,paste("M",dns,v,sep=""))
          target <- append(target,paste("ROOT", root))
        }
        else{
          target <- append(target,paste("AS",i[j+1],v, sep=""))
        }
        
      }
      
      ind = ind + 1
    }
  }
  link <- data.frame(source, target)
  return(link)
  
  
  
}
