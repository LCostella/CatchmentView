findASNv6 <- function(ip,time){
  
  asn <- NA
  
  if(startsWith(as.character(ip), "::" )){
    return("None")
  }
  else{
    if(endsWith(as.character(ip), "::" )){
      splitIP <- strsplit(as.character(ip),"::")[[1]]
      splitIP <- paste(splitIP,":0:0:",sep ="")
      splitIP <- strsplit(as.character(splitIP),":")[[1]] 
    }
    else{
      splitIP <- strsplit(as.character(ip),"::")[[1]]
      splitIP <- paste(splitIP,collapse =":0:0:")
      splitIP<- strsplit(as.character(splitIP),":")[[1]] 
    }
    if(time == 1){
      second <-splitIP[2]
      try({
        second <- substr(second,1,2)
      })
      find <- paste(splitIP[1],second,sep = ":")
    }
    else{
      second <- splitIP[1]
      find <- paste(second,":",sep = "")
    }
    
    a <-greedy_match(trie_v6,find) [[1]]
    for ( i in a){
      ind <- as.numeric(i)
      
      if(is.na(ind) || ip2asn$asn[ind] == "None" || ip2asn$asn[ind] == "0" ){
        
      }
      else{
        if(endsWith(as.character(ip2asn$start[ind]), "::" )){
          splitS <- strsplit(as.character(ip2asn$start[ind]),"::")[[1]]
          splitS <- paste(splitS,":0:0:",sep ="")
        }
        else{
          splitS <- strsplit(as.character(ip2asn$start[ind]),"::")[[1]]
          splitS <- paste(splitS,collapse =":0:0:")
        }
        splitS <- strsplit(as.character(splitS),":") [[1]]
        
        
        if(endsWith(as.character(ip2asn$end[ind]), "::" )){
          splitE <- strsplit(as.character(ip2asn$end[ind]),"::")[[1]]
          splitE <- paste(splitE,":0:0:",sep="")
          
        }
        else{
          splitE <- strsplit(as.character(ip2asn$end[ind]),"::")[[1]]
          splitE <- paste(splitE,collapse =":0:0:")
        }
        splitE <- strsplit(as.character(splitE),":") [[1]]
        
        if(length(splitE) - length(splitS) > 0)
          splitS <- append(splitS,rep(0,length(splitE) - length(splitS)))
        
        flag = TRUE
        menor <-  FALSE
        maior <- FALSE
        for(k in 1:length(splitIP)){
          if(is.na(splitS[k]) || is.na(splitE[k]) || is.na(splitIP[k])){
            flag = FALSE
            break
          }
          if((menor == TRUE ||  (strtoi(splitS[k],base = 16L) <= strtoi(splitIP[k],base = 16L))) && (maior == TRUE || (strtoi(splitE[k],base = 16L) >= strtoi(splitIP[k],base = 16L))) )
          {
            if(strtoi(splitS[k],base = 16L) < strtoi(splitIP[k],base = 16L))
              menor <- TRUE
            if(strtoi(splitE[k],base = 16L) > strtoi(splitIP[k],base = 16L))
              maior <- TRUE
          }
          else{
            flag = FALSE
            break
          }
        }
        
        if(flag == TRUE){
          asn <- (as.character(ip2asn$asn[[as.numeric(i)]]))
          
          return(asn)
        }
        
      }
    }

  }
  return(asn) 
}

findASNv4 <- function(ip,time){
  asn <- NA
  splitIP <- strsplit(as.character(ip),"[.]")[[1]]
  if(time == 1)
    find <- paste(splitIP[1],splitIP[2],splitIP[3], sep= ".")
  if(time == 2)
    find <- paste(splitIP[1],splitIP[2], sep= ".")
  if(time == 3)
    find <- paste(splitIP[1], sep= ".")
  
  find <- paste(find,".",sep="")
  a <-greedy_match(trie_v4,find) [[1]]
  
  for ( i in a){
    
    ind <- as.numeric(i)
    
    if(as.character(ip2asnV4$asn[ind]) == "None" || as.character(ip2asnV4$asn[ind]) == "0"){
      
    }
    else{
      splitS <- strsplit(as.character(ip2asnV4$start[ind]),"[.]")[[1]]
      splitE <- strsplit(as.character(ip2asnV4$end[ind]),"[.]")[[1]]
      
      flag = TRUE
      menor = FALSE
      maior =FALSE
      
      for(k in 1:length(splitIP)){
        if(is.na(splitS[k]) || is.na(splitE[k]) || is.na(splitIP[k])){
          flag = FALSE
          break
        }
        if((menor == TRUE ||  (as.numeric(splitS[k]) <= as.numeric(splitIP[k]))) && (maior == TRUE || (as.numeric(splitE[k]) >= as.numeric(splitIP[k]))) )
        {
          if(as.numeric(splitS[k]) < as.numeric(splitIP[k]))
            menor <- TRUE
          if(as.numeric(splitE[k]) > as.numeric(splitIP[k]))
            maior <- TRUE
        }
        else{
          flag = FALSE
          break
        }
      }
      
      if(flag == TRUE){
        asn <- (as.character(ip2asnV4$asn[[as.numeric(i)]]))
        return(asn)
      }
    }
    
  }
  
 return(asn) 
}

findAsn <- function(data,v){
  resource <- "None"
  print(paste("Buscando ASNs IPv", v, sep =" "))
  local = c("10.","127.","172.16.","192.168.")
  ind =1
  for ( i in data$resultado){
    data$asn[[ind]] <- list()
    data$asn[[ind]] <- append(data$asn[[ind]], data$as[[ind]])
    iplist <- NULL
    iplist <- c()
    for(ipi in i){
      ipi$ip
      iplist <- append(iplist,ipi$ip)
    }
    
    for (j in iplist){
      
      if(j == iplist[length(iplist)])
        break
     
      if(v==4){
        l <- (startsWith(as.character(j), local )) 
        
        if ( as.character(j) != "*" && (!is.element(TRUE, l)) && (!startsWith(as.character(j),resource)) ){
            resource <- strsplit(j,"[.]")[[1]]
            resource <- paste(resource[1],resource[2],resource[3],sep=".")
            asn <- findASNv4(j,1)
            if(is.na(asn))
              asn <- findASNv4(j,2)
            if(is.na(asn))
              asn <- findASNv4(j,3)
            if(!is.null(asn) && (asn !='None') && (!is.na(asn)) && (asn != "0") ){
              data$asn[[ind]] <- append(data$asn[[ind]], asn)
            }
            
        }
      }
            
      else{
        
        if (as.character(j) != "*" && (!is.null(j)) && (!startsWith(as.character(j),resource)) ){
          
          resource <- strsplit(as.character(j),":")[[1]]
          resource <- paste(resource[1],resource[2],resource[3],sep=":")
          asn <- findASNv6(j,1)
          if(is.na(asn))
            asn <- findASNv6(j,2)
          
          if(!is.null(asn) && (asn !='None') && (!is.na(asn)) ){
              data$asn[[ind]] <- append(data$asn[[ind]], asn)
          }
              
            
        }
        
      }
    }
    data$asn[[ind]] <- unique(as.character(data$asn[[ind]]))
    ind = ind + 1
   
  }   
  
  return(data)
}  