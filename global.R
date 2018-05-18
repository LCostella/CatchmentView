library(devtools)
library(shinydashboard)
library(jsonlite)
library(triebeard)
library(leaflet)
library(doParallel)
library(geosphere)
library(DT)
library(shiny)
library(shinyalert)
library(shinyBS)
library(curl)
library(devtools)
library(networkD3)
library(googledrive)
library(ggplot2)
library(plotly)
library(plogr)
#devtools::install_github("LCostella/NetworkD3")

countries <- NULL

url <-"20180215.json"
json <- fromJSON(url)

# activity_url <- "https://ftp.ripe.net/ripe/atlas/probes/archive/meta-latest"
# temp <- tempfile()
# download.file(activity_url, temp)
# json <- fromJSON(temp)
probes <- data.frame(ProbeID = json$objects$id, IPv4prefix = json$objects$prefix_v4, 
                      IPv6prefix = json$objects$prefix_v6, Latitude = json$objects$latitude, 
                      Longitude = json$objects$longitude, Country = json$objects$country_code,
                      asnv4 = json$objects$asn_v4, asnv6 = json$objects$asn_v6,
                      IPv4 = json$objects$address_v4,IPv6 = json$objects$address_v6)   

countries <- probes$Country
countries <- data.frame((table(countries)))
countries <- countries[which(countries$countries!=""),]
cntr <- NULL
cntr <- read.csv2("country.csv")
countries$name <- NULL
for (i in 1:length(countries$countries)){
  for(j in 1:length(cntr$A2..ISO.)){
    if(as.character(countries$countries[i]) == as.character(cntr$A2..ISO.[[j]])){
      countries$name[[i]] <- as.character(cntr$COUNTRY[j])
      break}
  }
}


root <- read.table("b.csv", sep = ";",header = T) 
bb <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
bModal <-read.csv("bModal.csv",sep=";",header = T)

root <- read.table("c.csv", sep = ";",header = T) 
cc <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
cModal <-read.csv("cModal.csv",sep=";",header = T)

root <- read.table("f.csv", sep = ";",header = T) 
f <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
fModal <-read.csv("fModal.csv",sep=";",header = T)

root <- read.table("i.csv", sep = ";",header = T) 
i <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
iModal <-read.csv("iModal.csv",sep=";",header = T)

root <- read.table("k.csv", sep = ";",header = T) 
k <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
kModal <-read.csv("kModal.csv",sep=";",header = T)

root <- read.table("l.csv", sep = ";",header = T) 
l <- data.frame (Name = root$Town, lat = root$Latitude, lon = root$Longitude, IPv4 = root$IPv4, IPv6 = root$IPv6)
lModal <-read.csv("lModal.csv",sep=";",header = T)

ASNv6 <- read.csv(file = "ip2asnV6.csv", sep = ";",header = T) 
trie_v6 <- trie(keys = "000", values = "fake")
trie_add(trie_v6, keys = as.character(ASNv6$start), values = as.character(ASNv6$row))
ip2asn <- ASNv6
ASNv6 <- NULL

ASNv4 <- read.csv("ip2asnv4.csv", sep = ";",header = T)
trie_v4 <- trie(keys = "000", values = "fake")
trie_add(trie_v4, keys = as.character(ASNv4$start), values = as.character(ASNv4$row))
ip2asnV4 <- ASNv4
ASNv4 <- NULL

asnList <- read.csv2("ASNList.csv")
asnList <- data.frame(asn = asnList$autonomous_system_number,holder = asnList$autonomous_system_organization)

