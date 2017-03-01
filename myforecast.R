### Creo lista di country
mycountry <- list()

### Creo i vettori per la riclassifica delle country
mycountry$eu <- c("Italy","France","Germany","United Kingdom","Spain","Russia","Netherlands","Greece","Belgium","Switzerland","Poland","Czech Republic","Romania","Slovakia","Austria","Serbia","Denmark","Ukraine","Croatia","Norway","Sweden","Hungary","Portugal","Bulgaria","Ireland","Slovenia","Finland","Estonia","Luxembourg","Lithuania","Belarus","Montenegro","Bosnia & Herzegovina","Andorra","Latvia","Moldova","Macedonia (FYROM)","Albania","Kosovo","Jersey","San Marino","Malta","Iceland","Monaco","Guernsey","Liechtenstein","Gibraltar","Faroe Islands","Svalbard & Jan Mayen","Isle of Man","?land Islands")
mycountry$apac <- c("Turkey", "Japan","Israel","Taiwan","India","China","Hong Kong","South Korea","United Arab Emirates","Singapore","Lebanon","Indonesia","Kazakhstan","Saudi Arabia","Thailand","Kuwait","Cyprus","Iran","Pakistan","Georgia","Malaysia","Qatar","Philippines","Vietnam","Jordan","Bangladesh","Azerbaijan","Sri Lanka","Iraq","Palestine","Bahrain","Armenia","Oman","Cambodia","Nepal","Mongolia","Uzbekistan","Afghanistan","Maldives","Syria","Macau","Kyrgyzstan","Myanmar (Burma)","Turkmenistan","Brunei","Yemen","Tajikistan","Laos","Bhutan","Timor-Leste")
mycountry$us <- c("United States", "Canada")

### Creo lista di canali
mychannel <- list()

### Creo i vettori per la riclassifica dei canali
mychannel$paidsearch <- c("cpc")
mychannel$retargeting <- c("retargeting", "shopping")
mychannel$display <- c("cpm","display","DISPLAY")
mychannel$affiliation <- c("affiliazione", "affiliation")
mychannel$email <- c("email")
mychannel$direct <- c("(none)")
mychannel$organic <- c("organic")
mychannel$referral <- c("referral")

myforecast <- function(file, store) {
  
  ### Installo i pacchetti minimi
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  
  ### Pulisco l'ambiente
  rm = list(ls())
  
  ### Setto l'ambiente di lavoro
  setwd("C:/Users/imbrigliaf/Documents")
  
  ##########################################
  ### Leggo il csv                       ###
  ### Formato come da seguente schema    ###
  ### |MeseAnno|Paese|Mezzo|Sessioni|    ###
  ### Trasformo le variabili             ###
  ### Riclassifico canali e paesi        ###
  ##########################################
  
  csv <- read.csv(paste('FORECAST/',as.character(store),'/',as.character(file),'.csv', sep = ""), 
                  stringsAsFactors = FALSE)
  
  ### Trasformo il mese in data ed estraggo mese e anno in formato stringa
  csv[,1] <- as.Date(as.character(csv[,1]), format = "%Y%M")
  csv$mese <- month(csv[,1])
  csv$mese <- as.character(csv$mese)
  csv$year <- year(csv[,1])
  csv$year <- as.character(csv$year)
  
  ### Riclassifico i canali
  for(i in 1:length(csv[,3])){
    for(j in 1:length(mychannel)){
      if(csv[,3][i] %in% unlist(mychannel(j))){
        csv$medium[i] <- names(mychannel[j])
      } 
    }
  }
  
  ### Riclassifico i paesi
  for(i in 1:length(csv[,2])){
    for(j in 1:length(mycountry)){
      if(csv[,2][i] %in% unlist(mycountry(j))){
        csv$region[i] <- names(mycountry[j])
      } 
    }
  }
 
  
}