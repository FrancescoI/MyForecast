### Pulisco l'ambiente
rm(list=ls())

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
mychannel$email <- c("email", "newsletter")
mychannel$direct <- c("(none)")
mychannel$organic <- c("organic")
mychannel$referral <- c("referral")

myforecast <- function(store, file) {
  
  ### Installo i pacchetti minimi
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  
  
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
  medium <- c()
  for(i in 1:length(csv[,2])){
    for(j in 1:length(mychannel)){
      if(csv[,2][i] %in% unlist(mychannel[j])){
        medium[i] <- names(mychannel[j])
      } 
    }
       if(is.na(medium[i])) {
         medium[i] <- "others"
       }
  }
  csv$medium <- medium
  
  
  ### Riclassifico i paesi
  region <- c()
  for(i in 1:length(csv[,3])){
    for(j in 1:length(mycountry)){
      if(csv[,3][i] %in% unlist(mycountry[j])){
        region[i] <- names(mycountry[j])
      } 
    }
    if(is.na(region[i])) {
      region[i] <- "others"
    }
  }
  csv$region <- region
  
  
  ### Aggrego i dati per mese e salvo in nuova variabile
  all <- select(csv, Mese.dell.anno, Sessioni) %>% group_by(Mese.dell.anno) %>% summarise(sessioni = sum(Sessioni))
  all <- rename(all, data = Mese.dell.anno)
  
  
  ### Trovo la data minima ed estraggo anno e mese
  min_year <- year(min(all$data))
  min_month <- month(min(all$data))
  
  
  ### Creo l'oggetto all_ts di classe "ts"
  ### Mese e Anno di partenza v. sopra
  ### Frequenza 12 mesi
  all_ts <- ts(all$sessioni, start = c(min_year, min_month), frequency = 12)
  print(all_ts)
  
  ### TODO
  ### 1. Importare dati con min. 24 mesi e applicare forecast
  ### 1.1 Salvare output su file ad hoc
  ### 2. Segmentare dati su base region
  ### 2.1 Per ogni region, segmentare su base channel
  ### 2.2 Forecastare ogni "item"
  ### 2.3 Salvare ogni forecast in formato |Mese|Region|Channel|Sessioni
  
  ### write.csv(csv, paste("FORECAST/",store,"/output.csv", sep=""), row.names = FALSE)
}
