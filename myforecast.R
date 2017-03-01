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
  
  ########################################
  ### Leggo il csv                       #
  ### Formato come da seguente schema    #
  ### |MeseAnno|Paese|Mezzo|Sessioni|    #
  ########################################
  
  csv <- read.csv(paste('FORECAST/',as.character(store),'/',as.character(file),'.csv', sep = ""), 
                  stringsAsFactors = FALSE)
  
  ### Trasformo il mese in data ed estraggo mese e anno in formato stringa
  csv[,1] <- as.Date(as.character(csv[,1]), format = "%Y%M")
  csv$mese <- month(csv[,1])
  csv$mese <- as.character(csv$mese)
  csv$year <- year(csv[,1])
  csv$year <- as.character(csv$year)
  
  
  
}