myforecast <- function(file, store) {
  
  ### Installo i pacchetti minimi
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  
  ### Pulisco l'ambiente
  rm = list(ls())
  
  ########################################
  ### Leggo il csv                       #
  ### Formato come da seguente schema    #
  ### |MeseAnno|Paese|Mezzo|Sessioni|    #
  ########################################
  
  csv <- read.csv(paste('FORECAST/',as.character(store),'/',as.character(file),'.csv', sep = ""), 
                  stringsAsFactors = FALSE)
  
  
}