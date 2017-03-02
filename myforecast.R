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
  library(plotly)
  
  
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
  csv[,1] <- as.Date(paste(as.character(csv[,1]), '01', sep = ""), format = "%Y%m%d")
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
  
  
  
  
  ##########################################
  ### Inizio Forecast di dati mensili,   ###
  ### aggregati per region e canale.     ###
  ###                                    ###
  ### Aggrego i dati riclassificati,     ###
  ### eseguo i 3 forecast,               ###
  ### combino e scrivo file in output    ###  
  ##########################################
  
  
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
  
  
  
  
  ####################################################################################
  ### Creo i modelli ETS, ARIMA e TBATS.                                             #
  ### Laddove possibile, applico il parametro damped = TRUE per smorzare la crescita.#
  ### NB: il dampening abbassa il fitting del modello, ma ci consente di proiettare  #
  ### i dati nel futuro in maniera più conservativa                                  #
  ####################################################################################
  
  
  ### Creo dataframe finale con le singole previsioni
  pred_all_total <- data.frame(data = seq(max(csv$Mese.dell.anno), by = "month", length.out = 25))
  
  
  ### Creo il modello ETS
  ets_all <- ets(all_ts, damped = TRUE, allow.multiplicative.trend = TRUE)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("ets_all")) {
    pred_all_ets <- forecast(ets_all)
    pred_all_total$ets <- c(1,as.vector(pred_all_ets$mean))
  }  
  
  
  ### Creo il modello ARIMA
  arima_all <- auto.arima(all_ts)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("arima_all")) {
    pred_all_arima <- forecast(arima_all)
    pred_all_total$arima <- c(1,as.vector(pred_all_arima$mean))
  }  
  
  
  ### Creo il modello TBATS
  tbats_all <- tbats(all_ts, use.box.cox = TRUE, use.damped.trend = TRUE)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("tbats_all")) {
    pred_all_tbats <- forecast(tbats_all)
    pred_all_total$tbats <- c(1,as.vector(pred_all_tbats$mean))
  }  
  
  
  ### Elimino prima riga
  pred_all_total <- pred_all_total[-1,]
  
  
  ### Creo media aritmetica dei 3 modelli
  pred_all_total$combined <- rowMeans(pred_all_total[,-1])
  
  
  ### Faccio l'append della serie storica predetta con quella di partenza
  combined <- data.frame(data =  pred_all_total$data, sessioni = pred_all_total$combined)
  final_all <- dplyr::union_all(as.data.frame(all), combined)
  
  
  ### Creo l'oggetto da plottare e lo eseguo
  q <- ggplot(final_all, aes(x=data, y=sessioni)) + geom_point() + geom_line() + theme_bw() + ggtitle("Forecast Overall")
  qq <- ggplotly(q)
  print(qq)
  
  
  
  ### Scrivo il forecast in output
  write.csv(final_all, paste("FORECAST/",store,"/all.csv", sep=""), row.names = FALSE)
  
  
  ### TODO
  ### 1. Importare dati con min. 24 mesi e applicare forecast (done!)
  ### 1.1 Salvare output su file ad hoc (done!)
  ### 2. Segmentare dati su base region
  ### 2.1 Per ogni region, segmentare su base channel
  ### 2.2 Forecastare ogni "item"
  ### 2.3 Salvare ogni forecast in formato |Mese|Region|Channel|Sessioni
  
  ### write.csv(csv, paste("FORECAST/",store,"/output.csv", sep=""), row.names = FALSE)
}
