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


### Inizializzo la mia funzione
myforecast <- function(store, file, periods, cleaning.outlier = FALSE) {
  
  
  ### Installo i pacchetti minimi
  library(forecast)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
  library(forecastxgb)
  
  
  ### Setto l'ambiente di lavoro
  setwd("C:/Users/imbrigliaf/Documents")
  
  
  
  
  ##########################################
  ### Leggo il csv                       ###
  ### Formato come da seguente schema    ###
  ### |MeseAnno|Paese|Mezzo|Sessioni|    ###
  ### Trasformo le variabili             ###
  ### Riclassifico canali e paesi        ###
  ##########################################
  
  
  ### Leggo il csv parametrizzato sugli argomenti della mia funzione
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
  
  
  ### Pulisco gli outlier sulla base dell'argomento della funzione
  if(cleaning.outlier == TRUE) {
    all_ts <- tsclean(all_ts)
    print('Aggregated traffico: trying to clean outliers before modelling')
  }
  
  
  
  ###################################################################
  ### Creo i modelli ETS, ARIMA e TBATS.                          ###
  ### Faccio la previsione e salvo su un data.frame ad hoc.       ###
  ### Combino i 3 modelli con una semplice media aritmetica       ###
  ###################################################################
  
  
  ### Creo dataframe finale con le singole previsioni
  pred_all_total <- data.frame(data = seq(max(csv$Mese.dell.anno), by = "month", length.out = periods+1))
  
  
  ### Creo modello dati per fare storage accuracy dei 3 modelli
  accuracy_all <- list()
  
  
  ### Creo il modello ETS
  ets_all <- ets(all_ts)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("ets_all")) {
    pred_all_ets <- forecast(ets_all, h = periods)
    pred_all_total$ets <- c(1,as.vector(pred_all_ets$mean))
    print(ets_all)
    print(accuracy(ets_all))
    accuracy_all$ets <- accuracy(ets_all)[,2]
  }  
  
  
  ### Creo il modello ARIMA
  arima_all <- auto.arima(all_ts, lambda = 0.99)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("arima_all")) {
    pred_all_arima <- forecast(arima_all, h = periods)
    pred_all_total$arima <- c(1,as.vector(pred_all_arima$mean))
    print(arima_all)
    print(accuracy(arima_all))
    accuracy_all$arima <- accuracy(arima_all)[,2]
  }  
  
  
  ### Creo il modello TBATS
  tbats_all <- tbats(all_ts, use.box.cox = FALSE)
  
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("tbats_all")) {
    pred_all_tbats <- forecast(tbats_all, h = periods)
    pred_all_total$tbats <- c(1,as.vector(pred_all_tbats$mean))
    print(tbats_all)
    print(accuracy(tbats_all))
    accuracy_all$tbats <- accuracy(tbats_all)[,2]
  }  
  
  ### Creo il modello XGBoost
  xgb_all <- xgbar(all_ts, maxlag = 12, nrounds_method = 'cv')
  
  ## Creo funzione RMSE per XGBoost
  rmse_xgb <- function(a,b) {
    t <- (a - b)^2
    tt <- sum(t, na.rm = TRUE)
    l <- length(a) - length(which(is.na(a)))
    ttt <- tt/l
    ac <<- sqrt(ttt)
    return(ac)
  }
  
  ### Se non genero errore nella creazione del modello,
  ### Faccio la previsione e salvo su oggetto "_all"
  if(exists("xgb_all")) {
    pred_all_xgb <- forecast(xgb_all, h = periods)
    pred_all_total$xgb <- c(1,as.vector(pred_all_xgb$mean))
    print(xgb_all)
    accuracy_all$xgb <- rmse_xgb(xgb_all$fitted, all_ts)
  }  
  
  
  ### Elimino prima riga
  pred_all_total <- pred_all_total[-1,]
  
  
  ### Creo media aritmetica dei 3 modelli
  pred_all_total$combined <- rowMeans(pred_all_total[,-1])
  
  
  ### Creo i pesi dei 3 modelli sulla base dell'RMSE in-sample
  w_all <- lapply(accuracy_all, function(x){
    (1/x)/sum(1/(unlist(accuracy_all)))
  })
  
  
  ### Creo media ponderata su errore in-sample
  a <- data.frame(temp = rep(0, ncol(pred_all_total[,!names(pred_all_total) %in% c('data', 'combined')])))
  b <- data.frame(temp = rep(0, nrow(pred_all_total)))
  for(i in 1:length(pred_all_total$data)){
    for(j in 1:ncol(pred_all_total[,!names(pred_all_total) %in% c('data', 'combined')])){
      a[j,] <- (sum(w_all[[j]] * pred_all_total[i,!names(pred_all_total) %in% c('data', 'combined')][j]))
    }
    b[i,] <- colSums(a)
  }
  pred_all_total$combinedweightned <- b$temp
  names(pred_all_total$combinedweightned) <- 'combinedweightned'
  
  
  ### Approssimo all'unità
  pred_all_total[,-1] <- apply(pred_all_total[,-1], 2, function(x){round(x,0)})
  
  
  ### Da wide a long per creare il DB
  pred_all_total_long <- gather(pred_all_total, type, sessioni, -data)
  
  
  ### Riprendo i dati actual e aggiungo la colonna "type = actual"
  all <- mutate(all, type = "actual")
  
  
  ### Combino i dataset e creo DB finale
  final_all <- dplyr::union(all, pred_all_total_long)
  
  
  ### Creo l'oggetto da plottare e lo eseguo
  q <- ggplot(final_all, aes(x = data, y = sessioni, col = type)) + geom_point() + geom_line() + theme_bw() + ggtitle(paste("Forecast di traffico per",store))
  qq <- ggplotly(q)
  print(qq)
  
  
  ### Scrivo il forecast in output
  write.csv(final_all, paste("FORECAST/"
                             ,store
                             ,"/"
                             ,paste(year(Sys.Date()),month(Sys.Date())
                                    , sep = ""),"_forecast_all.csv", sep=""), row.names = FALSE)    
  
  
  
  ##########################################
  ### Inizio Forecast di dati mensili,   ###
  ### segmentati per region e canale.    ###
  ##########################################    
  
  
  ### Pulisco l'ambiente da variabili temporanee di prima
  rm(list = c("all"
              , "all_ts"
              , "pred_all_total"
              , "accuracy_all"
              , "ets_all"
              , "pred_all_ets"
              , "arima_all"
              , "pred_all_arima"
              , "tbats_all"
              , "pred_all_tbats"
              , "a"
              , "b"
              , "pred_all_total_long"))
  
  
  ### Creo funzione che fa forecast per canale
  x <- function(data) {
    
    ### Converto il dataframe in formato TS con f=12 e
    ### Start date uguale alla minima data disponibile per la serie storica
    if(length(data$sessioni) > 17) {
      data.ts <- ts(data$sessioni, frequency = 12, start = c(year(min(data$Mese.dell.anno)), month(min(data$Mese.dell.anno))))
    } else {
      data.ts <- ts(data$sessioni, frequency = 1, start = c(year(min(data$Mese.dell.anno)), month(min(data$Mese.dell.anno))))
    }
    
    ### Pulisco gli outlier sulla base dell'argomento della funzione
    if(cleaning.outlier == TRUE) {
      data.ts <- tsclean(data.ts)
      print('Segmented traffico: trying to clean outliers before modelling')
    }
    
    
    ### Creo dataframe finale con le singole previsioni
    pred_segment_total <- data.frame(data = seq(max(data$Mese.dell.anno), by = "month", length.out = periods+1))
    
    
    ### Creo modello dati per fare storage accuracy dei 3 modelli
    accuracy_segment <- list()
    
    
    ### Creo il modello ETS
    ets_segment <- ets(data.ts)
    
    
    ### Se non genero errore nella creazione del modello,
    ### Faccio la previsione e salvo su oggetto "_all"
    if(exists("ets_segment")) {
      pred_segment_ets <- forecast(ets_segment, h = periods)
      pred_segment_total$ets <- c(1,as.vector(pred_segment_ets$mean))
      accuracy_segment$ets <- accuracy(ets_segment)[,2]
    }  
    
    
    ### Creo il modello ARIMA
    arima_segment <- auto.arima(data.ts, lambda = 0.99)
    
    
    ### Se non genero errore nella creazione del modello,
    ### Faccio la previsione e salvo su oggetto "_segment"
    if(exists("arima_segment")) {
      pred_segment_arima <- forecast(arima_segment, h = periods)
      pred_segment_total$arima <- c(1,as.vector(pred_segment_arima$mean))
      accuracy_segment$arima <- accuracy(arima_segment)[,2]
    }  
    
    
    ### Creo il modello TBATS
    tbats_segment <- tbats(data.ts, use.box.cox = FALSE)
    
    
    ### Se non genero errore nella creazione del modello,
    ### Faccio la previsione e salvo su oggetto "_segment"
    if(exists("tbats_segment")) {
      pred_segment_tbats <- forecast(tbats_segment, h = periods)
      pred_segment_total$tbats <- c(1,as.vector(pred_segment_tbats$mean))
      accuracy_segment$tbats <- accuracy(tbats_segment)[,2]
    }  
    
    
    ### Creo il modello XGBoost
    xgb_segment <- xgbar(data.ts, maxlag = 12, nrounds_method = 'cv')
    
    
    ### Se non genero errore nella creazione del modello,
    ### Faccio la previsione e salvo su oggetto "_all"
    if(exists("xgb_segment")) {
      pred_segment_xgb <- forecast(xgb_segment, h = periods)
      pred_segment_total$xgb <- c(1,as.vector(pred_segment_xgb$mean))
      print(xgb_segment)
      accuracy_segment$xgb <- rmse_xgb(xgb_segment$fitted, data.ts)
    }  
    
    
    ### Elimino prima riga
    pred_segment_total <- pred_segment_total[-1,]
    
    
    ### Creo media aritmetica dei 3 modelli
    pred_segment_total$combined <- rowMeans(pred_segment_total[,-1])
    
    
    ### Creo i pesi dei 3 modelli sulla base dell'RMSE in-sample
    w_segment <- lapply(accuracy_segment, function(x){
      (1/x)/sum(1/(unlist(accuracy_segment)))
    })
    
    
    ### Creo media ponderata su errore in-sample
    a <- data.frame(temp = rep(0, ncol(pred_segment_total[,!names(pred_segment_total) %in% c('data', 'combined')])))
    b <- data.frame(temp = rep(0, nrow(pred_segment_total)))
    for(i in 1:length(pred_segment_total$data)){
      for(j in 1:ncol(pred_segment_total[,!names(pred_segment_total) %in% c('data', 'combined')])){
        a[j,] <- (sum(w_segment[[j]] * pred_segment_total[i,!names(pred_segment_total) %in% c('data', 'combined')][j]))
      }
      b[i,] <- colSums(a)
    }
    pred_segment_total$combinedweightned <- b$temp
    names(pred_segment_total$combinedweightned) <- 'combinedweightned'
    
    
    ### Approssimo all'unità
    pred_segment_total[,-1] <- apply(pred_segment_total[,-1], 2, function(x){round(x,0)})
    pred_segment_total <<- pred_segment_total
    
  }
  
  
  ### Creo lista di serie storiche
  my_series <- vector("list", length(unique(region)) * length(unique(medium)))
  
  
  ### Creo lista di previsioni in uscita
  my_forecasts <- vector("list", length(unique(region)) * length(unique(medium)))
  
  
  ### Creo loop per estrarre forecast mensili per ogni coppia region-medium
  i <- 1
  for(regions in unique(region)){
    for(mediums in unique(medium)) {
      my_series[[i]] <- select(csv, Mese.dell.anno, medium, region, Sessioni) %>% group_by(Mese.dell.anno, medium, region) %>% filter(region == regions, medium == mediums) %>% summarise(sessioni = sum(Sessioni))
      names(my_series)[i] <- paste(regions, "_", mediums, sep="") 
      x(my_series[[i]])
      my_forecasts[[i]] <- pred_segment_total
      names(my_forecasts)[i] <- paste(regions, "_", mediums, sep="")
      i <- i+1
    }
  }
  
  
  ### Trasformo il file originale affinché sia joinabile con DB forecast
  final_all <- select(csv, Mese.dell.anno, Sessioni, medium, region)
  final_all <- rename(final_all, data = Mese.dell.anno, sessioni = Sessioni)
  final_all <- mutate(final_all, type = "actual")
  
  
  ### Appendo iterativamente il file originale alle previsioni puntuali
  for(i in 1:length(my_forecasts)){
    tmp <- gather(my_forecasts[[i]], type, sessioni, -data) %>% mutate(region = strsplit(names(my_forecasts[i]), "_")[[1]][1], medium = strsplit(names(my_forecasts[i]), "_")[[1]][2])
    final_all <- dplyr::union(final_all, tmp)
  }
  
  write.csv(final_all, paste("FORECAST/"
                             ,store
                             ,"/"
                             ,paste(year(Sys.Date()),month(Sys.Date())
                                    , sep = ""),"_forecast_segmented.csv", sep=""), row.names = FALSE)
  
}
