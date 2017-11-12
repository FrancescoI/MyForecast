### Preparo l'ambiente
library(tidyverse)
library(forecast)
library(forecastHybrid)
### library(forecastxgb)
library(lubridate)
### Personalizzare la working directory
setwd('C:/Users/imbrigliaf/Documents')


### Carico i dati dalla cartella Valentino.
### A tendere parametrizzare rispetto al nome dello store
d2 <- read.csv('Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/SmartStat/Dati/VALENTINO/valentino.csv',
               stringsAsFactors = FALSE)

### d2 (nome provvisorio) avrà la forma di:
###
### Date   Medium Country Device.Category Sessions
### 20160707 referral   China         desktop   176433
### 20170120 referral   China         desktop   165737
### 20170307 referral   China         desktop   139838
### 20170126 referral   China         desktop   116580
### 20170119 referral   China         desktop   113736
### 20170127 referral   China         desktop   112746 


### Carico i file di mappatura delle country e dei canali
geo <- read.csv("Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/SmartStat/Dati/geomapping.csv",
                stringsAsFactors = FALSE)

channel <- read.csv("Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/SmartStat/Dati/channelmapping.csv",
                    stringsAsFactors = FALSE,
                    sep = ";")

### Mappo le Country GA alle Country di BI. Mappo i medium GA alla riclassifica di canali che vogliamo per i budget.
### Per qualsiasi dubbio, guardarsi i file csv da cui dipendono i dataframe geo e channel
d2.full <- left_join(d2, geo, by = c("Country" = "CountryGA"))
d2.full <- left_join(d2.full, channel, by = c("Medium" = "mediumGA"))

### Quando la left join mi dà N/A, sostituisco con i valori di default
d2.full$CountryBP[which(is.na(d2.full$CountryBP))] <- 'OT non EU'
d2.full$mezzi[which(is.na(d2.full$mezzi))] <- 'Others'

### Converto la data da formato numeric a data
d2.full$Date <- as.Date(as.character(d2.full$Date), "%Y%m%d")

### Rimappo i Device, eliminando il tablet e aggregandolo in desktop
d2.full$Device <- ifelse(d2.full$Device.Category == 'mobile', 'mobile', 'desktop')

### Tolgo un po' di colonne inutili
d2.full.final <- d2.full %>% group_by(Date, CountryBP, mezzi, Device) %>% summarise(Sessions = sum(Sessions))

### Questo solo per non dover cambiare il codice sotto :)
d2 <- d2.full.final

### Ricavo Anno, Mese e AnnoMese dalla Data
d2$Year <- year(d2$Date)
d2$Month <- month(d2$Date)
d2$YearMonth <- ifelse(d2$Month < 10, paste(d2$Year,'0',d2$Month, sep = ''), paste(d2$Year,d2$Month, sep = ''))

### Aggrego mensilmente e mi salvo i risultati in un nuovo dataframe
d3 <- d2 %>% group_by(YearMonth, CountryBP, mezzi) %>% summarise(Sessions = sum(Sessions))

### Preparo il df finale dove avrò tutte le previsioni
final <- data.frame(Forecast = 0, Lower80 = 0, Higher80 = 0, Lower95 = 0, Higher95 = 0, Date = 'XXX', Channel = 'XXX', Country = 'XXX')

### Inizio il loop
for(country in unique(d3$CountryBP)) {
  for(medium in unique(d3$mezzi)){
    
    ### Preparo DF filtrato
    tmp <- d3 %>% filter(CountryBP == country, mezzi == medium)
    
    ### Verifico quanti datapoint per l'oggetto tmp
    if(nrow(tmp) < 5) {
      
      ### Se non ho row, non faccio nulla
      
    } else if(nrow(tmp) > 24) {
      
      ### Se ho almeno 25 datapoint, seguo il flusso normale
      ### Creo l'oggetto TS partendo dal min. date di tmp
      tmp.ts <- ts(tmp$Sessions, frequency = 12, start = c(as.numeric(c(substring(min(tmp$YearMonth), 1, 4), substring(min(tmp$YearMonth), 5, 7)))))
      
      ### Ripulisco la serie storica con la funzione tsclean
      tmp.ts <- tsclean(tmp.ts)
      
      ### HybridModel: Arima, ETS, STLM
      m <- hybridModel(tmp.ts, models = 'aes')
      
      ### Forecast su orizzonte temporale di 15 unità (mesi)
      p <- forecast(m, h = 15)
      
      ### Stampa a video a che punto siamo e plotto a schermo la previsione
      cat(paste('Finito FC per', medium, 'in', country, '\n'))
      plot(p, main = paste('Forecast di', medium, 'in', country))
      grid()
      
      ### Salvo la previsione in un dataframe df con appesi anche il canale e la country del ciclo for
      df <- data.frame(p, Date = row.names(as.data.frame(p)), Channel = rep(medium, 15), Country = rep(country, 15))
      
      ### RInomino le colonne
      names(df) <- c('Forecast', 'Lower80', 'Higher80', 'Lower95', 'Higher95', 'Date', 'Channel', 'Country')
      
      ### Append
      final <- rbind(final, df)
      
    } else {
      
      ### Se ho un numero compreso tra 1 e 24, uso solo ETS.
      ### I passaggi sotto sono gli stessi di prima
      ### A tendere ha senso scrivere una funzione per questa parte, parametrica rispetto al numero di datapoint
      ### Per ora ripetiamoci...
      
      tmp.ts <- ts(tmp$Sessions, f = 12, start = c(as.numeric(c(substring(min(tmp$YearMonth), 1, 4), substring(min(tmp$YearMonth), 5, 7)))))
      tmp.ts <- tsclean(tmp.ts)
      m <- ets(tmp.ts)
      p <- forecast(m, h = 15)
      cat(paste('Finito FC per', medium, 'in', country, '\n'))
      plot(p, main = paste('Forecast di', medium, 'in', country))
      grid()
      df <- data.frame(p, Date = row.names(as.data.frame(p)), Channel = rep(medium, 15), Country = rep(country, 15))
      names(df) <- c('Forecast', 'Lower80', 'Higher80', 'Lower95', 'Higher95', 'Date', 'Channel', 'Country')
      final <- rbind(final, df)
    }
  }
}

### TOlgo la prima riga
final <- final[-1, ]

### Scrivo l'output come csv
### write.csv(final, 'd2_forecast.csv', row.names = FALSE)
