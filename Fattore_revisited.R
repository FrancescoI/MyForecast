### Preparo l'ambiente
library(tidyverse)
library(forecast)
library(forecastHybrid)
### library(forecastxgb)
library(lubridate)
### Personalizzare la working directory
setwd('C:/Users/imbrigliaf/Documents')
library(caret)
library(prophet)

### Leggo file raw
raw <- read_delim("Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/Analisi/Forecast/Valentino/raw.csv",
                  ",",
                  escape_double = FALSE, trim_ws = TRUE)


### Carico i file di mappatura delle country e dei canali
geo <- read.csv("Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/SmartStat/Dati/geomapping.csv",
                stringsAsFactors = FALSE)

channel <- read.csv("Y:/Shared Data/DIGITAL MARKETING IN SEASON/DM/Analisi e report/1. Analisi In Season/Web Analytics-Repository_All/Analisi/Forecast/Valentino/channelmapping.csv",
                    stringsAsFactors = FALSE,
                    sep = ";")



### Join e Mappatura
raw_m <- raw %>% left_join(geo, by = c('Country' = 'CountryGA')) %>% select(-Country) %>% select(-CountryBI) %>% select(-Region) %>%
  left_join(channel, by = c('Medium' = 'mediumGA')) %>% select(-Medium)

raw_m$CountryBP[which(is.na(raw_m$CountryBP))] <- 'OT non EU'
raw_m$mezzi[which(is.na(raw_m$mezzi))] <- 'Others'


raw_m$Device <- ifelse(raw_m$`Device Category` == 'mobile', 'mobile', 'desktop')
raw_m$`Device Category` <- NULL
raw_m$Date <- as.Date(as.character(raw_m$Date), format = '%Y%m%d')


### Leggo il calendario
calendar <- read.csv2('calendar.csv')
calendar$Data <- as.character(calendar$Data)
calendar$Date <- as.Date(calendar$Data, "%d/%m/%Y")

calendar$Data <- NULL
calendar$Country <- NULL

### Estraggo ulteriori feature
calendar$wday <- as.character(wday(calendar$Date))
calendar$mday <- as.character(mday(calendar$Date))
calendar$month <- as.character(month(calendar$Date))

### Filtro IT
raw_m_it <- raw_m %>% filter(CountryBP == 'IT')  %>% group_by(Date, CountryBP, mezzi) %>% summarise(Sessions = sum(Sessions)) 
final <- NULL


### Definisco periodo di holdout
hold <- 92

for(medium in unique(raw_m_it$mezzi)) {
  
  cat(paste('Starting from', medium, '-----------------------------------------|'))
  
  tmp <- raw_m_it %>% filter(mezzi == medium)

  ### Ottengo master
  master <- tmp %>% left_join(calendar, by = c('Date' = 'Date'))
  master$Sessions[which(is.na(master$Sessions))] <- 0
  
  ### Ricavo master2
  master2 <- tmp %>% left_join(calendar, by = c('Date' = 'Date'))
  master2$Sessions[which(is.na(master2$Sessions))] <- 0
  
  ### Droppo le variabili inutili per master
  master$CountryBP <- NULL
  master$Date <- NULL
  master$mezzi <- NULL
  
  ### Droppo le variabili inutili per master2
  master2$CountryBP <- NULL
  master2$mezzi <- NULL
  
  ### Splitto train e test per master
  train <- master[1:(nrow(master)-hold),]
  test <- master[(nrow(master)-hold+1):nrow(master), ]
  
  ### Splitto train e test per master2
  train2 <- master2[1:(nrow(master2)-hold),]
  test2 <- master2[(nrow(master2)-hold+1):nrow(master2), ]
  
  ### Splitto train e test per Arima
  ### master3 <- cbind(master[,-c(24,25,26)], predict(dummyVars(~ wday + mday + month, data = train), master)[,-c(1,8,50)])
  ### train3 <- master3[1:(nrow(master3)-hold),]
  ### test3 <- master3[(nrow(master3)-hold+1):nrow(master3), ]
  
  ### Modello con ARIMA
  ### m_a <- auto.arima(ts(train3$Sessions, f = 365), xreg = train3[,-1])
  
  ### Modello con RF
  ctrl <- trainControl(method = 'none')
  mtry <- 40
  tunegrid_rf <- expand.grid(.mtry = mtry)
  m_rf <- train(Sessions ~ ., data = train, method = 'rf', trControl = ctrl, tuneGrid = tunegrid_rf)
  print(m_rf$finalModel)
  
  ### Modello con XGBOOST
  ctrl_xgb <- trainControl(method = 'none')
  tunegrid_xgb <- expand.grid(nrounds = 150,
                              max_depth = 1,
                              eta = 0.3,
                              gamma = 0,
                              colsample_bytree = 0.8,
                              min_child_weight = 1,
                              subsample = 1)
  m_xgb <- train(Sessions ~ ., data = train, method = 'xgbTree', trControl = ctrl_xgb, tuneGrid = tunegrid_xgb)

  ### Modello con Prophet
  names(train2)[1] <- 'ds'
  names(train2)[2] <- 'y'
  m_p <- prophet(train2)
  names(test2)[1] <- 'ds'
  
  ### Prevedo RF, Prophet, XGBOOST e Arima
  p_rf <- predict(m_rf, test)
  p_p <- predict(m_p, test2[,-2])
  p_xgb <- predict(m_xgb, test)
  ### p_a <- forecast(m_a, xreg = test3[,-1])
  
  ### Calcolo Accuracy RF, Prophet e XGBOOST
  print(accuracy(p_rf, test$Sessions))
  print(accuracy(p_p$yhat, test2$Sessions))
  print(accuracy(p_xgb, test$Sessions))
  print(accuracy(data.frame(p_a)[,1], test$Sessions))
  
  ### Plotto RF
  par(mfrow = c(1,1))
  print(plot(train$Sessions, type = "l", main = medium))
  lines(m_rf$finalModel$predicted, col = 2)
  lines(predict(m_xgb, train), col = 3)
  print(varImpPlot(m_rf$finalModel))
  print(plot(varImp(m_xgb)))
  
  ### Plotto Prophet
  future <- make_future_dataframe(m_p, periods = 92)
  f_p <- predict(m_p, future)
  print(plot(m_p, f_p, main = medium))
  
  ### Plotto Arima
  ### plot(p_a)

  ### Faccio lo stacking dei modelli
  stacked <- data.frame(train, RF = m_rf$finalModel$predicted, Prophet = predict(m_p, train2[,-2])$yhat, XGB = predict(m_xgb, train))
  m_final <- train(Sessions ~ ., data = stacked, method = 'rf', trControl = ctrl)
  
  ### Faccio la previsione "stacked"
  preds <- data.frame(RF = p_rf, Prophet = p_p$yhat, XGB = p_xgb, test)
  p_final <- predict(m_final, preds)
  
  ### Creo dataframe
  final_tmp <- data.frame(PrevisioniRF = p_rf, PrevisioniProphet = p_p$yhat, PrevisioniXGBOOST = p_xgb , PrevisioniStacked = p_final, Actual = test$Sessions, Type = rep(medium, length(p_rf)))
  final <- rbind(final, final_tmp)
  
}
