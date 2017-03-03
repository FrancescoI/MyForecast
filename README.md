# Bulk Forecasting

Obiettivo del progetto è la creazione di un'<strong>utility</strong> in grado di <strong>gestire in automatico la riclassifica di dati</strong> Google Analytics secondo una tassonomia predefinita a monte, <strong>la segmentazione dei dati</strong> su dimensioni primarie e <strong>la previsione a 12/18 mesi</strong> delle singole serie storica.

La tassonomia predefinita prevede 9 canali per 4 region, secondo il seguente schema:

<strong>9 canali</strong>: Paid Search, Retargeting, Affiliation, Display, Email Marketing, Organic, Direct, Referral, Others

<strong>4 region</strong>: US, EU, APAC, Others

L'esigenza alla base del progetto è duplice:
- adozione di <strong>metodologie statistiche</strong> atte al forecast di serie storiche di traffico
- <strong>speed-up del processo di budgeting e forecasting</strong>, eliminando la dipendenza dai tempi di lavorazione dell'analista

L'output previsto per questo progetto è uno script composto da (1) una parte alta di configurazione dell'utility (nome cartella e nome file) e (2) una funzione preposta al caricamento dei dati, trasformazione degli stessi ed esecuzione delle previsioni numeriche.

Il prodotto finale prevede la generazione di due file:
- un forecast topdown, su 12/18 mesi, del traffico totale mese su mese
- un "db" con i forecast per singola coppia canale+region, anch'essi su 12/18 mesi e mese su mese

I forecast prevedono l'ensembling di tre tecniche distinte:
- un modello <strong>ARIMA</strong>
- un modello Exponential Smoothing a spazio di stato (<strong>ETS</strong>)
- un modello Exponential Smoothing a spazio di stato con controllo dell'eteroschedasticità, residui ARMA e componenti di Trend e Stagionalità (<strong>TBATS</strong>)

La scelta degli ordini del modello ARIMA e delle eventuali componenti additive/moltiplicative del modello ETS è automatizzata per mezzo del package <strong>"forecast"</strong> di Rob J Hyndman. 

Il forecast produce due output sulla base di due differenti metodi di ensembling dei modelli:
- una <strong>media aritmetica</strong> dei forecast ETS, ARIMA e TBATS
- una <strong>media ponderata sull'errore in-sample</strong> dei 3 modelli (RMSE)

Future evoluzioni della funzione potranno prevedere logiche più sofisticate nel criterio di ensembling (CV ed errori out-of-sample).
