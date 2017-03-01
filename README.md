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
- un modello Exponential Smoothing a stati di spazio (<strong>ETS</strong>)
- un modello Exponential Smoothing a stati di spazio con controllo dell'eteroschedasticità, residui ARMA e componenti di Trend e Stagionalità (<strong>TBATS</strong>)

La scelta degli ordini del modello ARIMA e delle eventuali componenti additive/moltiplicative del modello ETS è automatizzata per mezzo del package <strong>"forecast"</strong> di Rob J Hyndman. Per via della natura delle serie storiche da prevedere, la <strong>componente di trend</strong> (additiva o moltiplicativa) è sempre <strong>smorzata per logiche conservative</strong> nella previsione del traffico.

L'ensembling è, in questa prima fase, una semplice <strong>media aritmetica della stima puntuale<7strong> prodotta dai 3 modelli. Future evoluzioni del progetto potranno prevedere l'adozione di metodologie più robuste (http://stats.stackexchange.com/questions/47950/ensemble-time-series-model).
