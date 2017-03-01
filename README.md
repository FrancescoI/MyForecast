# Bulk Forecasting

Obiettivo la creazione di un'utility in grado di forecastare in automatico, in maniera bulk, 9x4 serie storiche mensili di traffico.

9 canali:
- Paid Search
- Retargeting
- Affiliation
- Display
- Email Marketing
- Organic
- Direct
- Referral
- Others

4 region:
- US
- EU
- APAC
- Others

La funzione si occupa di:
- leggere un csv da cartella specifica per store
  - il csv dovrà essere uno scarico dati unsampled da GA su specifica predefinita
- riclassificare "mezzi" e "paesi" secondo la tassonomia predefinita
- forecastare i dati mensili overall
- segmentare le serie storiche e forecastare per coppia canale+region
- scrivere in output due file
  - forecast aggregato
  - DB con forecast per singola coppia canale+region
