# Bulk Forecasting

High-level function for forecasting daily web traffic (Google Analytics data), using an ensembling of multiple inferential and ML techniques:

* Arima and Exponential Smoothing methods
* Random Forest (or any other ML regressor available in CARET package)
* Prophet (by Facebook)

Starting from daily data segmented by country, device and medium, geo and channel data are encoded in order to reduce cardinality and to be compliant with YNAP OFS taxonomy (12 countryies, 9 channels).

Then forecasting models are applied to every 9x12 time series data, in order to produce a final dataframe with historical and forecasted data as well.
