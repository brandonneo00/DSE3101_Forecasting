best_model = fitAR(65,"Q4",stat_gdp,1,8)
best_model


#print(best_model)
optimallag <- as.numeric(gsub("[^0-9]", "", best_model))
#print(optimallag)


referenceyear <- "ROUTPUT65Q4"
ref_column <- stat_gdp[, referenceyear] 
ref_column2 <- na.omit(ref_column)
FORECAST_HORIZON = 1

forecasting_gdp <- function(optimallag, data, referenceyear,FORECAST_HORIZON) {
  updated_series_data_no_na <- na.omit(data[, referenceyear])
  ar_model <- ar(updated_series_data_no_na, aic = TRUE, order.max = optimallag)
  next_quarter_prediction <- predict(ar_model, n.ahead = FORECAST_HORIZON)
  return(next_quarter_prediction)
}

forecasting_gdp(optimallag,stat_gdp, referenceyear,FORECAST_HORIZON)
