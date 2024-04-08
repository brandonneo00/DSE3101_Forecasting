##forecasting values
best_model = fitAR(65,"Q4",stat_gdp,2,8)
optimallag <- as.numeric(gsub("[^0-9]", "", best_model))

forecasting_values <- function(vintage_year, vintage_quarter, df, forecast_horizon) {
  col_prefix = "ROUTPUT"  
  ref_col = paste(col_prefix, vintage_year, vintage_quarter, sep = "")  reference_columm = na.omit(df[,ref_col]) #cleaning the data to extract the reference column  
  reference_column = as.matrix(reference_columm)   test_data = embed(reference_column, optimallag + forecast_horizon)
  lag_names = paste("t", 0:(ncol(test_data)-1), sep = "-") #renaming to t-0 to t-optimallags for colnames  
  colnames(test_data) <- lag_names
  Y = as.matrix(test_data[,1]) #t-0 (1 column and 70 rows)  
  X = as.matrix(test_data[, -c(1:forecast_horizon)]) # matrix containing t-2,t-3 and t-4 (3 columns and 70 rows)  
  ar_model = lm(Y~X)  
  n_step_forecast <- predict(ar_model)  
  result = data.frame(n_step_forecast)  
  final_value = tail(result$n_step_forecast, 1) #retrieving the last value
  return(final_value)
}

a = forecasting_values(65,"Q4",stat_gdp,1)
print(a)