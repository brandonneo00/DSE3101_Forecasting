#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)

library(BVAR)
library(readxl) 
library(dplyr)
library(zoo)
library(DT)

library(dynlm)

# Read the Excel file using the correct file path
path_to_data = "../ROUTPUTQvQd.xlsx"
gdp_raw <- read_excel(path_to_data, col_names = TRUE)
# Convert all columns except the first one (which contains dates) to numeric, keeping NA values as NA 
gdp_num <- gdp_raw[, 2:ncol(gdp_raw)]
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-gdp_raw[,1]


# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))

# Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)

stat_df = data.frame(gdp_date, stat_gdp)
stat_df$DATE = zoo::as.yearqtr(stat_df$DATE, format = "%Y:Q%q")
stat_df

earliest_year = min(as.integer(format(stat_df$DATE, "%Y")))
latest_year = max(as.integer(format(stat_df$DATE, "%Y")))


test = zoo::as.yearqtr("2022 Q2")
seq_dates = seq(test, length.out = 8, by = 1/4)
seq_dates

stat_df %>% 
  select(DATE, ROUTPUT24Q1) %>%
  filter(DATE %in% seq_dates)

# getting the last available vintage for comparing against fan chart
library(stringr)
get_last_available_vintage = function(stat_df_w_date){
  last_available_datapt = stat_df_w_date %>%
    select(DATE) %>%
    tail(1) %>%
    pull()
  last_available_vintage = seq(last_available_datapt, length.out = 2, by = 1/4)[2]
  last_available_vintage
  last_available_vintage_year = str_split(last_available_vintage, pattern=" ")[[1]][1]
  last_available_vintage_year_prefix = substr(last_available_vintage_year, start = 3, stop = 4)
  last_available_vintage_quarter = str_split(last_available_vintage, pattern=" ")[[1]][2]
  col_prefix = "ROUTPUT"
  last_available_vintage_col_name = paste(col_prefix, last_available_vintage_year_prefix, last_available_vintage_quarter, sep="")
  return(last_available_vintage_col_name)
}

library(reticulate)
#py_install("pandas")
#py_install("scikit-learn")
#py_install("matplotlib")
#py_install("openpyxl")
reticulate::source_python("RandomForestTS.py")
x1 = "2023"
x2 = "Q1"
x3 = "8Q"
test = rf(x1, x2, x3)
test


#reticulate::source_python("rf_backtesting.py")
#rf_backtest = back_pred("2010", "Q3", "8Q", as.integer(5))
#rf_backtest
#View(rf_backtest)
#rf_backtest[1]


# function to find optimal lags for AR
fitAR = function(vintage_year, vintage_quarter, df, forecast_horizon, max_lags){
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")
  #print(reference_col)
  
  subset_df = as.matrix(df %>%
                          select(reference_col))
  
  #subset_df$DATE = zoo::as.yearqtr(subset_df$DATE, format = "%Y:Q%q")
  #subset_df = subset_df %>%
  #  mutate_if(is.character, as.double)
  
  aux = embed(subset_df, (max_lags + forecast_horizon))
  aux = aux[complete.cases(aux), ]
  y = aux[, 1]
  X = data.frame(aux[, -c(1:forecast_horizon)])
  X_mat = as.matrix(aux[,-c(1:forecast_horizon)])
  
  train_df = data.frame(y, X)
  
  if (forecast_horizon == 1){ #only for forecast horizon = 1
    all_models_aic = list()
    all_models = list()
    for (i in (1:max_lags)){
      formula = as.formula(paste("y ~", paste0("X", 1:i, collapse = " + ")))
      model_fit = lm(formula, data = train_df)
      model_aic = AIC(model_fit)
      
      label = paste("AR", i, sep="")
      all_models_aic[label] = model_aic
      all_models[label] = model_fit 
    }
    best_model_key = names(which.min(unlist(all_models_aic)))
    best_model_lag = which.min(unlist(all_models_aic)) #optimal lag for forecast horizon 1
    best_model = all_models[best_model_lag] 
    return(best_model_lag)
    
  } else{ #for beyond 1 step ahead forecast
    all_models_loocv_mse = list()
    
    for (i in 1:max_lags){
      #which AR model we testing
      label = paste("AR", i, sep="")
      
      #creating the X matrix based on AR model
      subset_X_mat = as.matrix(X_mat[, 1:i])
      subset_X_mat = cbind(1, subset_X_mat) #adding a col of 1s for intercept term
      
      #calculating estimated coefficients
      beta_hat = solve(t(subset_X_mat) %*% subset_X_mat) %*% (t(subset_X_mat) %*% y)
      
      #predicted value
      y_hat = subset_X_mat %*% beta_hat
      
      #residuals
      residual = y - y_hat
      
      #projection matrix
      projection_mat = subset_X_mat %*% solve(t(subset_X_mat) %*% subset_X_mat) %*% t(subset_X_mat)
      
      #diagonal of H
      hii = diag(projection_mat)
      
      #computing LOOCV MSE
      counter = 0
      for (j in 1:length(hii)){
        e_tilda = ((1 - hii[j])^-1) * residual[j]
        counter = counter + e_tilda**2
      }
      loocv_mse = counter / length(y)
      all_models_loocv_mse[label] = loocv_mse
    }
    best_model_key = names(which.min(unlist(all_models_loocv_mse)))
    best_model_lag = which.min(unlist(all_models_loocv_mse))
    formula = as.formula(paste("y ~", paste0("X", 1:best_model_lag, collapse = " + ")))
    best_model = lm(formula, data = train_df)
  }
  #return(best_model_lag)
  return(as.numeric(gsub("[^0-9]", "", best_model_lag)))
}

ar_test = fitAR(65, "Q4", stat_gdp, 2, 8)
ar_test

# Variation of fitAR model that returns the model itself
fitAR_model = function(vintage_year, vintage_quarter, df, forecast_horizon, max_lags){
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")

  subset_df = as.matrix(df %>%
                          select(reference_col))

  aux = embed(subset_df, (max_lags + forecast_horizon))
  aux = aux[complete.cases(aux), ]
  y = aux[, 1]
  X = data.frame(aux[, -c(1:forecast_horizon)])
  X_mat = as.matrix(aux[,-c(1:forecast_horizon)])
  
  train_df = data.frame(y, X)
  
  if (forecast_horizon == 1){ #only for forecast horizon = 1
    all_models_aic = list()
    all_models = list()
    for (i in (1:max_lags)){
      formula = as.formula(paste("y ~", paste0("X", 1:i, collapse = " + ")))
      model_fit = lm(formula, data = train_df)
      model_aic = AIC(model_fit)
      
      label = paste("AR", i, sep="")
      all_models_aic[label] = model_aic
      all_models[label] = model_fit 
    }
    best_model_key = names(which.min(unlist(all_models_aic)))
    best_model_lag = which.min(unlist(all_models_aic)) #optimal lag for forecast horizon 1
    best_model = all_models[best_model_lag] 
    #return(best_model_lag)
    
  } else{ #for beyond 1 step ahead forecast
    all_models_loocv_mse = list()
    
    for (i in 1:max_lags){
      #which AR model we testing
      label = paste("AR", i, sep="")
      
      #creating the X matrix based on AR model
      subset_X_mat = as.matrix(X_mat[, 1:i])
      subset_X_mat = cbind(1, subset_X_mat) #adding a col of 1s for intercept term
      
      #calculating estimated coefficients
      beta_hat = solve(t(subset_X_mat) %*% subset_X_mat) %*% (t(subset_X_mat) %*% y)
      
      #predicted value
      y_hat = subset_X_mat %*% beta_hat
      
      #residuals
      residual = y - y_hat
      
      #projection matrix
      projection_mat = subset_X_mat %*% solve(t(subset_X_mat) %*% subset_X_mat) %*% t(subset_X_mat)
      
      #diagonal of H
      hii = diag(projection_mat)
      
      #computing LOOCV MSE
      counter = 0
      for (j in 1:length(hii)){
        e_tilda = ((1 - hii[j])^-1) * residual[j]
        counter = counter + e_tilda**2
      }
      loocv_mse = counter / length(y)
      all_models_loocv_mse[label] = loocv_mse
    }
    best_model_key = names(which.min(unlist(all_models_loocv_mse)))
    best_model_lag = which.min(unlist(all_models_loocv_mse))
    formula = as.formula(paste("y ~", paste0("X", 1:best_model_lag, collapse = " + ")))
    best_model = lm(formula, data = train_df)
  }
  return(best_model)
  #return(as.numeric(gsub("[^0-9]", "", best_model_lag)))
}

# function that returns the optimal ADL model
find_quarter_year <- function(input_vec){
  return_list <- list()
  first_non_na_index <- which(!is.na(input_vec))[1]
  start_year <- 1947
  quarters_per_year <- 4 
  actual_year <- start_year + (first_non_na_index-1) %/% quarters_per_year
  actual_quarter <- (first_non_na_index-1)%% quarters_per_year + 1
  return_list$year <- actual_year
  return_list$quarter <- actual_quarter
  return(return_list)
}



fit_adl <- function(vintage_year, vintage_quarter, routput_df, hstart_df, forecast_horizon, max_lags){
  
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")
  column_index <- which(names(routput_df) == reference_col)
  
  subset_routput_df = as.matrix(routput_df[,column_index])
  subset_hstart_df = match_quarter_to_month(reference_col, hstart_df)
  
  routput_start <- find_quarter_year(routput_df[,column_index])
  hstart_start <- find_quarter_year(subset_hstart_df)
  # print(subset_hstart_df)
  routput_ts <- ts(na.omit(subset_routput_df), start=c(routput_start$year,routput_start$quarter), frequency = 4)
  hstart_ts <- ts(na.omit(subset_hstart_df), start=c(hstart_start$year, hstart_start$quarter), frequency=4)
  
  optimal_model <- list()
  
  if(forecast_horizon == 1){
    ar_model <- fitAR_model(vintage_year, vintage_quarter, routput_df, forecast_horizon, max_lags)
    optimal_ar_lag <- as.numeric(summary(ar_model)[1])-1
    aic <- numeric(max_lags)
    for(i in 1:max_lags){
      model <- dynlm(routput_ts ~ L(routput_ts, 1:optimal_ar_lag) + L(hstart_ts, 1:i))
      aic[i] <- AIC(model)
    }
    best_lag_hstart <- which.min(aic)
    best_model <- dynlm(routput_ts ~ L(routput_ts, 1:optimal_ar_lag) + L(hstart_ts, 1:best_lag_hstart))
    optimal_model$routput_lag <- optimal_ar_lag
    optimal_model$hstart_lag <- best_lag_hstart
    optimal_model$model <- best_model
  }
  else if (forecast_horizon >= 2){
    all_models_loocv_mse = list()
    
    for(i in 1:max_lags){
      label = paste("ADL", i, sep="")
      #print(label)
      
      aux = embed(subset_hstart_df, (max_lags + forecast_horizon))
      aux = aux[complete.cases(aux), ]
      print(aux)
      X = data.frame(aux[, -c(1:forecast_horizon)])
      X_mat = as.matrix(aux[,-c(1:forecast_horizon)])
      
      subset_X_mat_hstart = as.matrix(X_mat[, 1:i])
      
      aux = embed(subset_routput_df, (max_lags + forecast_horizon))
      aux = aux[complete.cases(aux), ]
      y_sub = aux[, 1]
      X = data.frame(aux[, -c(1:forecast_horizon)])
      X_mat = as.matrix(aux[,-c(1:forecast_horizon)])
      subset_X_mat_routput = as.matrix(X_mat[, 1:i])
      
      num_rows_routput <- dim(subset_X_mat_routput)[1]
      num_rows_hstart <- dim(subset_X_mat_hstart)[1]
      #print(subset_X_mat_routput)
      
      if(num_rows_routput > num_rows_hstart){
        # print("here1")
        X_mat_routput = subset_X_mat_routput[1:num_rows_hstart,]
        y = y_sub[1:num_rows_hstart]
        X_mat_hstart = subset_X_mat_hstart
        
        
        
      }
      else if (num_rows_routput < num_rows_hstart){
        print("here2")
        X_mat_hstart = subset_X_mat_hstart[1:num_rows_routput,]
        y = y_sub
        X_mat_routput = subset_X_mat_routput
        
      }
      else{
        X_mat_routput = subset_X_mat_routput
        y=y_sub
        X_mat_hstart = subset_X_mat_hstart
      }
      
      
      combined_matrix<- cbind(1, X_mat_routput, X_mat_hstart)
      
      # aux = embed(subset_routput_df, (max_lags + forecast_horizon))
      # aux = aux[complete.cases(aux), ]
      # y = aux[, 1]
      
      cond_value_inv <- kappa(t(combined_matrix)%*% combined_matrix)
      
      if(cond_value_inv >1e10){
        pseudo_inv <- ginv(t(combined_matrix)%*% combined_matrix)
      }
      else{
        reg_inv <- solve(t(combined_matrix)%*% combined_matrix)
        
      }
      
      beta_hat = solve(t(combined_matrix) %*% combined_matrix) %*% (t(combined_matrix) %*% y)
      
      #predicted value
      y_hat = combined_matrix %*% beta_hat
      
      #residuals
      residual = y - y_hat
      
      #projection matrix
      projection_mat = combined_matrix %*% solve(t(combined_matrix) %*% combined_matrix) %*% t(combined_matrix)
      
      #diagonal of H
      hii = diag(projection_mat)
      
      counter = 0
      for (j in 1:length(hii)){
        e_tilda = ((1 - hii[j])^-1) * residual[j]
        counter = counter + e_tilda**2
      }
      loocv_mse = counter / length(y)
      all_models_loocv_mse[label] = loocv_mse
      
      
      
    }
    
    best_model_key = names(which.min(unlist(all_models_loocv_mse)))
    best_model_lag_hstart = which.min(unlist(all_models_loocv_mse))
    # print(best_model_lag_hstart)
    
    
    model_ar = fitAR_model(vintage_year, vintage_quarter, routput_df, forecast_horizon, max_lags)
    best_model_lag_routput = length(coef(model_ar))-1
    best_model <- dynlm(routput_ts ~ L(routput_ts, 1:best_model_lag_routput) + L(hstart_ts, 1:best_model_lag_hstart))
    # print(best_model)
    
    optimal_model$routput_lag <- best_model_lag_routput
    optimal_model$hstart_lag <- best_model_lag_hstart
    optimal_model$model <- best_model
    
  }
  
  return(optimal_model)
  
}

# importing additional data of predictors needed for ADL
hstart <- read_excel('../hstartsMvMd.xlsx')
hstart <- hstart %>%
  mutate(across(-DATE, ~ifelse(. == "#N/A", NA_real_, as.numeric(as.character(.)))))
hstart_quarterly <- hstart %>%
  mutate(DATE = as.yearqtr(DATE, format = "%Y:%m"), # Convert DATE to quarterly format
         DATE = format(DATE, "%Y:Q%q")) %>% # Adjust the format to "YYYY:QQ"
  group_by(DATE) %>%
  summarize(across(everything(), ~if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)), .groups = "drop")
hstart_num <- hstart_quarterly[, 2:ncol(hstart_quarterly)] 
hstart_date <- hstart_quarterly[,1]
transformation_codes <- rep(5, ncol(hstart_num))  # Apply the fred_transform function with the transformation codes 
hstart_gdp <- fred_transform(data = hstart_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)

match_quarter_to_month <- function(quarter_col, hstart_df) {
  # Extract year and quarter from the quarter column name
  year <- substr(quarter_col, 8, 9)
  quarter <- substr(quarter_col, 10, 11)
  
  # Define a mapping between quarters and months
  quarter_to_month <- c("Q1" = "M3", "Q2" = "M6", "Q3" = "M9", "Q4" = "M12")
  
  # Extract the corresponding month based on the quarter
  month <- quarter_to_month[quarter]
  # print(year)
  # print(month)
  
  # Construct the column name in hstart_df
  hstart_col <- paste("HSTARTS", year, month, sep = "")
  # print(hstart_col)
  
  # Return the subset of hstart_df
  return(hstart_df[, hstart_col])
}


adl <- fit_adl("73", "Q3", stat_gdp, hstart_gdp, 2, 6)

convert_to_full_year <- function(lastTwoDigits){
  yearSuffix <- as.numeric(lastTwoDigits)
  fullYear <- ""
  
  if(yearSuffix >= 65 && yearSuffix <= 99){
    fullYear <- paste("19", lastTwoDigits, sep="")
  }
  else if (yearSuffix >= 00 && yearSuffix <= 23){
    fullYear <- paste("20", lastTwoDigits, sep="")
  }
  return(fullYear)
}

getADLForecast <- function(vintage_year, vintage_quarter, routput_gdp, hstart_gdp, forecast_horizon, optimal_lag_routput, optimal_lag_hstart) {
  col_prefix = "ROUTPUT"  
  ref_col = paste(col_prefix, vintage_year, vintage_quarter, sep = "")  
  reference_columm = na.omit(routput_gdp[,ref_col]) #cleaning the data to extract the reference column  
  reference_column = as.matrix(reference_columm) 
  test_data_routput = embed(reference_column, optimal_lag_routput + forecast_horizon)
  nrow_routput = nrow(test_data_routput)
  Y = as.matrix(test_data_routput[,1]) #t-0 (1 column and 70 rows)  
  
  
  # print(test_data_routput)
  
  reference_column_hstart = match_quarter_to_month(ref_col, hstart_gdp)
  reference_column_hstart_cleaned = as.matrix(na.omit(reference_column_hstart))
  test_data_hstart = embed(reference_column_hstart_cleaned, optimal_lag_hstart + forecast_horizon)
  nrow_hstart = nrow(test_data_hstart)
  
  if(nrow_routput > nrow_hstart){
    final_routput = tail(test_data_routput, nrow_hstart)
    final_hstart = test_data_hstart
    Y_final = tail(Y, nrow_hstart)
    
  }
  else if(nrow_routput < nrow_hstart){
    
    final_hstart = tail(test_data_hstart, nrow_routput)
    print(final_hstart)
    final_routput = test_data_routput
    Y_final = Y
  }
  else{
    final_routput = test_data_routput
    final_hstart = test_data_hstart
    Y_final = Y
  }
  
  
  
  
  # print(test_data_hstart)
  
  
  lag_names_routput = paste("t", 0:(ncol(test_data_routput)-1), sep = "-") #renaming to t-0 to t-optimallags for colnames  
  colnames(test_data_routput) <- lag_names_routput
  
  lag_names_hstart = paste("t", 0:(ncol(test_data_hstart)-1), sep = "-" )
  colnames(test_data_hstart) <- lag_names_hstart
  
  
  
  X_routput = as.matrix(final_routput[, -c(1:forecast_horizon)]) # matrix containing t-2,t-3 and t-4 (3 columns and 70 rows)  
  X_hstart = as.matrix(final_hstart[, -c(1:forecast_horizon)])
  
  
  combined_matrix = cbind(X_routput, X_hstart)
  
  adl_model = lm(Y_final~combined_matrix)  
  n_step_forecast <- predict(adl_model, interval="prediction")  
  result = data.frame(n_step_forecast)  
  #print(result)
  final_value = tail(result, 1) #retrieving the last value
  return(final_value)
}

adl_forecast = getADLForecast(10, "Q4", stat_gdp, hstart_gdp, 1, adl$routput_lag, adl$hstart_lag)
adl_forecast
adl_forecast1 = getADLForecast(10, "Q4", stat_gdp, hstart_gdp, 2, adl$routput_lag, adl$hstart_lag)
adl_forecast1
adl_forecast$fit

predict_function <- function(model, optimal_lag_routput, optimal_lag_hstart, routput_values, hstart_values){
  #print(routput_values)
  intercept <- coef(model)[1]
  coefficients_routput <- coef(model)[2:(2+optimal_lag_routput-1)]
  coefficients_hstart <- coef(model)[(2+optimal_lag_routput):length(coef(model))]
  prediction <- intercept + sum(coefficients_routput * routput_values) + sum(coefficients_hstart * hstart_values)
  return(prediction)
  
}

ADLbacktest <- function(vintage_year, vintage_quarter, optimal_adl_model, routput_gdp, routput_date, hstart_gdp, hstart_date, forecast_horizon, max_lags){
  #adl <- fit_adl(vintage_year, vintage_quarter, routput_gdp, hstart_gdp, forecast_horizon, max_lags)
  model <- optimal_adl_model$model
  optimal_routput_lag <- optimal_adl_model$routput_lag
  optimal_hstart_lag <- optimal_adl_model$hstart_lag
  full_year <- convert_to_full_year(vintage_year)
  starting_index_string <- paste(full_year, ":", vintage_quarter, sep="")
  print(starting_index_string)
  row_index <- which(routput_date$DATE == starting_index_string)
  
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")
  column_index <- which(names(routput_gdp) == reference_col) #which column is the onw we want 
  
  routput_column = routput_gdp[,column_index]
  hstart_column = match_quarter_to_month(reference_col, hstart_gdp)
  
  last_column = routput_gdp[,ncol(routput_gdp)]
  prediction_values <- c()
  actual_values <- c()
  
  starting_row_index <- row_index - 50 - forecast_horizon
  count <- 0 
  return_list <- list()
  while(count < 50){
    hstart_values <- c()
    routput_values <- c()
    
    for(i in 1:optimal_routput_lag){
      starting_row_routput <- starting_row_index + count #loop through thr start of routput predictors 
      routput_values <- c(routput_values, routput_column[starting_row_routput-forecast_horizon-i]) 
      #print(routput_values)
    }
    
    for(j in 1:optimal_hstart_lag){
      starting_row_hstart <- starting_row_index+count
      hstart_values <- c(hstart_values, hstart_column[starting_row_hstart-forecast_horizon-i])
    }
    
    prediction <- predict_function(model=model, optimal_lag_routput=optimal_routput_lag, optimal_lag_hstart=optimal_hstart_lag, routput_values=routput_values, hstart_values=hstart_values)
    actual_values <- c(actual_values, last_column[starting_row_index+count])
    prediction_values <- c(prediction_values, prediction)
    count = count + 1
  }
  mse <- mean((actual_values-prediction_values)^2)
  return_list$predictions <- prediction_values
  rmse <- sqrt(mse)
  return_list$rmse <-rmse
  return_list$actual_values <- actual_values
  return(return_list)
  ### 
}

#adl_backtest1 = ADLbacktest("13", "Q3", stat_gdp, gdp_date, hstart_gdp, hstart_date, 1, 6)
#adl_backtest1

routput <- read_excel('../ROUTPUTQvQd.xlsx')
routput <- routput %>%
  mutate(across(-DATE, ~ifelse(. == "#N/A", NA_real_, as.numeric(as.character(.)))))
routput_num <- routput[, 2:ncol(routput)] 
routput_date <- routput[,1]
transformation_codes <- rep(5, ncol(routput_num))  # Apply the fred_transform function with the transformation codes 
routput_gdp <- fred_transform(data = routput_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)

#adl_backtest2 = ADLbacktest("13", "Q4", stat_gdp, gdp_date, hstart_gdp, hstart_date, 2, 6)
#adl_backtest2

#ADLbacktest("13", "Q4", routput_gdp, routput_date, hstart_gdp, hstart_date, 2, 6)
#mse = 0.518391

# function to make forecasts
getForecast <- function(vintage_year, vintage_quarter, df, forecast_horizon, optimal_ar_lag) {
  col_prefix = "ROUTPUT"  
  ref_col = paste(col_prefix, vintage_year, vintage_quarter, sep = "")  
  reference_columm = na.omit(df[,ref_col]) #cleaning the data to extract the reference column  
  reference_column = as.matrix(reference_columm)   
  test_data = embed(reference_column, optimallag + forecast_horizon)
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

##forecasting values
best_model = fitAR(65,"Q4",stat_gdp,2,8)
optimallag <- as.numeric(gsub("[^0-9]", "", best_model))


a = getForecast(65,"Q4",stat_gdp,1)
print(a)

ui <- fluidPage(
  
  # Application title
  titlePanel("Macroeconomic Forecasting of Real GDP"),
  
  theme = shinythemes::shinytheme("united"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year", min=earliest_year, max=latest_year, step=1, value = 2010),
      selectInput("quarter", "Select Quarter", choices=c("Q1", "Q2", "Q3", "Q4")),
      selectInput("alpha", "Select Alpha for Fan Chart", choices=c("50%", "80%", "90%"), selected="50%"),
      selectInput("model_choice", "Choose Model to show", choices = c("AR", "ADL", "Random Forest", "Combined", "Most Optimal"), selected = "AR")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series", plotlyOutput("line_plot"), textOutput("plot_description")),
        tabPanel("Table", DT::DTOutput("dt_table")),
      )
      
    )
  ),
  actionButton("show_about", "About")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$line_plot = renderPlotly({
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    if (input$alpha == "50%") {
      alpha = 0.5
    } else if (input$alpha == "80%"){
      alpha = 0.2
    } else if (input$alpha == "90%"){
      alpha = 0.1
    }
    
    print(reference_col)
    
    subset_df = stat_df %>%
      select(DATE, reference_col) %>%
      filter(complete.cases(.)) %>% #removing the "future" rows
      tail(16) %>% #showing the last 16 quarters of data 
      mutate(Category = "A")
    
    #print(subset_df)
    
    reference_quarter_numeric = as.numeric(substr(input$quarter, star = nchar(input$quarter), stop = nchar(input$quarter)))
    if (reference_quarter_numeric == 1){
      x_intercept_quarter = 4
      x_intercept_quarter = paste("Q", x_intercept_quarter)
      x_intercept_year = input$year - 1
    } else{
      x_intercept_quarter = reference_quarter_numeric - 1
      x_intercept_quarter = paste("Q", x_intercept_quarter)
      x_intercept_year = input$year
    }
    
    x_intercept = zoo::as.yearqtr(paste(x_intercept_year, x_intercept_quarter, sep=" "))
    x_intercept_numeric = as.numeric(x_intercept) 
    
    
    # showing the "true" values for 8 steps ahead from user's vintage point
    
    # very last col available in the data 
    #true_value_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    #true_value_seq_dates = seq(true_value_start_date, length.out = 8, by = 1/4)
    
    true_value_start_date = zoo::as.yearqtr(paste(x_intercept_year, x_intercept_quarter, sep=" "))
    true_value_seq_dates = seq(true_value_start_date, length.out = 9, by = 1/4)
    
    last_available_vintage = get_last_available_vintage(stat_df)
    true_df = stat_df %>% 
      select(DATE, last_available_vintage) %>%
      filter(DATE %in% true_value_seq_dates) %>%
      mutate(Category = "B")
    
    print(true_df)
    
    # making point forecasts using AR model
    ar_one_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 1, 8)
    ar_two_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 2, 8)
    ar_three_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 3, 8)
    ar_four_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 4, 8)
    ar_five_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 5, 8)
    ar_six_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 6, 8)
    ar_seven_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 7, 8)
    ar_eight_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, 8, 8)
    
    ar_one_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 1, ar_one_step_best_model_lag)
    ar_two_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 2, ar_two_step_best_model_lag)
    ar_three_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 3, ar_three_step_best_model_lag)
    ar_four_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 4, ar_four_step_best_model_lag)
    ar_five_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 5, ar_five_step_best_model_lag)
    ar_six_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 6, ar_six_step_best_model_lag)
    ar_seven_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 7, ar_seven_step_best_model_lag)
    ar_eight_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, 8, ar_eight_step_best_model_lag)
    
    forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
    ar_forecast_df = data.frame(DATE = forecast_seq_dates, 
                                point_forecast = c(ar_one_step_ahead_point_forecast, 
                                                   ar_two_step_ahead_point_forecast, 
                                                   ar_three_step_ahead_point_forecast, 
                                                   ar_four_step_ahead_point_forecast, 
                                                   ar_five_step_ahead_point_forecast, 
                                                   ar_six_step_ahead_point_forecast,
                                                   ar_seven_step_ahead_point_forecast, 
                                                   ar_eight_step_ahead_point_forecast), 
                                Category = "C")
    ar_aux_df = data.frame(DATE = x_intercept, 
                           point_forecast = (stat_df %>%
                                               select(DATE, reference_col) %>%
                                               filter(DATE == x_intercept) %>%
                                               pull(reference_col)), 
                           Category = "C")
    
    ar_forecast_df = rbind(ar_aux_df, ar_forecast_df)
    
    # making the point forecast for ADL
    adl_one_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 1, 8)
    adl_two_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 2, 8)
    adl_three_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 3, 8)
    adl_four_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 4, 8)
    adl_five_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 5, 8)
    adl_six_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 6, 8)
    adl_seven_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 7, 8)
    adl_eight_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, 8, 8)
    
    adl_one_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 1, adl_one_step_best_model$routput_lag, adl_one_step_best_model$hstart_lag)
    adl_two_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 2, adl_two_step_best_model$routput_lag, adl_two_step_best_model$hstart_lag)
    adl_three_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 3, adl_three_step_best_model$routput_lag, adl_three_step_best_model$hstart_lag)
    adl_four_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 4, adl_four_step_best_model$routput_lag, adl_four_step_best_model$hstart_lag)
    adl_five_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 5, adl_five_step_best_model$routput_lag, adl_five_step_best_model$hstart_lag)
    adl_six_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 6, adl_six_step_best_model$routput_lag, adl_six_step_best_model$hstart_lag)
    adl_seven_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 7, adl_seven_step_best_model$routput_lag, adl_seven_step_best_model$hstart_lag)
    adl_eight_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, 8, adl_eight_step_best_model$routput_lag, adl_eight_step_best_model$hstart_lag)
    
    adl_one_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_one_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 1, 8)$rmse
    adl_two_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_two_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 2, 8)$rmse
    adl_three_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_three_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 3, 8)$rmse
    adl_four_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_four_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 4, 8)$rmse
    adl_five_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_five_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 5, 8)$rmse
    adl_six_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_six_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 6, 8)$rmse
    adl_seven_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_seven_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 7, 8)$rmse
    adl_eight_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_eight_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 8, 8)$rmse
    
    forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
    adl_forecast_df = data.frame(DATE = forecast_seq_dates,
                                 point_forecast = c(adl_one_step_ahead_point_forecast$fit, 
                                                    adl_two_step_ahead_point_forecast$fit, 
                                                    adl_three_step_ahead_point_forecast$fit,
                                                    adl_four_step_ahead_point_forecast$fit,
                                                    adl_five_step_ahead_point_forecast$fit,
                                                    adl_six_step_ahead_point_forecast$fit,
                                                    adl_seven_step_ahead_point_forecast$fit,
                                                    adl_eight_step_ahead_point_forecast$fit), 
                                 rmse = c(adl_one_step_ahead_rmse, 
                                          adl_two_step_ahead_rmse,
                                          adl_three_step_ahead_rmse,
                                          adl_four_step_ahead_rmse,
                                          adl_five_step_ahead_rmse, 
                                          adl_six_step_ahead_rmse, 
                                          adl_seven_step_ahead_rmse,
                                          adl_eight_step_ahead_rmse),
                                 Category = "D")
    
    adl_aux_df = data.frame(DATE = x_intercept, 
                            point_forecast = (stat_df %>%
                                                select(DATE, reference_col) %>%
                                                filter(DATE == x_intercept) %>%
                                                pull(reference_col)),
                            rmse = 1,
                            Category = "D")
    adl_forecast_df = rbind(adl_aux_df, adl_forecast_df)
    
    # Random Forest Forecast
    rf_point_forecast_vector = rf(reference_year, reference_quarter, "8Q")
    rf_forecast_df = data.frame(DATE = forecast_seq_dates, 
                                point_forecast = rf_point_forecast_vector)
    print(rf_forecast_df)
    
    if (input$model_choice == "AR"){

      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      ar_forecast_df$tooltip <- paste("Date:", ar_forecast_df$DATE, "<br>Forecast:", round(ar_forecast_df$point_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Clean Values from very last Vintage
        geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
        geom_point(data = ar_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = ar_forecast_df$tooltip), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_line(data = ar_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", "Actual Change" = "chartreuse2", "Forecasted Change" = "blue"),
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      
      return(plotly_plot)
      
    } else if (input$model_choice == "ADL") {
      
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      print(z_crit_value)
      adl_forecast_df = adl_forecast_df %>%
        mutate(lower_forecast = point_forecast - z_crit_value*rmse, 
               upper_forecast = point_forecast + z_crit_value*rmse)
      print(adl_forecast_df)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      adl_forecast_df$tooltip <- paste("Date:", adl_forecast_df$DATE, "<br>Forecast:", round(adl_forecast_df$point_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Clean Values from very last Vintage
        geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = adl_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = adl_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = adl_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) +
        #Lower Bound for fanchart
        geom_point(data = adl_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        geom_line(data = adl_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = adl_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        geom_line(data = adl_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", "Actual Change" = "chartreuse2", "Forecasted Change" = "blue"),
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      
      print("hello ADL")
      print(adl_forecast_df)
      return(plotly_plot)
      
    } else if (input$model_choice == "Random Forest"){
      print("hello random forest")
      print(rf_forecast_df)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      rf_forecast_df$tooltip <- paste("Date:", rf_forecast_df$DATE, "<br>Forecast:", round(rf_forecast_df$point_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Clean Values from very last Vintage
        geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = rf_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = adl_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = rf_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) +
        #Lower Bound for fanchart
        #geom_point(data = rf_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #geom_line(data = rf_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        #geom_point(data = rf_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        #geom_line(data = rf_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", "Actual Change" = "chartreuse2", "Forecasted Change" = "blue"),
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      return(plotly_plot)
      
    }

  })
  
  
  output$dt_table = DT::renderDT({
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    print(reference_col)
    
    stat_df %>%
      select(DATE, reference_col) %>%
      DT::datatable()
  })
  
  observeEvent(input$show_about, {
    text_about = "As one of the fundamental indicators of economic activity and a pivotal metric guiding policy decisions, precise forecasting of GDP is imperative for policymakers, businesses, and individuals. However, GDP figures often undergo revisions, resulting in disparities between initial projections and final numbers. These revisions can profoundly influence the dependability and efficacy of macroeconomic forecasting models when operating with real-time data.
Hence, our project endeavors to construct and assess resilient forecasting models adept at integrating updated GDP data to deliver precise predictions."
    showModal(modalDialog(text_about, title = "About"))
    
    
  })
  
  output$plot_description <- renderText({
    title <- "Plot Description"
    description <- "We used an autoregressive (AR) model to forecast GDP values. In this case, GDP values are being forecasted based on their own past values.
    In our model, through estimating the optimal lag lengths, it selects the most relevant past GDP values to predict future trends."
    paste(title, description, sep = ": ")
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




# Dumpster - for debugging purposes

# importing data
# df = read_excel("ROUTPUTQvQd.xlsx", col_names = TRUE)
# df
# summary(df)
# 
# # converting the DATE col to be datetime in terms of quarters
# df$DATE = zoo::as.yearqtr(df$DATE, format = "%Y:Q%q")
# df 
# 
# df = df %>%
#   mutate_if(is.character, as.double)
# 
# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   mutate(lag_ROUTPUT24Q1 = lag(ROUTPUT24Q1)) %>%
#   mutate(log_ROUTPUT24Q1 = log(ROUTPUT24Q1, base = exp(1)), 
#          log_lag_ROUTPUT24Q1 = log(lag_ROUTPUT24Q1, base = exp(1))) %>%
#   mutate(log_diff = log_ROUTPUT24Q1 - log_lag_ROUTPUT24Q1)


# time series plot 
# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   ggplot(aes(x=DATE, y=ROUTPUT24Q1)) +
#   geom_line() +
#   ggtitle("Change in Real GDP Across Time")
# 
# df %>%
#   select(DATE,  "ROUTPUT22Q1") %>%
#   ggplot(aes(x=DATE, y="ROUTPUT22Q1")) +
#   geom_line() +
#   labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))

# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   plot_ly(x = DATE, y = ROUTPUT24Q1, type="scatter", mode="lines")


# autocorrelation plot across time
# acf_values = acf(df$ROUTPUT24Q1, lag.max=150)
# acf_values = acf(df$ROUTPUT24Q1, lag.max=150, plot = FALSE)
# acf_df = data.frame(lag = acf_values$lag, acf = acf_values$acf)
# ggplot(acf_df, aes(x = lag, y = acf)) + 
#   geom_bar(stat = "identity") + 
#   labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(acf_df, aes(x = lag, y = acf)) + 
#   geom_segment(aes(xend = lag, yend = 0), color = "blue") +
#   geom_point(color = "red", size = 1, shape = 18) + 
#   labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))

 
