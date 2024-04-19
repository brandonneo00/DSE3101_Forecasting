
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny) #for RShiny app
library(shinyjs) #for javascript in R
#remotes::install_github("daattali/shinycssloaders") #please install this to get the captions feature for the loading spinner to avoid errors when running
library(shinycssloaders) #for loading spinner
library(shinythemes) #for color palette
library(plotly) #for converting plots to be interactive
library(ggplot2) #for making plots nicer than base plots
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


library(BVAR)
library(dplyr)
library(zoo)
library(DT)

library(dynlm)

# Read the Excel file using the correct file path
path_to_data = "ROUTPUTQvQd.xlsx"
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


# getting the last available vintage for comparing against fan chart

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


last_vintage = get_last_available_vintage(stat_df)
last_vintage_year = substr(last_vintage, start = nchar(last_vintage) - 3, stop = nchar(last_vintage) - 2)
#earliest_year = min(as.integer(format(stat_df$DATE, "%Y")))
last_vintage_year = as.integer(paste("20", last_vintage_year, sep=""))
last_vintage_quarter = substr(last_vintage, start = nchar(last_vintage) - 1, stop = nchar(last_vintage))
all_poss_quarters = c("Q1", "Q2", "Q3", "Q4")


reticulate::source_python("rf_backtesting.py")

# function to find optimal lags for AR
fitAR = function(vintage_year, vintage_quarter, df, forecast_horizon, max_lags){
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
  # # print(subset_hstart_df)
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
      ## print(label)
      
      aux = embed(subset_hstart_df, (max_lags + forecast_horizon))
      aux = aux[complete.cases(aux), ]
      # # print(aux)
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
      ## print(subset_X_mat_routput)
      
      if(num_rows_routput > num_rows_hstart){
        # # print("here1")
        X_mat_routput = subset_X_mat_routput[1:num_rows_hstart,]
        y = y_sub[1:num_rows_hstart]
        X_mat_hstart = subset_X_mat_hstart
        
        
        
      }
      else if (num_rows_routput < num_rows_hstart){
        # # print("here2")
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
    # # print(best_model_lag_hstart)
    
    
    model_ar = fitAR_model(vintage_year, vintage_quarter, routput_df, forecast_horizon, max_lags)
    best_model_lag_routput = length(coef(model_ar))-1
    best_model <- dynlm(routput_ts ~ L(routput_ts, 1:best_model_lag_routput) + L(hstart_ts, 1:best_model_lag_hstart))
    # # print(best_model)
    
    optimal_model$routput_lag <- best_model_lag_routput
    optimal_model$hstart_lag <- best_model_lag_hstart
    optimal_model$model <- best_model
    
  }
  
  return(optimal_model)
  
}

# importing additional data of predictors needed for ADL
hstart <- read_excel('hstartsMvMd.xlsx')
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
  # # print(year)
  # # print(month)
  
  # Construct the column name in hstart_df
  hstart_col <- paste("HSTARTS", year, month, sep = "")
  # # print(hstart_col)
  
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
  
  
  # # print(test_data_routput)
  
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
    # # print(final_hstart)
    final_routput = test_data_routput
    Y_final = Y
  }
  else{
    final_routput = test_data_routput
    final_hstart = test_data_hstart
    Y_final = Y
  }
  
  
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
  ## print(result)
  final_value = tail(result, 1) #retrieving the last value
  return(final_value)
}

adl_forecast = getADLForecast(10, "Q4", stat_gdp, hstart_gdp, 1, adl$routput_lag, adl$hstart_lag)
adl_forecast
adl_forecast1 = getADLForecast(10, "Q4", stat_gdp, hstart_gdp, 2, adl$routput_lag, adl$hstart_lag)
adl_forecast1
adl_forecast$fit

ar_predict_function <- function(model, optimal_lag_routput, routput_values) {
  coefficients <- coef(model)
  # Extracting intercept and lag coefficients
  intercept <- coefficients[1]
  lag_coefficients <- coefficients[2:(optimal_lag_routput + 1)]
  
  # Calculating prediction
  prediction <- intercept + sum(lag_coefficients * routput_values)
  return(routput_values)
}

# Assuming you have already fitted the AR model using fitAR_model function
ar_model <- fitAR_model(65, "Q4", stat_gdp, 2, 8)
ar_model
ar_test

#model = fitAR_model(65, "Q4", stat_gdp, 2, 8)
# Now you can use the predict_function with the fitted model
predictions <- ar_predict_function(ar_model, ar_test, stat_gdp)
predictions


adl_predict_function <- function(model, optimal_lag_routput, optimal_lag_hstart, routput_values, hstart_values){
  ## print(routput_values)
  intercept <- coef(model)[1]
  coefficients_routput <- coef(model)[2:(2+optimal_lag_routput-1)]
  coefficients_hstart <- coef(model)[(2+optimal_lag_routput):length(coef(model))]
  prediction <- intercept + sum(coefficients_routput * routput_values) + sum(coefficients_hstart * hstart_values)
  return(prediction)
  
}



ARBacktest <- function(vintage_year, vintage_quarter, model, routput_gdp, routput_date, forecast_horizon, max_lags) {
  optimal_lag <- length(coef(model))-1
  
  col_prefix <- "ROUTPUT"
  reference_col <- paste(col_prefix, vintage_year, vintage_quarter, sep="")
  # # print(reference_col)
  column_index <- which(names(routput_gdp) == reference_col)
  # # print(column_index)
  
  routput_column <- routput_gdp[, column_index]
  
  last_column <- routput_gdp[, ncol(routput_gdp)]
  prediction_values <- c()
  actual_values <- c()
  
  full_year <- convert_to_full_year(vintage_year)
  starting_index_string <- paste(full_year, ":", vintage_quarter, sep="")
  row_index <- which(routput_date$DATE == starting_index_string)
  starting_row_index <- row_index - 50 - forecast_horizon
  count <- 0
  return_list <- list()
  
  while (count < 50) {
    lagged_values <- c()
    
    for (i in 1:optimal_lag) {
      starting_row <- starting_row_index + count
      lagged_values <- c(lagged_values, routput_column[starting_row - forecast_horizon - i])
    }
    
    prediction <- ar_predict_function(model = model, optimal_lag_routput = optimal_lag, routput_values = lagged_values)
    
    actual_values <- c(actual_values, last_column[starting_row_index + count])
    prediction_values <- c(prediction_values, prediction)
    count <- count + 1
  }
  
  mse <- mean((actual_values - prediction_values)^2)
  rmse <- sqrt(mse)
  return_list$rmse <- rmse
  return_list$predictions<- prediction_values
  
  return(return_list)
}


ARBacktest("85", "Q3", ar_model, stat_gdp, gdp_date, 2, 6)

ADLbacktest <- function(vintage_year, vintage_quarter, optimal_adl_model, routput_gdp, routput_date, hstart_gdp, hstart_date, forecast_horizon, max_lags){
  #adl <- fit_adl(vintage_year, vintage_quarter, routput_gdp, hstart_gdp, forecast_horizon, max_lags)
  model <- optimal_adl_model$model
  optimal_routput_lag <- optimal_adl_model$routput_lag
  optimal_hstart_lag <- optimal_adl_model$hstart_lag
  full_year <- convert_to_full_year(vintage_year)
  starting_index_string <- paste(full_year, ":", vintage_quarter, sep="")
  # # print(starting_index_string)
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
      ## print(routput_values)
    }
    
    for(j in 1:optimal_hstart_lag){
      starting_row_hstart <- starting_row_index+count
      hstart_values <- c(hstart_values, hstart_column[starting_row_hstart-forecast_horizon-i])
    }
    
    prediction <- adl_predict_function(model=model, optimal_lag_routput=optimal_routput_lag, optimal_lag_hstart=optimal_hstart_lag, routput_values=routput_values, hstart_values=hstart_values)
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
}

routput <- read_excel('ROUTPUTQvQd.xlsx')
routput <- routput %>%
  mutate(across(-DATE, ~ifelse(. == "#N/A", NA_real_, as.numeric(as.character(.)))))
routput_num <- routput[, 2:ncol(routput)] 
routput_date <- routput[,1]
transformation_codes <- rep(5, ncol(routput_num))  # Apply the fred_transform function with the transformation codes 
routput_gdp <- fred_transform(data = routput_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)


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


library(reticulate)
reticulate::source_python("rf_feature_importance.py")


ui <- fluidPage(
  ## Inserting background image
  tags$head(
    tags$style(
      HTML(
        "
        #background-image {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          z-index: -1;
        }
        "
      )
    )
  ),
  
  # Embedding the image using HTML img tag
  tags$img(src = "lightsciencebackground.jpg", id = "background-image", alt = "Background Image"),
  
  # Application title
  titlePanel("Experimenting with Macroeconomic Forecasting: Real GDP Change"),
  
  h4("Welcome to the forefront of macroeconomic forecasting."),
  h4("Step into our innovative lab, where we blend scientific rigour with economic analysis to navigate the complex terrain of GDP forecasting."),
  #h3("Let the experiment begin!"),
  fluidRow(column(9, 
                  h3("Let the experiment begin!", style = 'margin: auto;')), 
           column(3, 
                  align = "right", 
                  actionButton("show_about", "About", style = "margin-left: auto;"),
           )),
  
  theme = shinythemes::shinytheme("united"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year", min=1975, max=2023, step=1, value = 2010),
      selectInput("quarter", "Select Quarter", choices=c("Q1", "Q2", "Q3", "Q4")),
      selectInput("alpha", "Select Coverage for Fan Chart", choices=c("50%", "80%", "90%"), selected="50%"),
      selectInput("model_choice", "Choose Model to show", choices = c("Autoregressive Model (AR)", "Autoregressive Distributed Lag Model (ADL)", "Random Forest", "Combined", "Most Optimal"), selected = "AR"), 
      checkboxInput("hide_line_point", "Show Actual Change", value = FALSE),
      tags$div(style = "color: red;", "WARNING: Actions related to Random Forest, Combined and Most Optimal will result in a longer run time"),
      tags$img(src = "forecast-analytics.png", height = 200, width = 200),
      tags$img(src = "experiment.png", height = 200, width = 200)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series", fluidRow(shinycssloaders::withSpinner(plotlyOutput("line_plot"), type = 8, color.background = "#0275D8", caption = "Model training in progress..."),  
                                         fluidRow(style = "display: flex; align-items: center;", 
                                                  column(3,
                                                         align = "right",
                                                         # Add text label
                                                         textOutput("table_label")), 
                                                  column(9,tableOutput("table_forecast")))), 
                 textOutput("plot_description"), 
                 conditionalPanel(condition = "input.model_choice == 'Random Forest'", plotOutput("rf_feature_importance"))),
        tabPanel("Historical Data", shinycssloaders::withSpinner(DT::DTOutput("dt_table"), , type = 8, color.background = "#0275D8", caption = "Model training in progress...")),
        # Add a new tab for Statistics with Data Table
        tabPanel("Statistics",
                 h4("Statistics Table"),
                 shinycssloaders::withSpinner(DT::DTOutput("stats_table"), , type = 8, color.background = "#0275D8", caption = "Model training in progress...")
        )
      ),
      
      # Add a new picture below the plot description and mirror it
      tags$img(src = "Teacher_teaching_with_a_stick.svg", height = 200, width = 200, style = "transform: scaleX(-1);")
    )
  )
)


adl_setup <- function(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept){
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
  return (adl_forecast_df)
}

ar_setup <- function(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept){
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
  
  ar_model_one_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 1, 8)
  ar_model_two_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 2, 8)
  ar_model_three_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 3, 8)
  ar_model_four_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 4, 8)
  ar_model_five_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 5, 8)
  ar_model_six_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 6, 8)
  ar_model_seven_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 7, 8)
  ar_model_eight_step_ahead <- fitAR_model(reference_year, reference_quarter, stat_df, 8, 8)
  
  ar_one_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_one_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_two_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_two_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_three_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_three_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_four_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_four_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_five_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_five_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_six_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_six_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_seven_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_seven_step_ahead, routput_gdp, routput_date, 8)$rmse
  ar_eight_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_model_eight_step_ahead, routput_gdp, routput_date, 8)$rmse
  
  
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
                              rmse = c(ar_one_step_ahead_rmse, 
                                       ar_two_step_ahead_rmse,
                                       ar_three_step_ahead_rmse,
                                       ar_four_step_ahead_rmse,
                                       ar_five_step_ahead_rmse, 
                                       ar_six_step_ahead_rmse, 
                                       ar_seven_step_ahead_rmse,
                                       ar_eight_step_ahead_rmse),
                              Category = "C")
  return(ar_forecast_df)
  
}

rf_cal_rmsfe <- function(reference_year, reference_quarter) {
  rf_fifty_vectors <- list()
  
  rmse <- c()
  for (i in 1:8) {
    rf_back <- pred_50(reference_year, reference_quarter, as.integer(i), as.integer(3))
    rf_fifty_vectors <- c(rf_fifty_vectors, rf_back)
    result_rmse <- get_rmse()
    rmse <- c(rmse, result_rmse)
  }
  return(rmse)
}

rf_cal_rmsfe <- function(reference_year, reference_quarter) {
  rf_fifty_vectors <- list()
  
  reference_year <- paste("20", reference_year, sep = "")
  rmse <- c()
  for (i in 1:8) {
    rf_back <- pred_50(reference_year, reference_quarter, as.integer(i), as.integer(1))
    
    rf_fifty_vectors <<- c(rf_fifty_vectors, list(rf_back))
    result_rmse <- get_rmse(rf_back)
    rmse <- c(rmse, result_rmse)
  }
  return(rmse)
}

combined_backtest <- function(vintage_year, vintage_quarter, routput_gdp, routput_date, hstart_gdp, hstart_date, forecast_horizon, rf_rmse, max_lags){
  actual_values <- c()
  last_column = routput_gdp[,ncol(routput_gdp)]
  full_year <- convert_to_full_year(vintage_year)
  starting_index_string <- paste(full_year, ":", vintage_quarter, sep="")
  # # print(starting_index_string)
  row_index <- which(routput_date$DATE == starting_index_string)
  starting_row_index <- row_index - 50 - forecast_horizon
  count <- 0 
  
  actual_values <- c(actual_values, last_column[starting_row_index+count])
  
  #vintage year is the last two digit of YYYY
  
  optimal_adl_model <- fit_adl(vintage_year, vintage_quarter, routput_gdp, hstart_gdp, forecast_horizon, max_lags)
  adl_backtest <- ADLbacktest(vintage_year, vintage_quarter, optimal_adl_model, routput_gdp, routput_date, hstart_gdp, hstart_date, forecast_horizon, max_lags)
  adl_rmse <- adl_backtest$rmse
  
  ar_model <- fitAR_model(vintage_year, vintage_quarter, routput_gdp, forecast_horizon, max_lags)
  ar_backtest <- ARBacktest(vintage_year, vintage_quarter, ar_model, routput_gdp, routput_date, forecast_horizon, max_lags)
  ar_rmse <- ar_backtest$rmse
  average_rmse <- (adl_rmse + ar_rmse + rf_rmse)/3
  return(average_rmse)
  
}


rf_setup <- function(reference_year, reference_quarter, forecast_seq_dates){
  rf_point_forecast_vector = rf(reference_year, reference_quarter, as.integer(1))
  rf_forecast_df = data.frame(DATE = forecast_seq_dates,
                              point_forecast = rf_point_forecast_vector,
                              rmse = rf_cal_rmsfe(reference_year, reference_quarter))
  return(rf_forecast_df)
}

combined_setup <- function(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept, forecast_seq_dates){
  reticulate::source_python("RandomForestTS.py")
  reticulate::source_python("rf_backtesting.py")
  reticulate::source_python("rf_feature_importance.py")
  
  adl_forecast_df <- adl_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
  ar_forecast_df <- ar_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
  rf_forecast_df <- rf_setup(reference_year, reference_quarter, forecast_seq_dates)
  
  adl_forecast_points <- adl_forecast_df$point_forecast[1:length(adl_forecast_df$point_forecast)]
  ar_forecast_points <- ar_forecast_df$point_forecast[1:length(ar_forecast_df$point_forecast)]
  
  adl_rmse <- adl_forecast_df$rmse[1:length(adl_forecast_df$rmse)]
  ar_rmse <- ar_forecast_df$rmse[1:length(ar_forecast_df$rmse)]
  
  
  
  
  
  combined_point_forecast <- (adl_forecast_points + ar_forecast_points + rf_forecast_df$point_forecast)/3
  combined_point_rmse <- (adl_rmse+ar_rmse+rf_forecast_df$rmse)/3
  
  
  combined_forecast_df = data.frame(DATE = forecast_seq_dates,
                                    point_forecast = combined_point_forecast,
                                    rmse = combined_point_rmse, 
                                    Category = "E")
  
  
  
  return (combined_forecast_df)
  
}

optimal_setup <- function(reference_year, reference_quarter, reference_col, input, x_intercept, rmse, forecast_horizon, seq_date, forecast_seq_dates, rf_rmse){
  index_smallest = which.min(rmse)
  
  if(index_smallest == 1){
    ar_step_best_model_lag = fitAR(reference_year, reference_quarter, stat_df, forecast_horizon, 8)
    ar_step_ahead_point_forecast = getForecast(reference_year, reference_quarter, stat_df, forecast_horizon, ar_step_best_model_lag)
    
    ar_step_ahead_rmse <- ARBacktest(reference_year, reference_quarter, ar_step_ahead_point_forecast, routput_gdp, routput_date, 8)$rmse
    
    ar_forecast_df = data.frame(DATE = seq_date,
                                point_forecast = c(ar_step_ahead_rmse),
                                rmse = c(ar_step_ahead_rmse),
                                Category = "C")
    return(ar_forecast_df)
    
    
  }else if (index_smallest == 2){
    adl_one_step_best_model = fit_adl(reference_year, reference_quarter, stat_gdp, hstart_gdp, forecast_horizon, 8)
    
    adl_one_step_ahead_point_forecast = getADLForecast(reference_year, reference_quarter, stat_gdp, hstart_gdp, forecast_horizon, adl_one_step_best_model$routput_lag, adl_one_step_best_model$hstart_lag)
    
    adl_one_step_ahead_rmse = ADLbacktest(reference_year, reference_quarter, adl_one_step_best_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, 1, 8)$rmse
    
    
    adl_forecast_df = data.frame(DATE = seq_date,
                                 point_forecast = c(adl_one_step_ahead_point_forecast$fit), 
                                 rmse = c(adl_one_step_ahead_rmse),
                                 Category = "D")
    
    return (adl_forecast_df)
    
  } else if (index_smallest == 3){
    reticulate::source_python("RandomForestTS.py")
    reticulate::source_python("rf_backtesting.py")
    reticulate::source_python("rf_feature_importance.py")
    
    rf_point_forecast_vector = rf(reference_year, reference_quarter, as.integer(1))
    rf_forecast_df = data.frame(DATE = seq_date,
                                point_forecast = rf_point_forecast_vector[forecast_horizon],
                                rmse = rf_rmse, 
                                Category="R")
    
    
    return(rf_forecast_df)
    
  }
  else if (index_smallest == 4){
    reticulate::source_python("RandomForestTS.py")
    reticulate::source_python("rf_backtesting.py")
    reticulate::source_python("rf_feature_importance.py")
    
    adl_forecast_df <- adl_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
    ar_forecast_df <- ar_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
    rf_forecast_df <- rf_setup(reference_year, reference_quarter, forecast_seq_dates)
    
    adl_forecast_points <- adl_forecast_df$point_forecast[2:length(adl_forecast_df$point_forecast)]
    ar_forecast_points <- ar_forecast_df$point_forecast[2:length(ar_forecast_df$point_forecast)]
    
    adl_rmse <- adl_forecast_df$rmse[2:length(adl_forecast_df$rmse)]
    ar_rmse <- ar_forecast_df$rmse[2:length(ar_forecast_df$rmse)]
    
    
    
    
    
    combined_point_forecast <- (adl_forecast_points + ar_forecast_points + rf_forecast_df$point_forecast)/3
    combined_point_rmse <- (adl_rmse+ar_rmse+rf_forecast_df$rmse)/3
    
    
    combined_forecast_df = data.frame(DATE = seq_date,
                                      point_forecast = combined_point_forecast[forecast_horizon],
                                      rmse = combined_point_rmse[forecast_horizon], 
                                      Category = "E")
    
    return(combined_forecast_df)
    
  }
  
}



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$line_plot = renderPlotly({
    
    prevInputs <- reactiveValues(year = NULL, quarter = NULL)
    observe({
      # Current year and quarter input values
      currentYear <- input$year
      currentQuarter <- input$quarter
      
      # Check if current inputs are different from the previous ones
      if (is.null(prevInputs$year) || is.null(prevInputs$quarter) || 
          currentYear != prevInputs$year || currentQuarter != prevInputs$quarter) {
        
        print(paste("CurrentYear:",  currentYear, "PrevYear:", prevInputs$year))
        
        # Update previous input values
        prevInputs$year <- currentYear
        prevInputs$quarter <- currentQuarter
        
        # Perform your operation here
        # For example, this could be a print statement or a more complex operation
        print(paste("Year:", currentYear, "Quarter:", currentQuarter))
        
        # If using setup_adl() or similar, trigger it here
        # result <- setup_adl(currentYear, currentQuarter)
        # use the result as needed
      }
    })
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
    
    # # print(reference_col)
    
    subset_df = stat_df %>%
      select(DATE, reference_col) %>%
      filter(complete.cases(.)) %>% #removing the "future" rows
      tail(16) %>% #showing the last 16 quarters of data 
      mutate(Category = "A")
    
    forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
    
    ## print(subset_df)
    
    reference_quarter_numeric = as.numeric(substr(input$quarter, start = nchar(input$quarter), stop = nchar(input$quarter)))
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
    true_value_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    true_value_seq_dates = seq(true_value_start_date, length.out = 8, by = 1/4)
    
    last_available_vintage = get_last_available_vintage(stat_df)
    true_df = stat_df %>% 
      select(DATE, last_available_vintage) %>%
      filter(DATE %in% true_value_seq_dates) %>%
      mutate(Category = "B")
    
    
    # Random Forest Forecast
    
    
    if (input$model_choice == "Autoregressive Model (AR)"){
      print("HERE AT AR AR AR")
      ar_forecast_df <- ar_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
      # print(ar_forecast_df)
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      # # print(z_crit_value)
      ar_forecast_df = ar_forecast_df %>%
        mutate(lower_forecast= point_forecast - z_crit_value *rmse,
               upper_forecast = point_forecast + z_crit_value *rmse)
      
      print(ar_forecast_df)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      ar_forecast_df$tooltip <- paste("Date:", ar_forecast_df$DATE, "<br>Forecast:", round(ar_forecast_df$point_forecast, 2))
      ar_forecast_df$tooltip_lower <- paste("Date:", ar_forecast_df$DATE, "<br>Lower Bound:", round(ar_forecast_df$lower_forecast, 2))
      ar_forecast_df$tooltip_upper <- paste("Date:", ar_forecast_df$DATE, "<br>Upper Bound:", round(ar_forecast_df$upper_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = ar_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = ar_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = ar_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        #Lower Bound for fanchart
        geom_point(data = ar_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound", text = ar_forecast_df$tooltip_lower))) +
        geom_line(data = ar_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = ar_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound", text = ar_forecast_df$tooltip_upper))) +
        geom_line(data = ar_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        # Shading for fanchart
        geom_ribbon(data = ar_forecast_df, aes(x = DATE, ymin = lower_forecast, ymax = upper_forecast), fill = "lightblue", alpha = 0.65) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Growth of Real GDP Across Time", x = "", y = "Growth in GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", 
                                      "Actual Change" = "chartreuse2", 
                                      "Forecasted Change" = "blue", 
                                      "Lower Bound" = "orange",  
                                      "Upper Bound" = "yellow"), 
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      if (input$hide_line_point) {
        #Clean Values from very last Vintage
        gg <- gg + 
          geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
          geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) + 
          scale_color_manual(values = c("Historical Change" = "black", 
                                        "Actual Change" = "chartreuse2", 
                                        "Forecasted Change" = "blue", 
                                        "Lower Bound" = "orange",  
                                        "Upper Bound" = "yellow"), 
                             name = "Legend Name")
      }
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      return(plotly_plot)
      
    } else if (input$model_choice == "Autoregressive Distributed Lag Model (ADL)") {
      print("HERE AT ADL ADL ADL")
      adl_forecast_df <- adl_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
      print(adl_forecast_df)
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      # # print(z_crit_value)
      adl_forecast_df = adl_forecast_df %>%
        mutate(lower_forecast = point_forecast - z_crit_value*rmse, 
               upper_forecast = point_forecast + z_crit_value*rmse)
      # # print(adl_forecast_df)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      adl_forecast_df$tooltip <- paste("Date:", adl_forecast_df$DATE, "<br>Forecast:", round(adl_forecast_df$point_forecast, 2))
      adl_forecast_df$tooltip_lower <- paste("Date:", adl_forecast_df$DATE, "<br>Lower Bound:", round(adl_forecast_df$lower_forecast, 2))
      adl_forecast_df$tooltip_upper <- paste("Date:", adl_forecast_df$DATE, "<br>Upper Bound:", round(adl_forecast_df$upper_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = adl_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = adl_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = adl_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        #Lower Bound for fanchart
        geom_point(data = adl_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound", text = adl_forecast_df$tooltip_lower))) +
        geom_line(data = adl_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = adl_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound", text = adl_forecast_df$tooltip_upper))) +
        geom_line(data = adl_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        # Shading for fanchart
        geom_ribbon(data = adl_forecast_df, aes(x = DATE, ymin = lower_forecast, ymax = upper_forecast), fill = "lightblue", alpha = 0.65) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Growth of Real GDP Across Time", x = "", y = "Growth in GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", 
                                      "Actual Change" = "chartreuse2", 
                                      "Forecasted Change" = "blue", 
                                      "Lower Bound" = "orange",  
                                      "Upper Bound" = "yellow"), 
                           name = "Legend Name")  +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      if (input$hide_line_point) {
        #Clean Values from very last Vintage
        gg <- gg + 
          geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
          geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) + 
          scale_color_manual(values = c("Historical Change" = "black", 
                                        "Actual Change" = "chartreuse2", 
                                        "Forecasted Change" = "blue", 
                                        "Lower Bound" = "orange",  
                                        "Upper Bound" = "yellow"), 
                             name = "Legend Name")
      }
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      
      return(plotly_plot)
      
    } else if (input$model_choice == "Random Forest"){
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      rf_forecast_df <- rf_setup(reference_year, reference_quarter, forecast_seq_dates)
      rf_forecast_df = rf_forecast_df %>%
        mutate(lower_forecast = point_forecast - z_crit_value*rmse,
               upper_forecast = point_forecast + z_crit_value*rmse)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      rf_forecast_df$tooltip <- paste("Date:", rf_forecast_df$DATE, "<br>Forecast:", round(rf_forecast_df$point_forecast, 2))
      rf_forecast_df$tooltip_lower <- paste("Date:", rf_forecast_df$DATE, "<br>Lower Bound:", round(rf_forecast_df$lower_forecast, 2))
      rf_forecast_df$tooltip_upper <- paste("Date:", rf_forecast_df$DATE, "<br>Upper Bound:", round(rf_forecast_df$upper_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = rf_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = rf_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = rf_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        #Lower Bound for fanchart
        geom_point(data = rf_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound", text = rf_forecast_df$tooltip_lower))) +
        geom_line(data = rf_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = rf_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        geom_line(data = rf_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound", text = rf_forecast_df$tooltip_upper))) +
        # Shading for fanchart
        geom_ribbon(data = rf_forecast_df, aes(x = DATE, ymin = lower_forecast, ymax = upper_forecast), fill = "lightblue", alpha = 0.65) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Growth of Real GDP Across Time", x = "", y = "Growth in GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", 
                                      "Actual Change" = "chartreuse2", 
                                      "Forecasted Change" = "blue", 
                                      "Lower Bound" = "orange",  
                                      "Upper Bound" = "yellow"), 
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      if (input$hide_line_point) {
        #Clean Values from very last Vintage
        gg <- gg + 
          geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
          geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) + 
          scale_color_manual(values = c("Historical Change" = "black", 
                                        "Actual Change" = "chartreuse2", 
                                        "Forecasted Change" = "blue", 
                                        "Lower Bound" = "orange",  
                                        "Upper Bound" = "yellow"), 
                             name = "Legend Name")
      }
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      return(plotly_plot)
      
    } else if (input$model_choice == "Combined"){
      combined_forecast_df <- combined_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept, forecast_seq_dates)
      
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      # # print(z_crit_value)
      combined_forecast_df = combined_forecast_df %>%
        mutate(lower_forecast = point_forecast - z_crit_value*rmse,
               upper_forecast = point_forecast + z_crit_value*rmse)
      # print(adl_forecast_df)
      
      # Prepare the tooltip content for each dataset
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      combined_forecast_df$tooltip <- paste("Date:", combined_forecast_df$DATE, "<br>Forecast:", round(combined_forecast_df$point_forecast, 2))
      combined_forecast_df$tooltip_lower <- paste("Date:", combined_forecast_df$DATE, "<br>Lower Bound:", round(combined_forecast_df$lower_forecast, 2))
      combined_forecast_df$tooltip_upper <- paste("Date:", combined_forecast_df$DATE, "<br>Upper Bound:", round(combined_forecast_df$upper_forecast, 2))
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = combined_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = combined_forecast_df$tooltip), show.legend = TRUE) +
        geom_line(data = combined_forecast_df, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        #Lower Bound for fanchart
        geom_point(data = combined_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound", text = combined_forecast_df$tooltip_lower))) +
        geom_line(data = combined_forecast_df, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = combined_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound", text = combined_forecast_df$tooltip_upper))) +
        geom_line(data = combined_forecast_df, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        # Shading for fanchart
        geom_ribbon(data = combined_forecast_df, aes(x = DATE, ymin = lower_forecast, ymax = upper_forecast), fill = "lightblue", alpha = 0.65) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Growth of Real GDP Across Time", x = "", y = "Growth in GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", 
                                      "Actual Change" = "chartreuse2", 
                                      "Forecasted Change" = "blue", 
                                      "Lower Bound" = "orange",  
                                      "Upper Bound" = "yellow"), 
                           name = "Legend Name")  +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      if (input$hide_line_point) {
        #Clean Values from very last Vintage
        gg <- gg + 
          geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
          geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) + 
          scale_color_manual(values = c("Historical Change" = "black", 
                                        "Actual Change" = "chartreuse2", 
                                        "Forecasted Change" = "blue", 
                                        "Lower Bound" = "orange",  
                                        "Upper Bound" = "yellow"), 
                             name = "Legend Name")
      }
      
      plotly_plot <- ggplotly(gg, tooltip = "text")
      
      return(plotly_plot)
    } else if (input$model_choice == "Most Optimal"){
      # 1. ar 2. adl 3. rf 4. combined
      actual_values <- c()
      rmse_values <- c()
      adl_models_rmse <- c()
      ar_models_rmse <- c()
      combined_models_rmse <- c()
      rf_models_rmse <- rf_cal_rmsfe(reference_year, reference_quarter)
      final_dataframe <- data.frame()
      forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
      forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
      
      
      
      for(i in 1:8){
        rmse <- c()
        
        optimal_ar_model <- fitAR_model(reference_year, reference_quarter, stat_gdp, i, 8)
        ar_rmse <- ARBacktest(reference_year, reference_quarter, optimal_ar_model, stat_gdp, gdp_date, i, 8)$rmse
        rmse <- c(rmse, ar_rmse)
        
        
        optimal_adl_model <- fit_adl(reference_year, reference_quarter, routput_gdp, hstart_gdp, i, 8)
        adl_rmse <- ADLbacktest(reference_year, reference_quarter, optimal_adl_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, i, 8)$rmse
        rmse <- c(rmse,  adl_rmse)
        
        rmse <- c(rmse, rf_models_rmse[i])
        
        
        combined_rmse  <- combined_backtest(reference_year, reference_quarter, routput_gdp, gdp_date, hstart_gdp, hstart_date, i, rf_models_rmse[i], 8)
        rmse <- c(rmse, combined_rmse)
        
        
        new_row <- optimal_setup(reference_year, reference_quarter, reference_col, input, x_intercept, rmse, i,forecast_seq_dates[i], forecast_seq_dates, rf_models_rmse[i] )
        
        final_dataframe <- rbind(final_dataframe, new_row)
        
      }
      
      z_crit_value = qnorm((alpha/2), lower.tail=FALSE)
      
      final_dataframe = final_dataframe %>%
        mutate(lower_forecast = point_forecast - z_crit_value*rmse, 
               upper_forecast = point_forecast + z_crit_value*rmse)
      
      subset_df$tooltip <- paste("Date:", subset_df$DATE, "<br>Value:", round(subset_df[[reference_col]], 2))
      true_df$tooltip <- paste("Date:", true_df$DATE, "<br>Value:", round(true_df[[last_available_vintage]], 2))
      final_dataframe$tooltip <- paste("Date:", final_dataframe$DATE, "<br>Forecast:", round(final_dataframe$point_forecast, 2))
      
      
      
      gg <- ggplot() +
        #Historical Values before Vintage Point
        geom_line(data = subset_df, aes(x = DATE, y = !!sym(reference_col), color = "Historical Change"), show.legend = TRUE) +
        #geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
        #geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) +
        #Forecasted Point Forecast OOS
        geom_point(data = final_dataframe, aes(x = DATE, y = point_forecast, color = "Forecasted Change", text = final_dataframe$tooltip), show.legend = TRUE) +
        geom_line(data = final_dataframe, aes(x = DATE, y = point_forecast, color = "Forecasted Change"), show.legend = FALSE) +
        #Lower Bound for fanchart
        geom_point(data = final_dataframe, (aes(x = DATE, y = lower_forecast, color = "Lower Bound", text = final_dataframe$tooltip_lower))) +
        geom_line(data = final_dataframe, (aes(x = DATE, y = lower_forecast, color = "Lower Bound"))) +
        #Upper Bound for fanchart
        geom_point(data = final_dataframe, (aes(x = DATE, y = upper_forecast, color = "Upper Bound", text = final_dataframe$tooltip_upper))) +
        geom_line(data = final_dataframe, (aes(x = DATE, y = upper_forecast, color = "Upper Bound"))) +
        # Shading for fanchart
        geom_ribbon(data = final_dataframe, aes(x = DATE, ymin = lower_forecast, ymax = upper_forecast), fill = "lightblue", alpha = 0.65) +
        #X intercept
        geom_vline(xintercept = x_intercept_numeric, color = "red", linetype = "dashed") +
        labs(title = "Change in Growth of Real GDP Across Time", x = "", y = "Growth in GDP") +
        scale_x_yearqtr(format = "%Y Q%q") +
        scale_color_manual(values = c("Historical Change" = "black", 
                                      "Actual Change" = "chartreuse2", 
                                      "Forecasted Change" = "blue", 
                                      "Lower Bound" = "orange",  
                                      "Upper Bound" = "yellow"), 
                           name = "Legend Name") +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "bottom") +
        guides(color = guide_legend(title = "Legend Name"))
      
      if (input$hide_line_point) {
        #Clean Values from very last Vintage
        gg <- gg + 
          geom_line(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change"), show.legend = TRUE) +
          geom_point(data = true_df, aes(x = DATE, y = !!as.name(last_available_vintage), color = "Actual Change", text = true_df$tooltip), show.legend = TRUE) + 
          scale_color_manual(values = c("Historical Change" = "black", 
                                        "Actual Change" = "chartreuse2", 
                                        "Forecasted Change" = "blue", 
                                        "Lower Bound" = "orange",  
                                        "Upper Bound" = "yellow"), 
                             name = "Legend Name")
      }
      
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
    
    # # print(reference_col)
    
    stat_df %>%
      slice(-1) %>%
      select(DATE, reference_col) %>%
      mutate(DATE = format(DATE, "%Y-Q%q")) %>%
      rename(Date = DATE, 
             "Historical GDP Growth Data" = reference_col) %>%
      mutate(`Historical GDP Growth Data` = round(`Historical GDP Growth Data`, 3)) %>%
      DT::datatable(rownames = FALSE)
  })
  
  observeEvent(input$display_new_GDP, {
    text_about = "hello"
    showModal(modalDialog(text_about, title="Me"))
    
  })
  
  output$stats_table = DT::renderDT({
    prevInputs <- reactiveValues(year = NULL, quarter = NULL)
    observe({
      # Current year and quarter input values
      currentYear <- input$year
      currentQuarter <- input$quarter
      
      # Check if current inputs are different from the previous ones
      if (is.null(prevInputs$year) || is.null(prevInputs$quarter) || 
          currentYear != prevInputs$year || currentQuarter != prevInputs$quarter) {
        
        print(paste("CurrentYear:",  currentYear, "PrevYear:", prevInputs$year))
        
        # Update previous input values
        prevInputs$year <- currentYear
        prevInputs$quarter <- currentQuarter
        
        # Perform your operation here
        # For example, this could be a print statement or a more complex operation
        print(paste("Year:", currentYear, "Quarter:", currentQuarter))
        
        # If using setup_adl() or similar, trigger it here
        # result <- setup_adl(currentYear, currentQuarter)
        # use the result as needed
      }
    })
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
    
    reference_quarter_numeric = as.numeric(substr(input$quarter, start = nchar(input$quarter), stop = nchar(input$quarter)))
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
    true_value_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    true_value_seq_dates = seq(true_value_start_date, length.out = 8, by = 1/4)
    
    #true_value_start_date = zoo::as.yearqtr(paste(x_intercept_year, x_intercept_quarter, sep=" "))
    #true_value_seq_dates = seq(true_value_start_date, length.out = 9, by = 1/4)
    
    last_available_vintage = get_last_available_vintage(stat_df)
    true_df = stat_df %>% 
      select(DATE, last_available_vintage) %>%
      filter(DATE %in% true_value_seq_dates)
    
    ar_forecast_df = ar_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
    
    ar_forecast_df = ar_forecast_df %>%
      filter(DATE %in% true_value_seq_dates) 
    
    stat_display_df = data.frame(Forecast_Horizon =1:nrow(ar_forecast_df), 
                                 AR_RMSE = round(ar_forecast_df$rmse, 3))
    
    
    if (input$model_choice == "Autoregressive Model (AR)"){
      stat_display_df = stat_display_df %>%
        select(-Forecast_Horizon)
      stat_display_df = t(stat_display_df)
      DT::datatable(stat_display_df, colnames = c("1 Step", '2 Step', '3 Step', '4 Step', '5 Step', '6 Step', "7 Step", "8 Step"))
    } else if (input$model_choice == "Autoregressive Distributed Lag Model (ADL)") {
      adl_forecast_df = adl_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
      adl_forecast_df = adl_forecast_df %>%
        filter(DATE %in% true_value_seq_dates)
      stat_display_df = stat_display_df %>%
        mutate(ADL_RMSE = round(adl_forecast_df$rmse, 3)) %>%
        select(-Forecast_Horizon)
      stat_display_df = t(stat_display_df)
      DT::datatable(stat_display_df, colnames = c("1 Step", '2 Step', '3 Step', '4 Step', '5 Step', '6 Step', "7 Step", "8 Step"))
    } else if (input$model_choice == "Random Forest") {
      rf_forecast_df = rf_setup(reference_year, reference_quarter, forecast_seq_dates)
      stat_display_df = stat_display_df %>%
        mutate(RF_RMSE = round(rf_forecast_df$rmse, 3)) %>%
        select(-Forecast_Horizon)
      stat_display_df = t(stat_display_df)
      DT::datatable(stat_display_df, colnames = c("1 Step", '2 Step', '3 Step', '4 Step', '5 Step', '6 Step', "7 Step", "8 Step"))
    } else if (input$model_choice == "Combined"){
      combined_forecast_df <- combined_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept, forecast_seq_dates)
      print("this is stat table combined forecast")
      print(combined_forecast_df)
      stat_display_df = stat_display_df %>%
        mutate(Combined_Forecast_RMSE = round(combined_forecast_df$rmse, 3)) %>%
        select(-Forecast_Horizon)
      stat_display_df = t(stat_display_df)
      DT::datatable(stat_display_df, colnames = c("1 Step", '2 Step', '3 Step', '4 Step', '5 Step', '6 Step', "7 Step", "8 Step"))
    } else if (input$model_choice == "Most Optimal") {
      actual_values <- c()
      rmse_values <- c()
      adl_models_rmse <- c()
      ar_models_rmse <- c()
      combined_models_rmse <- c()
      rf_models_rmse <- rf_cal_rmsfe(reference_year, reference_quarter)
      final_dataframe <- data.frame()
      
      for(i in 1:8){
        print(i)
        rmse <- c()
        
        optimal_ar_model <- fitAR_model(reference_year, reference_quarter, stat_gdp, i, 8)
        ar_rmse <- ARBacktest(reference_year, reference_quarter, optimal_ar_model, stat_gdp, gdp_date, i, 8)$rmse
        rmse <- c(rmse, ar_rmse)
        
        
        optimal_adl_model <- fit_adl(reference_year, reference_quarter, routput_gdp, hstart_gdp, i, 8)
        adl_rmse <- ADLbacktest(reference_year, reference_quarter, optimal_adl_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, i, 8)$rmse
        rmse <- c(rmse,  adl_rmse)
        
        rmse <- c(rmse, rf_models_rmse[i])
        
        
        combined_rmse  <- combined_backtest(reference_year, reference_quarter, routput_gdp, gdp_date, hstart_gdp, hstart_date, i, rf_models_rmse[i], 8)
        rmse <- c(rmse, combined_rmse)
        
        
        new_row <- optimal_setup(reference_year, reference_quarter, reference_col, input, x_intercept, rmse, i,forecast_seq_dates[i], forecast_seq_dates, rf_models_rmse[i] )
        print(new_row)
        
        final_dataframe <- rbind(final_dataframe, new_row)
      }
      stat_display_df = stat_display_df %>%
        mutate(Most_Optimal_RMSE = round(final_dataframe$rmse, 3)) %>%
        select(-Forecast_Horizon)
      stat_display_df = t(stat_display_df)
      DT::datatable(stat_display_df, colnames = c("1 Step", '2 Step', '3 Step', '4 Step', '5 Step', '6 Step', "7 Step", "8 Step"))
    }
  })
  
  
  observeEvent(input$show_about, {
    text_about = "As one of the fundamental indicators of economic activity and a pivotal metric guiding policy decisions, precise forecasting of GDP is imperative for policymakers, businesses, and individuals. However, GDP figures often undergo revisions, resulting in disparities between initial projections and final numbers. These revisions can profoundly influence the dependability and efficacy of macroeconomic forecasting models when operating with real-time data.
Hence, our project endeavors to construct and assess resilient forecasting models adept at integrating updated GDP data to deliver precise predictions."
    showModal(modalDialog(text_about, title = "About"))
    
    
  })
  
  
  output$plot_description <- renderText({
    if (input$model_choice == "Autoregressive Model (AR)"){
      title <- "Description"
      description <- "We used an autoregressive (AR) model to forecast GDP values. In this case, GDP values are being forecasted based on their own past values.
    In our model, through estimating the optimal lag lengths, it selects the most relevant past GDP values to predict future trends."
      paste(title, description, sep = ": ")
    } else if (input$model_choice == "Random Forest") {
      title <- "Description"
      description <- "A Random Forest is an ensemble learning method that builds multiple decision trees during training and combines their predictions to improve accuracy and reduce overfitting. It randomly selects subsets of data and features for each tree, then aggregates their predictions to make the final prediction."
      paste(title, description, sep = ": ")
    }else if (input$model_choice == "Autoregressive Distributed Lag Model (ADL)") {
      title <- "Description"
      description <- "We employed an autoregressive distributed lag (ADL) model to forecast GDP values. In this approach, GDP predictions are made by considering both the historical values of GDP and the past values of other relevant variables such as Housing Start. Housing start measures the total number of new privately owned housing units for which construction was initiated during a specified period. Our model, through the estimation of optimal lag lengths for each variable, selects the most relevant past values to forecast future GDP trends."
      paste(title, description, sep = ": ")
    }else if (input$model_choice == "Combined") {
      title <- "Description"
      description <- "We used a combined model to forecast GDP values, which takes the average of predictions from our previous three models: Autoregressive (AR), Autoregressive Distributed Lag (ADL), and Random Forest. In this approach, GDP values are forecasted not only based on their own past values but also considering additional explanatory variables like Housing Start and non-linear patterns captured by the different models. The combined model reduces model uncertainty as it takes the average of all 3 models predictions."
      paste(title, description, sep = ": ")
    }else if (input$model_choice == "Most Optimal") {
      title <- "Description"
      description <- " The most optimal model is developed by selecting the best-performing model for each specific combination of year and forecast horizon. This selection is based on comparing the Root Mean Squared Forecast Error (RMSFE) from backtesting across all four models - (AR, ADL, Random Forest and Combined), choosing the one with the lowest RMSFE to ensure superior performance."
      paste(title, description, sep = ": ")
    }
  })
  
  output$table_forecast = renderTable({
    
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    forecast_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    forecast_seq_dates = seq(forecast_start_date, length.out = 8, by = 1/4)
    
    reference_quarter_numeric = as.numeric(substr(input$quarter, start = nchar(input$quarter), stop = nchar(input$quarter)))
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
    true_value_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    true_value_seq_dates = seq(true_value_start_date, length.out = 8, by = 1/4)
    
    
    last_available_vintage = get_last_available_vintage(stat_df)
    true_df = stat_df %>% 
      select(DATE, last_available_vintage) %>%
      filter(DATE %in% true_value_seq_dates)
    
    
    if (input$model_choice == "Autoregressive Model (AR)") {
      print("inside table forecast")
      ar_forecast_df = ar_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
      
      print(ar_forecast_df)
      ar_forecast_df = ar_forecast_df %>%
        filter(DATE %in% true_value_seq_dates) 
      
      table_forecast_df = ar_forecast_df %>%
        select(DATE, point_forecast) 
      
      combined_df = full_join(true_df, table_forecast_df, by = "DATE") 
      summary_df = combined_df %>%
        select(DATE, last_available_vintage, point_forecast)%>%
        mutate(Difference = ROUTPUT24Q1 - point_forecast) %>%
        mutate(DATE = format(DATE, "%Y-Q%q")) %>%
        rename(Date = DATE,
               "Forecasting Error" = Difference, 
               "Latest Vintage Values" = ROUTPUT24Q1, 
               "Point Forecast" = point_forecast)
      
    } else if(input$model_choice == "Autoregressive Distributed Lag Model (ADL)") {
      
      adl_forecast_df = adl_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept)
      adl_forecast_df = adl_forecast_df %>%
        filter(DATE %in% true_value_seq_dates)
      
      table_forecast_df = adl_forecast_df %>%
        select(DATE, point_forecast)
      
      combined_df = full_join(true_df, table_forecast_df, by = "DATE") 
      summary_df = combined_df %>%
        select(DATE, ROUTPUT24Q1, point_forecast)%>%
        mutate(Difference = ROUTPUT24Q1 - point_forecast) %>%
        mutate(DATE = format(DATE, "%Y-Q%q")) %>%
        rename(Date = DATE,
               "Forecasting Error" = Difference, 
               "Latest Vintage Values" = ROUTPUT24Q1, 
               "Point Forecast" = point_forecast)
    } else if (input$model_choice == "Random Forest") {
      
      rf_forecast_df = rf_setup(reference_year, reference_quarter, forecast_seq_dates) 
      table_forecast_df = data.frame(DATE = true_value_seq_dates, 
                                     point_forecast = rf_forecast_df$point_forecast)
      
      combined_df = full_join(true_df, table_forecast_df, by = "DATE") 
      summary_df = combined_df %>%
        select(DATE, ROUTPUT24Q1, point_forecast)%>%
        mutate(Difference = ROUTPUT24Q1 - point_forecast) %>%
        mutate(DATE = format(DATE, "%Y-Q%q")) %>%
        rename(Date = DATE,
               "Forecasting Error" = Difference, 
               "Latest Vintage Values" = ROUTPUT24Q1, 
               "Point Forecast" = point_forecast)
      
    } else if (input$model_choice == "Combined") {
      combined_forecast_df <- combined_setup(reference_year, reference_quarter, reference_col, stat_gdp, hstart_gdp, input, x_intercept, forecast_seq_dates)
      table_forecast_df = data.frame(DATE = true_value_seq_dates, 
                                     point_forecast = combined_forecast_df$point_forecast)
      combined_df = full_join(true_df, table_forecast_df, by = "DATE") 
      summary_df = combined_df %>%
        select(DATE, ROUTPUT24Q1, point_forecast)%>%
        mutate(Difference = ROUTPUT24Q1 - point_forecast) %>%
        mutate(DATE = format(DATE, "%Y-Q%q")) %>%
        rename(Date = DATE,
               "Forecasting Error" = Difference, 
               "Latest Vintage Values" = ROUTPUT24Q1, 
               "Point Forecast" = point_forecast)
      
    } else if (input$model_choice == "Most Optimal") {
      actual_values <- c()
      rmse_values <- c()
      adl_models_rmse <- c()
      ar_models_rmse <- c()
      combined_models_rmse <- c()
      rf_models_rmse <- rf_cal_rmsfe(reference_year, reference_quarter)
      final_dataframe <- data.frame()
      
      for(i in 1:8){
        print(i)
        rmse <- c()
        
        optimal_ar_model <- fitAR_model(reference_year, reference_quarter, stat_gdp, i, 8)
        ar_rmse <- ARBacktest(reference_year, reference_quarter, optimal_ar_model, stat_gdp, gdp_date, i, 8)$rmse
        rmse <- c(rmse, ar_rmse)
        
        
        optimal_adl_model <- fit_adl(reference_year, reference_quarter, routput_gdp, hstart_gdp, i, 8)
        adl_rmse <- ADLbacktest(reference_year, reference_quarter, optimal_adl_model, stat_gdp, gdp_date, hstart_gdp, hstart_date, i, 8)$rmse
        rmse <- c(rmse,  adl_rmse)
        
        rmse <- c(rmse, rf_models_rmse[i])
        
        
        combined_rmse  <- combined_backtest(reference_year, reference_quarter, routput_gdp, gdp_date, hstart_gdp, hstart_date, i, rf_models_rmse[i], 8)
        rmse <- c(rmse, combined_rmse)
        
        
        new_row <- optimal_setup(reference_year, reference_quarter, reference_col, input, x_intercept, rmse, i,forecast_seq_dates[i], forecast_seq_dates, rf_models_rmse[i] )
        print(new_row)
        
        final_dataframe <- rbind(final_dataframe, new_row)
      }
      
      table_forecast_df = data.frame(DATE = true_value_seq_dates, 
                                     point_forecast = final_dataframe$point_forecast)
      combined_df = full_join(true_df, table_forecast_df, by = "DATE") 
      summary_df = combined_df %>%
        select(DATE, ROUTPUT24Q1, point_forecast)%>%
        mutate(Difference = ROUTPUT24Q1 - point_forecast) %>%
        mutate(DATE = format(DATE, "%Y-Q%q")) %>%
        rename(Date = DATE,
               "Forecasting Error" = Difference, 
               "Latest Vintage Values" = ROUTPUT24Q1, 
               "Point Forecast" = point_forecast)
    }
  })
  
  output$table_label <- renderText({
    "Summary Table:"
  })
  
  output$rf_feature_importance = renderPlot({
    if (input$model_choice == "Random Forest"){
      lines <- readLines("rf_feature_importance_plot/impt.txt")
      impt <- lapply(lines,function(line) strsplit(line,", ")[[1]])
      lines2 <- readLines("rf_feature_importance_plot/name.txt")
      names<- lapply(lines2,function(line) strsplit(line,", ")[[1]])
      
      name1<- names[[1]]
      n1 <- impt[[1]]
      n2 <- impt[[2]]
      n3 <- impt[[3]]
      n4 <- impt[[4]]
      n5 <- impt[[5]]
      n6 <- impt[[6]]
      n7 <- impt[[7]]
      n8 <- impt[[8]]
      
      df <- data.frame(name1 = name1,
                       n1 = n1,
                       n2 = n2,
                       n3 = n3,
                       n4 = n4,
                       n5 = n5,
                       n6 = n6,
                       n7 = n7,
                       n8 = n8)
      
      # Transpose the dataframe and gather the columns
      transposed_df <- df %>%
        pivot_longer(cols = -name1, names_to = "category", values_to = "value") %>%
        mutate(category = sub("n", "", category),
               category = paste0("t+", category)) %>%
        pivot_wider(names_from = name1, values_from = value)
      
      name3 <- sort(names(transposed_df))
      
      
      convert_to_numeric <- function(x) {
        if (!identical(x, 'category')) {
          as.numeric(x)
        } else {
          x
        }
      }
      
      numeric_df <- transposed_df %>%
        mutate_at(vars(-category), convert_to_numeric)
      
      # Mutate the columns to add them together or rename them based on the provided mapping
      mutated_df <- numeric_df %>%
        mutate(`Private Investment` = rinvbf + rinvchi,
               `Consumption` = RCONS + rconhh + rconsnp,
               `CPI` = CPI + P,
               `Industrial Production Index` = IPT,
               `Housing` = HSTARTS + rinvresid,
               `Labour Market/Productivity` = LFC + LFPART + POP + OPH + RUC,
               `Monetary` = M1,
               `Personal Income` = WSD + OLI + PINTI + PINTPAID + PROPI + PTAX,
               `Investment/Saving` = PTAX + rinvbf + rinvchi + rinvresid +RATESAV,
               `Export/Import` = RNX + REX + RIMP,
               `Govt Expenditure` = RG +RGF +RGSL,
               `GDP` = ROUTPUT) %>%
        select(-rinvbf, -rinvchi, -RCONS, -rconhh, -rconsnp, -CPI, -P, -IPT, 
               -HSTARTS, -rinvresid, -LFC, -LFPART, -POP, -OPH, -RUC, -M1, 
               -WSD, -OLI, -PINTI, -PINTPAID, -PROPI, -PTAX, -RNX, -REX, -RIMP, -ROUTPUT,-RATESAV
               ,-RG,-RGF,-RGSL)
      
      
      long_df <- tidyr::pivot_longer(mutated_df, cols = -category, names_to = "t", values_to = "value")
      
      # Plot stacked bar chart
      ggplot(long_df, aes(x = category, y = value, fill = t)) +
        geom_bar(stat = "identity") +
        labs(x = "", y = "Feature Importance", fill = "Category") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } 
  })
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

