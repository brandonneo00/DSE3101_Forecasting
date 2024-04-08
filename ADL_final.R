library(readxl)
library(forecast)
library(zoo)
library(tseries)
library(tibble) # Assuming data is in a tibble
library(tidyr) # For pivoting the data
library(dplyr)
library(dynlm)
library("BVAR")

routput <- read_excel('/Users/jiangruyang/Desktop/Y3S2/DSE3101/data/ROUTPUTQvQd.xlsx')
hstart <- read_excel('/Users/jiangruyang/Desktop/Y3S2/DSE3101/data/hstartsMvMd.xlsx')

hstart <- hstart %>%
  mutate(across(-DATE, ~ifelse(. == "#N/A", NA_real_, as.numeric(as.character(.)))))

routput <- routput %>%
  mutate(across(-DATE, ~ifelse(. == "#N/A", NA_real_, as.numeric(as.character(.)))))

hstart_quarterly <- hstart %>%
  mutate(DATE = as.yearqtr(DATE, format = "%Y:%m"), # Convert DATE to quarterly format
         DATE = format(DATE, "%Y:Q%q")) %>% # Adjust the format to "YYYY:QQ"
  group_by(DATE) %>%
  summarize(across(everything(), ~if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)), .groups = "drop")


routput_num <- routput[, 2:ncol(routput)] 
routput_date <- routput[,1]
transformation_codes <- rep(5, ncol(routput_num))  # Apply the fred_transform function with the transformation codes 
routput_gdp <- fred_transform(data = routput_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)

hstart_num <- hstart_quarterly[, 2:ncol(hstart_quarterly)] 
hstart_date <- hstart_quarterly[,1]
transformation_codes <- rep(5, ncol(hstart_num))  # Apply the fred_transform function with the transformation codes 
hstart_gdp <- fred_transform(data = hstart_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)


stat_gdp <- routput_gdp



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
  X = data.frame(aux[, -1])
  X_mat = as.matrix(aux[, -1])
  
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

    best_model = all_models[best_model_key]
    
  } else{ #for beyond 1 step ahead forecast
    all_models_loocv_mse = list()
    
    for (i in 1:max_lags){
      #which AR model we testing
      label = paste("AR", i, sep="")
      
      #creating the X matrix based on AR model
      subset_X_mat = as.matrix(X_mat[, 1:i])

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

find_optimal_lag_adl <- function(vintage_year, vintage_quarter, routput_df, hstart_df, forecast_horizon, max_lags){
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")
  subset_routput_df = as.matrix(routput_df %>% select(reference_col))
  subset_hstart_df = match_quarter_to_month(reference_col, hstart_df)
  # print(subset_hstart_df)
  routput_ts <- ts(na.omit(subset_routput_df), start=c(1947,1), frequency = 4)
  hstart_ts <- ts(na.omit(subset_hstart_df), start=c(1947,1), frequency=4)
  
  optimal_lags <- list()
  
  #print(routput_ts)
  #print(hstart_ts)
  
  
  if(forecast_horizon == 1){
    ar_model <- fitAR(vintage_year, vintage_quarter, routput_df, forecast_horizon, max_lags)
    optimal_ar_lag <- as.numeric(summary(ar_model)[1])-1
    aic <- numeric(max_lags)
    for(i in 1:max_lags){
      model <- dynlm(routput_ts ~ L(routput_ts, 1:optimal_ar_lag) + L(hstart_ts, 1:i))
      aic[i] <- AIC(model)
    }
    best_lag_hstart <- which.min(aic)
    best_model <- dynlm(routput_ts ~ L(routput_ts, 1:optimal_ar_lag) + L(hstart_ts, 1:best_lag_hstart))
    optimal_lags$routput_lag <- optimal_ar_lag
    optimal_lags$hstart_lag <- best_lag_hstart
  }
  else if (forecast_horizon >= 2){
    all_models_loocv_mse = list()
    
    for(i in 1:max_lags){
      label = paste("ADL", i, sep="")
      #print(label)
    
      aux = embed(subset_hstart_df, (max_lags + forecast_horizon))
      aux = aux[complete.cases(aux), ]
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
      
      if(num_rows_routput > num_rows_hstart){
        print("here1")
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
    print(best_model_lag_hstart)
    
    
    model_ar = fitAR(vintage_year, vintage_quarter, routput_df, forecast_horizon, max_lags)
    best_model_lag_routput = length(coef(model_ar))-1
    best_model <- dynlm(routput_ts ~ L(routput_ts, 1:best_model_lag_routput) + L(hstart_ts, 1:best_model_lag_hstart))

    optimal_lags$routput_lag <- best_model_lag_routput
    optimal_lags$hstart_lag <- best_model_lag_hstart

  }
  
  return(optimal_lags)
  
}

forecasting_values <- function(vintage_year, vintage_quarter, routput_gdp, hstart_gdp, forecast_horizon, optimal_lag_routput, optimal_lag_hstart ) {
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
  n_step_forecast <- predict(adl_model)  
  result = data.frame(n_step_forecast)  
  final_value = tail(result$n_step_forecast, 1) #retrieving the last value
  return(final_value)
}


adl_lags <- find_optimal_lag_adl("15", "Q3", routput_gdp, hstart_gdp, 2, 6)
routput_lag <- adl_lags$routput_lag
hstart_lag <- adl_lags$hstart_lag[[1]]

routput_lag
hstart_lag


adl_forecast <- forecasting_values("15", "Q3", routput_gdp, hstart_gdp, 2, routput_lag, hstart_lag)
print(adl_forecast)
