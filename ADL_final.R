library(readxl)
library(forecast)
library(zoo)
library(tseries)
library(tibble) # Assuming data is in a tibble
library(tidyr) # For pivoting the data
library(dplyr)
library(dynlm)
library("BVAR")

routput <- read_excel('ROUTPUTQvQd.xlsx')
hstart <- read_excel('hstartsMvMd.xlsx')

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
    print(best_model_key)
    best_model = all_models[best_model_key]
    
  } else{ #for beyond 1 step ahead forecast
    all_models_loocv_mse = list()
    
    for (i in 1:max_lags){
      #which AR model we testing
      label = paste("AR", i, sep="")
      
      #creating the X matrix based on AR model
      subset_X_mat = as.matrix(X_mat[, 1:i])
      print(subset_X_mat)
      
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
  
  # Construct the column name in hstart_df
  hstart_col <- paste("HSTARTS", year, month, sep = "")
  
  # Return the subset of hstart_df
  return(hstart_df[, hstart_col])
}

fit_adl <- function(vintage_year, vintage_quarter, routput_df, hstart_df, forecast_horizon, max_lags){
  col_prefix = "ROUTPUT"
  reference_col = paste(col_prefix, vintage_year, vintage_quarter, sep="")
  subset_routput_df = as.matrix(routput_df %>% select(reference_col))
  subset_hstart_df = match_quarter_to_month(reference_col, hstart_df)
  routput_ts <- ts(na.omit(subset_routput_df), start=c(1947,1), frequency = 4)
  hstart_ts <- ts(na.omit(subset_hstart_df), start=c(1947,1), frequency=4)
  
  
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
  }
  else if (forecast_horizon >= 2){
    lag =3 
    aux = embed(subset_routput_df, (max_lags + forecast_horizon))
    aux = aux[complete.cases(aux), ]
    y = aux[, 1]
    X = data.frame(aux[, -1])
    X_mat = as.matrix(aux[, -1])
    subset_X_mat_routput = as.matrix(X_mat[, 1:lag])
    
    aux = embed(subset_hstart_df, (max_lags + forecast_horizon))
    aux = aux[complete.cases(aux), ]
    y = aux[, 1]
    X = data.frame(aux[, -1])
    X_mat = as.matrix(aux[, -1])
    subset_X_mat_hstart = as.matrix(X_mat[, 1:lag])
    
    rows_routput <- dim(subset_X_mat_routput)[1]
  
    X_mat_hstart = subset_X_mat_hstart[1:rows_routput,]
    combined_matrix<- cbind(1, subset_X_mat_routput, X_mat_hstart)
    
    aux = embed(subset_routput_df, (max_lags + forecast_horizon))
    aux = aux[complete.cases(aux), ]
    y = aux[, 1]
    
    beta_hat = solve(t(combined_matrix) %*% combined_matrix) %*% (t(combined_matrix) %*% y)
    
    #predicted value
    y_hat = combined_matrix %*% beta_hat
    
    #residuals
    residual = y - y_hat
    
    #projection matrix
    projection_mat = combined_matrix %*% solve(t(combined_matrix) %*% combined_matrix) %*% t(combined_matrix)
    
    #diagonal of H
    hii = diag(projection_mat)
    print(hii)

  }
  
  # return(best_model)
  
}


fit_adl(70, "Q4", routput_gdp, hstart_gdp, 2, 6)
#testing the fucntion
match_quarter_to_month("ROUTPUT70Q4", hstart_quarterly)


test = fitAR(71, "Q4", stat_gdp, 3, 8)
test

for (i in 65:99){
  print(fitAR(i, "Q4", stat_gdp, 2, 8))
}

fitAR("70","Q4",stat_gdp,4,10)


