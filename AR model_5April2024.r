library(readxl)
library(forecast)
library(zoo)
library(tseries)
library(tibble) # Assuming data is in a tibble
library(tidyr) # For pivoting the data
library(dplyr)
library("BVAR")


data <- read_excel('ROUTPUTQvQd.xlsx')
date = data %>%
  select(DATE)
date

gdp_num <- data[, 2:ncol(data)] 
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-data[,1]

# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))  # Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)


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
    best_model_lag = which.min(unlist(all_models_aic)) #optimal lag
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
  return(best_model_lag)
}

test = fitAR(65, "Q4", stat_gdp, 2, 2)
test

test = fitAR(65, "Q4", stat_gdp, 1, 2)
test

for (i in 65:99){
  print(fitAR(i, "Q4", stat_gdp, 1, 8))
}

fitAR("70","Q4",stat_gdp,4,10)

