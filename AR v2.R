library(readxl)
library(forecast)
library(zoo)
library(tseries)
library(tibble) # Assuming data is in a tibble
library(tidyr) # For pivoting the data
library(dplyr)
library("BVAR")

FORECAST_HORIZON = 3 # user input from the frontend
MAX_LAG = 15

fitAR=function(Y,p,h){
  
  #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
  
  
  aux=embed(Y,p+h) #create p lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  -> remove everything not included in the direct regression --> ncol(Y) = 1 if it's a single regression 
  
  X.out=tail(aux,1)[1:ncol(X)] #retrieve last p observations 
  
  
  
  model=lm(y~X) #estimate direct h-step AR(p) by OLS 
  coef=coef(model) #extract coefficients
  
  
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector, 1 here is used to times constant coefficient
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}

find_best_model_aic=function(stat_gdp_column){
  clean_column = na.omit(stat_gdp_column)
  Y=as.matrix(clean_column) 
  aic_values <- vector("numeric", MAX_LAG)
  for(p in 1:MAX_LAG){
    model <- fitAR(Y, p, h=1)
    aic_values[p] <- AIC(model$model)
  }
  best_lag <- which.min(aic_values)
  print(best_lag)
  
  best_model <- fitAR(Y, best_lag, h=1)
  return(best_model)
}

find_best_model_loocv=function(stat_gdp_column){
  forecast_horizon = FORECAST_HORIZON
  column_clean <- na.omit(stat_gdp_column)
  lag_range = 1:MAX_LAG
  
  for(lag in lag_range){
    mse <- numeric(MAX_LAG)
    starting_index <- forecast_horizon + lag
    Y <- column_clean[starting_index:length(column_clean)]
    n_rows <- length(column_clean) - starting_index + 1
    X <- matrix(nrow = n_rows, ncol = lag + 1)
    for(i in starting_index:length(column_clean)){
      X[i - starting_index + 1, ] <- c(1, column_clean[(i-forecast_horizon-lag+1):(i-forecast_horizon)])
      
      
    }
    X_transpose <- t(X)
    
    # Multiplying X' and X
    XX <- X_transpose %*% X
    
    # Inverting (X'X)
    XX_inv <- solve(XX)
    
    # Multiplying (X'X)^-1 with X'
    XX_inv_X_transpose <- XX_inv %*% X_transpose
    
    # Final multiplication with X to get X(X'X)^-1X'
    projection_matrix <- X %*% XX_inv_X_transpose
    
    e = Y-projection_matrix
    hii = diag(e)
    running_sum = 0
    
    for(i in 1:length(hii)){
      inv_hii = (1-hii[i])^(-1)
      running_sum = running_sum + sum((e[,i] * inv_hii)^2)
      
    }
    mse[lag-1]= running_sum/length(Y)
    
    
    
  }
  best_lag = which.min(mse)
  Y=as.matrix(column_clean) 
  best_model <- fitAR(Y, best_lag, h=FORECAST_HORIZON)
  return(best_model)
  

  
}

construct_all_models=function(stat_gdp){
  model_list = list()
  for(i in 1:ncol(stat_gdp)){
    if(FORECAST_HORIZON == 1){
      model_list[[names(stat_gdp)[i]]] = find_best_model_aic(stat_gdp[,i])
    }else{
      model_list[[names(stat_gdp)[i]]] = find_best_model_loocv(stat_gdp[,i])
    }
  }
  return(model_list)
}

data <- read_excel('ROUTPUTQvQd.xlsx')

gdp_num <- data[, 2:ncol(data)] 
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-data[,1]

# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))  # Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)


best_models_aic = list()
best_models_loocv = list()

if(FORECAST_HORIZON == 1){
  best_models_aic = construct_all_models(stat_gdp)
  
}else{
  best_models_loocv = construct_all_models(stat_gdp)
}


best_models_aic[[3]]$pred




view(stat_gdp)





















############# TRIAL CODE BELOW
# 
# lag_range <- 2:8
# 
# mse <- numeric(7)
# 
# column <- stat_gdp[,53]
# forecast_horizon = 6
# column_clean <- na.omit(column)
# 
# 
# for(lag in lag_range){
#   starting_index <- forecast_horizon + lag
#   Y <- column_clean[starting_index:length(column_clean)]
#   n_rows <- length(column_clean) - starting_index + 1
#   X <- matrix(nrow = n_rows, ncol = lag + 1)
#   for(i in starting_index:length(column_clean)){
#     X[i - starting_index + 1, ] <- c(1, column_clean[(i-forecast_horizon-lag+1):(i-forecast_horizon)])
#     
#     
#   }
#   X_transpose <- t(X)
#   
#   # Multiplying X' and X
#   XX <- X_transpose %*% X
#   
#   # Inverting (X'X)
#   XX_inv <- solve(XX)
#   
#   # Multiplying (X'X)^-1 with X'
#   XX_inv_X_transpose <- XX_inv %*% X_transpose
#   
#   # Final multiplication with X to get X(X'X)^-1X'
#   projection_matrix <- X %*% XX_inv_X_transpose
#   
#   e = Y-projection_matrix
#   hii = diag(e)
#   running_sum = 0
#   
#   for(i in 1:length(hii)){
#     inv_hii = (1-hii[i])^(-1)
#     running_sum = running_sum + sum((e[,i] * inv_hii)^2)
#     
#   }
#   mse[lag-1]= running_sum/length(Y)
#   
#   
#   
# }
# 
# 
# 
# column <- stat_gdp[,1]
# forecast_horizon = 3
# 
# lag = 7
# column_clean <- na.omit(column)
# 
# starting_index <- forecast_horizon + lag
# 
# Y <- column_clean[starting_index:length(column_clean)]
# 
# n_rows <- length(column_clean) - starting_index + 1
# 
# X <- matrix(nrow = n_rows, ncol = lag + 1)
# 
# # sample_prediction = best_models[[1]]$pred[,1]
# 
# 
# 
# for(i in starting_index:length(column_clean)){
#   X[i - starting_index + 1, ] <- c(1, column_clean[(i-forecast_horizon-lag+1):(i-forecast_horizon)])
#   
#   
# }
# 
# X_transpose <- t(X)
# 
# # Multiplying X' and X
# XX <- X_transpose %*% X
# 
# # Inverting (X'X)
# XX_inv <- solve(XX)
# 
# # Multiplying (X'X)^-1 with X'
# XX_inv_X_transpose <- XX_inv %*% X_transpose
# 
# # Final multiplication with X to get X(X'X)^-1X'
# projection_matrix <- X %*% XX_inv_X_transpose
# 
# e = Y-projection_matrix
# hii = diag(e)
# running_sum = 0
# 
# for(i in 1:length(hii)){
#   inv_hii = (1-hii[i])^(-1)
#   running_sum = running_sum + sum((e[,i] * inv_hii)^2)
#   
# }
# running_sum/length(Y)
# 
# 
# 
# 
# solve(1-hii)
# column_clean[(starting_index-forecast_horizon-lag+1):(starting_index-forecast_horizon)]
# 
