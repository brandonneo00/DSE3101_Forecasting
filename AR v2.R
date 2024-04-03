library(readxl)
library(forecast)
library(zoo)
library(tseries)
library(tibble) # Assuming data is in a tibble
library(tidyr) # For pivoting the data
library(dplyr)
library(fitAR)
library("BVAR")
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

data <- read_excel('ROUTPUTQvQd.xlsx')


# Read the Excel file using the correct file path 
# Convert all columns except the first one (which contains dates) to numeric, keeping NA values as NA 
gdp_num <- data[, 2:ncol(data)] 
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-data[,1]


# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))  # Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)


models_list <- list()

max_lags <- 6

first_column <- stat_gdp[1]

column_non_na <- na.omit(first_column)

first_column_matrix <- as.matrix(column_non_na)

column_ts <- ts(column_non_na, start=c(1947, 2), frequency=4)

aic_values <- numeric(length(max_lags))
models <- numeric(length(max_lags))

for(p in 1:max_lags){
  print(p)
  model <- fitAR(first_column_matrix, p, h=1)
  
  aic_values[p] <- AIC(model$model)
}

aic_values

best_lag <- which.min(aic_values)
best_lag
best_model <- fitAR(first_column_matrix, best_lag, h=1)
