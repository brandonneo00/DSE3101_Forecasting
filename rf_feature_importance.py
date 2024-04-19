import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from math import sqrt
import matplotlib.pyplot as plt

#Read data
file_paths = [
    r"pop_mvmd.csv",
    r"propi.csv",
    r"ptax.csv",
    r"ratesav.csv",
    r"rcon.csv",
    r"rconhh_09Q3.csv",
    r"rcons.csv",
    r"rconsnp_09Q3.csv",
    r"rex.csv",
    r"rg.csv",
    r"rgf.csv",
    r"rgsl.csv",
    r"rimp.csv",
    r"rinvbf.csv",
    
    r"rinvrsid.csv",
    r"rnx.csv",
    r"ruc_md.csv",
    r"wsd.csv",
    r"cpi_md94Q3.csv",
    r"gdp.csv",
    r"hstarts_mvmd.csv",
    r"ipt_mvmd.csv",
    r"lfc_mvmd.csv",
    r"lfpart_mvmd.csv",
    r"m1_md.csv",
    r"oli.csv",
    r"oph.csv",
    r"oph_98Q4.csv",
    r"p.csv",
    r"pinti.csv",
    r"pintpaid.csv"
]
dataframes = ["rinvchi"]
for file_path in file_paths:
    try:
        # Extracting filename without extension
        file_name = file_path.split("\\")[-1].split(".")[0]
        # Reading CSV into DataFrame with variable name same as filename
        globals()[file_name] = pd.read_csv(file_path, encoding="utf-8")
        
        # Printing name and dimensions
        #print("Name:", file_name)
        #print("Dimensions:", globals()[file_name].shape)
        dataframes.append(file_name)
    except Exception as e:
        print(f"Error reading {file_path}: {e}")

rinvchi = pd.read_excel("rinvchiQvQd.xlsx")

monthly_dataframes = []

for file_name in dataframes:
    # Check if the number of rows in the DataFrame is greater than 308
    if len(globals()[file_name]) > 308:
        # Append the file name to the list
        monthly_dataframes.append(file_name)
def convert (df):
    quarterly_data = df[df['DATE'].str.endswith(('03', '06', '09', '12'))].reset_index(drop=True)

    #print(quarterly_data.shape)
    return quarterly_data

for file_name in monthly_dataframes:
    globals()[file_name] = convert(globals()[file_name])

ipt_mvmd = ipt_mvmd.iloc[-308:].reset_index(drop=True)


def get_month_code(lookup):
    suffix = lookup[-2:]
    month_mapping = {
        "Q1": "M3",
        "Q2": "M6",
        "Q3": "M9",
        "Q4": "M12"
    }
    if suffix in month_mapping:
        return lookup[:2] + month_mapping[suffix]
    else:
        return None



def data_extract(lookup):
    result_df = pd.DataFrame() 
    gdp_columns = [col for col in gdp if col.endswith(lookup)]
    if gdp_columns:
        # Extract the column along with its header
        result_df = pd.concat([result_df, gdp["DATE"]], axis=1)
        #result_df = pd.concat([result_df, gdp[gdp_columns[0]]], axis=1)



    #other predictor
    for name in dataframes: 
        df= globals()[name]  
        df_q = [col for col in df if col.endswith(lookup)]
        if df_q:
            result_df = pd.concat([result_df, df[df_q[0]]], axis=1)
        else:
            mlook = get_month_code(lookup)
            df_q = [col for col in df if col.endswith(mlook)]
            if df_q:
             result_df = pd.concat([result_df, df[df_q[0]]], axis=1)
    return result_df

#Add lags
#use lagged observations (e.g. t-x to t-12) as input variables to forecast the current time step (t) , where x is the number of steps ahead
def series_to_supervised(data, col_name, n_in=1, steps = 1):
    """
    Frame a time series as a supervised learning dataset.
    Arguments:
    data: Sequence of observations as a list, NumPy array, or pandas DataFrame/Series.
    Col_name: name of column to transform
    n_in: Number of lag observations as input (X).
    n_out: Number of observations as output (y).
    dropnan: Boolean whether or not to drop rows with NaN values.
    Returns:
    Pandas DataFrame of series framed for supervised learning.
    """
    n_vars = 1
    if isinstance(data, list) or isinstance(data, np.ndarray):
        n_vars = 1
    elif isinstance(data, pd.DataFrame):
        n_vars = data.shape[1]
    elif isinstance(data, pd.Series):
        n_vars = 1
        data = pd.DataFrame(data)
    else:
        raise ValueError("Unsupported data type. Please provide data as a list, NumPy array, or pandas DataFrame/Series.")

    df = pd.DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, steps-1,-1):
        cols.append(df.shift(i))
        names += [(f'{col_name}(t-{i})')]

    # put it all together
    agg = pd.concat(cols, axis=1)
    agg.columns = names
    #agg.dropna(subset=[agg.columns[-1]], inplace=True)


    return agg

def add_na(data, name, n):
    if isinstance(data, pd.DataFrame):
        # Assuming you want to operate on the first column of the DataFrame
        yy = data.iloc[:, 0].dropna()
    elif isinstance(data, pd.Series):
        yy = data.dropna()
    else:
        raise ValueError("Input data must be a DataFrame or Series")

    # Reset the index of yy to ensure unique index values
    yy = yy.reset_index(drop=True)

    # Create a Series of NaN values with the same length as yy
    xx = pd.Series([np.nan] * n, index=range(len(yy)+1, len(yy) + n+1))
    # Set the name of the xx series to be the same as the original column name
    xx.name = name
    return pd.concat([yy, xx])

def random_forest (x1, x2,horizon,feature_importance, feature_name, n_tree = 50):
    lookup = x1[-2:] + x2
 

    df = data_extract(lookup)
    df = df[df.loc[:, df.columns.str.startswith('ROUTPUT')].notna().any(axis=1)]

    X_no_lag = df
    X_no_lag = X_no_lag.drop(columns=['DATE'])
    X_no_lag = X_no_lag.loc[:, ~X_no_lag.columns.duplicated()]


    y_no_lag = df.loc[:, df.columns.str.startswith('ROUTPUT')].iloc[:,0]


    # Drop NA rows at the bottom of X_no_lag
    #X_no_lag = X_no_lag.dropna(how='all', axis=0)
    # Drop NA rows at the bottom of y_no_lag
    #y_no_lag = y_no_lag.dropna(how='all')
    X_lag = pd.DataFrame()
    X_train = pd.DataFrame()
    X_test = pd.DataFrame()

    X_cols = list(set(X_no_lag.columns.tolist()))

    for i in range(len(X_cols)):
        c = X_cols[i]
        try:
            col_na = add_na (X_no_lag[c],c, horizon)
            lagged_col = series_to_supervised(col_na, c, 12,horizon)
            X_lag = pd.concat([X_lag, lagged_col], axis=1)


        except Exception as e:
            print(f"Error occurred for column: {c}, Error: {e}")
            
    #X_lag = X_lag.dropna(how='all')

    y_train = y_no_lag [horizon:,]
    #X_train = X_lag.iloc[horizon:-horizon,]  
    X_train = X_lag.iloc[horizon:horizon+len(y_train) ,]    
    y_train = y_no_lag [horizon:,]
    X_test = X_lag.iloc[-horizon:,]

    rf_model = RandomForestRegressor(n_estimators= n_tree)
    rf_model.fit(X_train, y_train)
    
    
    if horizon < len(feature_importance)+1:
    # If it exists, add rf_model.feature_importances_ to the existing one
        feature_importance[horizon-1] += rf_model.feature_importances_
    else:
    # If it doesn't exist, append rf_model.feature_importances_ to the array
        feature_importance.append([rf_model.feature_importances_])
        feature_name.append(X_train.columns)
    predictions = rf_model.predict(X_test) 
    return predictions

#######################################input
feature_importance_scores = []
feature_names =[]
n_rep = 1
###########################################

def rf (x1, x2,n_rep):
    horizon = 8
    prediction_array = np.array([])
    
    for i in range(1, horizon+1):
        for j in range (n_rep):
            result = 0
            
            result = result + random_forest (x1, x2, i,feature_importance_scores, feature_names, n_tree = 15) [-1]
        
            #print(prediction_array)
        prediction_array= np.append(prediction_array, result)
        # Iterate over each element of the feature_importance list and divide by rep

    return prediction_array/n_rep


        
def get_feature ():
    return feature_importance_scores

def get_feature_names():
    return feature_names



#feature importance


def get_unique_feature_impt():
    dd = get_feature() 
    result = [[x_i / n_rep for x_i in x] for x in dd]
    impt = []
    for i in range (len(result)):
        sub_array = result[i][0]
        step = 12-i
        # Iterate over the sub_array
        sums = []
        for j in range(0, len(sub_array), step):
            # Sum every 12 consecutive elements
            subset_sum = sum(sub_array[j:j+step])
            # Append the sum to the sums list
            sums.append(subset_sum)
        impt.append(sums)
    return impt


def get_unique_feature_names():
    index_list = get_feature_names()
    unique_modified_indices = []
    for i in range(len(index_list)):
        sub_list = index_list[i]


        modified_indices = []
        # Loop through the index list and extract the desired substring
        for j in range(len(sub_list)):
            index = sub_list[j]
            # Extract the substring before '(' 
            start_index = index.find('(')
            end_index = index.find(')')
            if start_index != -1 and end_index != -1:
                substring = index[0:start_index-4]
                modified_indices.append(substring)

        unique= list(set(modified_indices))
        unique_modified_indices.append(unique)

    return unique_modified_indices
