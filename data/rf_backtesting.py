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

##go back 50 data pts
def subtract_quarters(year, start_quarter, num_quarters):
    # Extract the year and quarter from the start_quarter
    zz, quarter = start_quarter.split("Q")
    year = int(year)
    quarter = int(quarter)
    
    # Convert the start_quarter to its numeric representation
    numeric_rep = year + (quarter - 1) / 4
    # Subtract num_quarters from the numeric representation
    new_numeric_rep = numeric_rep - num_quarters / 4
    # Calculate the new year and quarter
    new_year = int(new_numeric_rep)
    new_quarter = int((new_numeric_rep - new_year) * 4) + 1
    
    # Format the new quarter
    new_quarter_str = f"{new_year}Q{new_quarter}"
    
    return new_quarter_str[2:]


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


def random_forest (lookup,horizon, n_tree = 50):
    
    step = horizon

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
    #print("Dimensions of X_lag:", X_lag.shape)
    #print("Dimensions of X_train:", X_train.shape)
    #print("Dimensions of X_test:", X_test.shape)
    #print("Dimensions of y_train:", y_train.shape)

    rf_model = RandomForestRegressor(n_estimators= n_tree)
    rf_model.fit(X_train, y_train)
    predictions = rf_model.predict(X_test) 
    return predictions[-1]

def rf(lookup,horizon, rep):
    result = 0
    for i in range(rep):
        result+= random_forest(lookup,horizon)
    return result/ rep


def rf_pred (x1, x2, x3, rep):
    horizon = int(x3[0])
    rf_pred = []
    lookup = x1[-2:] + x2


    for i in range(1,horizon+1):
        rf_pred.append(  rf(lookup,i,rep) )
    return rf_pred

year_name = []

def back_pred(x1, x2, x3, rep):
    name = []
    horizon = int(x3[0])
    
    back_pred = []
    
    for i in range(50):
        new_quarter = subtract_quarters(x1, x2, 50 - i)
        pred = rf(new_quarter, horizon, rep)
        back_pred.append(pred)
        
        if len(name) == 0 or len(back_pred) == 50:
            name.append(new_quarter)

    return back_pred, name

def pred_50(x1, x2, x3, rep):
    gg = back_pred(x1, x2, x3, rep)
    year_name.append(gg[1])
    return gg[0]


def convert_date(date_str):

    year = date_str.split('Q')[0]
    quarter = date_str.split('Q')[1]
    return str(year) + ":Q" + str(quarter)

def get_test(start_date, end_date, gdp):
    # Convert start_date and end_date to year and quarter
    start= convert_date(start_date)
    end = convert_date(end_date)
    
    # Find the index of start_date and end_date

    start_index = gdp[gdp['DATE'].str.endswith(start)].index[0]
    #print(start_index)
    # Find the index of the row where 'date' ends with end_date
    end_index = gdp[gdp['DATE'].str.endswith(end)].index[0]
    #print(end_index)
    # Slice the DataFrame from start_index to end_index
    filtered_gdp = gdp.loc[start_index:end_index]
    # Extract ROUTPUT24Q1 column
    output_data = filtered_gdp['ROUTPUT24Q1']
    
    return output_data


def get_rmse(pred):
    start = year_name[0][0]
    end = year_name[0][1]
    test = get_test(start,end,gdp)
    
    mse = mean_squared_error(test, pred)
    rmse = np.sqrt(mse)
    return rmse





               
def back_pred2(x1, x2, n, rep):
    name = []
    horizon = n
    
    back_pred = []
    
    for i in range(50):
        new_quarter = subtract_quarters(x1, x2, 50 - i)
        pred = rf(new_quarter, horizon, rep)
        back_pred.append(pred)
        
        if len(name) == 0 or len(back_pred) == 50:
            name.append(new_quarter)

    return back_pred, name

def get_rmse2(pred,year_q):
    start = year_q[0]
    end = year_q[1]
    test = get_test(start,end,gdp)
    mse = mean_squared_error(test, pred)
    rmse = np.sqrt(mse)
    return rmse


def total_rmse(x1, x2, x3, rep):
    horizon = int(x3[0])
    rmse_total = []
    for i in range(1,horizon+1):
        result_arrays = back_pred2(x1, x2, i, rep)
        vector_50 = result_arrays[0]
        year_quarter = result_arrays[1]
        sub_rmse = get_rmse2(vector_50,year_quarter)
        rmse_total.append(sub_rmse)
    return rmse_total