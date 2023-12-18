import datetime
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
import sys

code_dir = "river-dl"
sys.path.append(code_dir)

from river_dl.preproc_utils import split_into_batches



def fill_x_data_gaps(df):
    df = df.replace({-9999: np.nan})
    df = df.interpolate(limit_direction='both')
    return df


def create_df_template(df_all):
    date_idx = pd.date_range(start=df_all.Datetime.min(),
                                end=df_all.Datetime.max(),
                                freq='2H')
    depths = df_all.Depth_m.unique()
    depths = depths[depths != np.nan]

    df_template = pd.DataFrame(index=date_idx, columns=depths)
    df_template.index.name = 'Datetime'
    df_template.columns.name = 'Depth_m_idx'
    df_template = df_template.reset_index().melt(id_vars='Datetime')
    df_template = df_template.set_index(['Datetime', 'Depth_m_idx'])
    return df_template


# def scale_and_center_data(x_data):

def get_good_start_dates(df, chunk_length=5):
    df = df.reset_index()
    dates = pd.date_range(start=df.Datetime.min(), end=df.Datetime.max(),
                          freq='10D')
    good_10d_start_dates = []
    for i, d in enumerate(dates):
        # take every other start of each 10-day period starting with the first
        if not i % 2:
            good_10d_start_dates.append(d)

    # get half-way through the 10-day period so we can have 5-day periods
    
    good_start_dates = []
    for d in good_10d_start_dates:
        good_start_dates.append(d)
        good_start_dates.append(d + datetime.timedelta(days=chunk_length))

    return good_start_dates



def chunk_depth_data(df_list):
    """
    takes a list of dataframes with indices of 'Depth_m_idx' and 'Datetime' - 
    one data frame for every training period. For each time period, and each
    depth, it takes the values and splits them into sequences of 30 time periods
    each. Then it stitches all of those up together.

    The end product is one array with dims [n_batch, seq_len, n_var]
    """
    seq_len = 30 # (5 days)
    l = []
    for df in df_list:
        gp = df.groupby('Depth_m_idx')
        for depth, depth_df in gp:
            if depth_df.shape[0] < 30:
                continue
            depth_df = depth_df.T
            depth_batch = split_into_batches(depth_df.values, seq_len)
            depth_batch = np.moveaxis(depth_batch, 2, 1)
            l.append(depth_batch)
    return np.vstack(l)


def get_data_for_dates(df, start_dates, num_days=5):
    data_list = []
    df = df.reset_index('Depth_m_idx')
    for d in start_dates:
        end_date = d + datetime.timedelta(hours=120)
        df_dates = df.groupby('Depth_m_idx').apply(lambda gdf: gdf.loc[d: end_date])
        del df_dates['Depth_m_idx']
        data_list.append(df_dates)
    return data_list


def chunk_idx(df_list):
    dfs_w_idx_vals = []
    for df in df_list:
        dfc = df.copy()
        dfc = dfc.reset_index()
        dfc['Datetime_vals'] = dfc['Datetime']
        dfc['Depth_m_idx_vals'] = dfc['Depth_m_idx']
        dfc = dfc.set_index(['Datetime', 'Depth_m_idx'])
        dfs_w_idx_vals.append(dfc[['Datetime_vals', 'Depth_m_idx_vals']])
    return chunk_depth_data(dfs_w_idx_vals)


def preprocess_data_rnn(data_file, x_vars, val_size, outfile=None):
    """
    Parameters
    ----------
    data_file : str 
        path to data file
    x_vars : list or tuple
        the variable names you want to use as predictors
    val_size: float
        the portion (as a decimal) of the data that you want in the validation
        partition
        
    Returns
    -------
    """

    # read in data
    df_all = pd.read_csv(data_file, parse_dates=['Datetime'])

    df_template = create_df_template(df_all)

    df_all = df_all.set_index(['Datetime', 'Depth_m_idx'])

    df_full_shape = df_template.join(df_all)

    # subset to x_vars
    x_data = df_full_shape[x_vars]

    # fill gaps
    x_data = x_data.fillna(-9999)
    x_data = x_data.groupby('Depth_m_idx').apply(fill_x_data_gaps)

    # check to make sure there are no NaN
    assert x_data.isna().sum().sum() == 0

    # scale and center
    x_std = x_data.std()
    x_mean = x_data.mean()
    x_scl = (x_data - x_data.mean())/x_data.std()

    # break up into sequences
    num_days = 5
    start_dates = get_good_start_dates(x_scl, chunk_length=num_days)
    # separate into train/val start_dates
    if val_size == 0:
        train_start_dates = start_dates
        val_start_dates = None
    elif val_size == 1:
        train_start_dates = None
        val_start_dates = start_dates
    else:
        train_start_dates, val_start_dates = train_test_split(start_dates,
                                                              test_size=val_size)
    # get data for train/val dates
    if train_start_dates:
        x_trn = get_data_for_dates(x_scl, train_start_dates, num_days=num_days)
        x_trn_values = chunk_depth_data(x_trn) 
        trn_idx = chunk_idx(x_trn)
        y_trn = get_data_for_dates(df_full_shape[['DO_mg.L']], train_start_dates,
                                   num_days=num_days)
        y_trn_values = chunk_depth_data(y_trn)
        trn_ids = trn_idx[:, :, [1]],
        trn_times = trn_idx[:, :, [0]],
    else:
        x_trn, x_trn_values, trn_idx, y_trn, y_trn_values, trn_ids, trn_times = None, None, None, None, None, None, None

    if val_start_dates:
        x_val = get_data_for_dates(x_scl, val_start_dates, num_days=num_days)
        x_val_values = chunk_depth_data(x_val) 
        val_idx = chunk_idx(x_val)
        try:
            y_val = get_data_for_dates(df_full_shape[['DO_mg.L']], val_start_dates,
                                       num_days=num_days)
            y_val_values = chunk_depth_data(y_val) 
        except KeyError:
            y_val = None
            y_val_values = None

        val_ids = val_idx[:, :, [1]],
        val_times = val_idx[:, :, [0]],
    else:
        x_val, x_val_values, val_idx, y_val, y_val_values, val_ids, val_times = None, None, None, None, None, None, None


    data = {
            'x_trn': x_trn_values,
            'x_val': x_val_values,
            'y_trn': y_trn_values,
            'y_val': y_val_values,
            'start_times_trn': train_start_dates,
            'start_times_val': val_start_dates,
            'times_trn': trn_times,
            'ids_trn': trn_ids,
            'times_val': val_times,
            'ids_val': val_ids,
            'x_vars': x_vars,
            'x_std': x_std,
            'x_mean': x_mean,
            }

    if outfile:
        np.savez_compressed(outfile, **data)
    return data


def preprocess_data_ann(data_file, x_vars, outfile=None):
    """
    Parameters
    ----------
    data_file : str 
        path to data file
    x_vars : list or tuple
        the variable names you want to use as predictors
        
    Returns
    -------
    """

    # read in data
    df_all = pd.read_csv(data_file, parse_dates=['Datetime', 'time_of_day'])
    df_all['time_of_day'] = df_all.time_of_day.dt.hour * 3600 +\
            df_all.time_of_day.dt.minute * 60 + df_all.time_of_day.dt.second

    df_all = df_all.set_index(['Datetime', 'Depth_m_idx'])

    # subset to x_vars
    x_data = df_full_shape[x_vars]
