import os
os.environ['R_HOME'] = "C:\Program Files\R\R-4.0.2"
from pathlib import Path
import yaml
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
import pandas as pd
from scipy.stats import wilcoxon
from statsmodels.stats.weightstats import DescrStatsW
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import pickle
import numpy as np
import rpy2.robjects as ro
import rpy2.robjects.numpy2ri as rpyn
from rpy2.robjects.packages import importr
from rpy2.robjects import conversion, default_converter


ROOT_dir = Path(__file__).parent.parent
with open(os.path.join(ROOT_dir, 'dbs', 'keys.yaml')) as f:
    keys_manager = yaml.load(f, Loader=yaml.FullLoader)


def weighted_corr(data, var1, var2, weight_col='wt_p'):
    with conversion.localconverter(default_converter):
        r_weights = importr('weights')
    with conversion.localconverter(default_converter):
        ro.r.assign('x', rpyn.numpy2rpy(data[var1].values))
        ro.r.assign('y', rpyn.numpy2rpy(data[var2].values))
        ro.r.assign('weights', rpyn.numpy2rpy(data[weight_col].values))
        ro.r('''results <- wtd.cor(x, y, weight=weights, bootp=TRUE, bootse=TRUE, bootn=1000)''')
        ro.r('''print(results)''')


def weighted_wilcoxon(data, value_col, weight_col, baseline=0, m='two-sided', n_bootstrap=1000):
    """
    Perform a weighted Mann-Whitney U test using bootstrapping.

    Parameters:
    - data: pandas DataFrame containing the data
    - value_col: str, name of the target value column
    - weight_col: str, name of the weight column
    - n_bootstrap: int, number of bootstrap samples to draw

    Returns:
    - mean_p_value: float, mean p-value from bootstrap samples
    - p_values: list of float, p-values from each bootstrap sample
    """
    v = data[value_col] - baseline
    values = v.values
    weights = data[weight_col].values

    # Normalize weights
    weights = weights / np.sum(weights)

    bootstrap_p_values = []
    bootstrap_stats_values = []
    for _ in range(n_bootstrap):
        # Resample according to weights
        resample_indices = np.random.choice(len(values), size=len(values), replace=True, p=weights)
        resampled_values = values[resample_indices]

        # Perform Mann-Whitney U test against zero
        res = wilcoxon(resampled_values, nan_policy='omit', alternative=m)
        bootstrap_p_values.append(res.pvalue)
        bootstrap_stats_values.append(res.statistic)

    # Calculate the mean p-value
    mean_p_value, std_p_value = np.mean(bootstrap_p_values), np.std(bootstrap_p_values)
    mean_stats_value, std_stats_value = np.mean(bootstrap_stats_values), np.std(bootstrap_stats_values)
    return mean_p_value, std_p_value, mean_stats_value, std_stats_value


def bootstrap_median_and_error(df, target_col, weight_col, n_bootstrap=1000):
    """
    Calculate the bootstrap median and error of a dataframe with target value and weight columns.

    Parameters:
    - df: pandas DataFrame containing the data
    - target_col: str, name of the target value column
    - weight_col: str, name of the weight column
    - n_bootstrap: int, number of bootstrap samples to draw

    Returns:
    - median: float, estimated median of the target values
    - median_error: float, standard error of the bootstrap medians
    """
    target_values = df[target_col].values
    wts = df[weight_col].values

    bootstrap_medians = []

    for _ in range(n_bootstrap):
        # Resample with replacement
        resample_indices = np.random.choice(len(target_values), size=len(target_values), replace=True)
        resampled_values = target_values[resample_indices]
        resampled_weights = wts[resample_indices]

        # Calculate weighted median for resample
        wdf = DescrStatsW(resampled_values, weights=resampled_weights, ddof=1)
        sts = wdf.quantile([0.50])
        median = sts.values[0]
        bootstrap_medians.append(median)

    # Calculate the median and standard error of the bootstrap medians
    median_estimate = np.median(bootstrap_medians)
    median_error = np.std(bootstrap_medians)

    return median_estimate, median_error


def list2chunks(target_list=None, chunk_num=None):
    chunks = []
    chunk_size = int(len(target_list) / chunk_num) + (len(target_list) // chunk_num > 0)
    for i in range(chunk_num):
        chunk = target_list[i * chunk_size:(i + 1) * chunk_size]
        if len(chunk) > 0:
            chunks.append(chunk)
    return chunks