import os
import pandas as pd
import sys
from pathlib import Path
import sqlalchemy
from tqdm import tqdm


ROOT_dir = Path(__file__).parent.parent
sys.path.append(ROOT_dir)
sys.path.insert(0, os.path.join(ROOT_dir, '/lib'))

from lib import preprocess as preprocess


class ActivityPatterns:
    def __init__(self):
        """
        :param month: 06, 07, 08, 12
        :type month: str
        :return: None
        :rtype: None
        """
        self.user = preprocess.keys_manager['database']['user']
        self.password = preprocess.keys_manager['database']['password']
        self.port = preprocess.keys_manager['database']['port']
        self.db_name = preprocess.keys_manager['database']['name']
        self.data = None
        self.clusters = None

    def load_process_data(self):
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}')
        self.data = pd.read_sql_query(
            sql="""SELECT uid, seq, loc, "localtime", leaving_localtime, h_s,
             dur, holiday_s, weekday_s, weekday_e FROM stops_p WHERE dur < 720;""", con=engine)
        df_uids = pd.read_sql_query(
            sql="""SELECT uid, num_loc FROM description.stops_p WHERE num_days > 7;""", con=engine)
        df_uids = df_uids.loc[df_uids.num_loc > 2, :]
        self.data = self.data.loc[self.data.uid.isin(df_uids.uid), :]
        num_uids, num_stays = self.data['uid'].nunique(), len(self.data)
        print(f"Apply {num_stays} stays from {num_uids} devices.")

    def aggregate_activity_temporal(self):
        # All records of stays
        all = list(self.data.loc[:, ['h_s', 'dur']].to_records(index=False))
        df_all = preprocess.cluster_tempo(pur='all', temps=all,
                                          interval=30, maximum_days=2, norm=False)
        # Holiday
        holidays = list(self.data.loc[self.data.holiday_s == 1, ['h_s', 'dur']].to_records(index=False))
        df_holidays = preprocess.cluster_tempo(pur='holiday', temps=holidays,
                                               interval=30, maximum_days=2, norm=False)
        # Non-holiday
        non_holidays = list(self.data.loc[self.data.holiday_s == 0, ['h_s', 'dur']].to_records(index=False))
        df_nholidays = preprocess.cluster_tempo(pur='non_holiday', temps=non_holidays,
                                                interval=30, maximum_days=2, norm=False)
        return pd.concat([df_all, df_holidays, df_nholidays])

    def add_weight2records(self):
        tqdm.pandas()
        self.data = self.data.groupby('uid').progress_apply(preprocess.record_weights).reset_index(drop=True)

    def cluster_stats(self, top_n=3):
        def cluster_attrs(data):
            freq = len(data)
            freq_wt = sum(data.wt)
            dur = data.dur.sum()
            return pd.Series(dict(freq=freq, freq_wt=freq_wt, dur=dur))

        # Computing the statistics of clusters
        print('Computing statistics of clusters (by loc)...')
        tqdm.pandas()
        self.clusters = self.data.groupby(['uid', 'loc', 'holiday_s']).progress_apply(cluster_attrs).reset_index()
        self.clusters = self.clusters.sort_values(by=['uid', 'holiday_s', 'freq_wt'], ascending=[True, True, False])
        print('Saving clusters statistics...')
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}')
        self.clusters.to_sql('clusters_p', engine, schema='description', index=False, if_exists='replace',
                             method='multi', chunksize=5000)

        print(f'Selecting top {top_n} clusters individually...')
        tqdm.pandas()
        # Weighted top clusters
        df_top = self.clusters.loc[self.clusters.holiday_s == 0, :].groupby('uid').head(top_n).reset_index(drop=True)
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}')
        df_top.to_sql(f'clusters_top{top_n}_wt_p', engine, schema='description', index=False, if_exists='replace',
                      method='multi', chunksize=5000)

    def activities_temporal(self, df_top=None):
        # Calculate temporal patterns of each cluster
        def cluster_tempo_agg(data):
            recs = list(data[['h_s', 'dur', 'wt']].to_records(index=False))
            df_tp = preprocess.cluster_tempo_weighted(temps=recs, interval=30, maximum_days=2)
            df_tp.loc[:, 'uid'] = data['uid'].values[0]
            df_tp.loc[:, 'loc'] = data['loc'].values[0]
            return df_tp

        print('Get records only for the input clusters of individuals...')
        self.data = pd.merge(self.data, df_top[['uid', 'loc']], on=['uid', 'loc'], how='inner')
        print('Extracting activity temporal profiles for top 3...')
        tqdm.pandas()
        df_tempo = self.data.groupby(['uid', 'loc']).progress_apply(cluster_tempo_agg).reset_index(drop=True)
        df_tempo_list = preprocess.df2batches(df_tempo, chunk_size=10000000)
        del df_tempo
        for df in tqdm(df_tempo_list, desc='Saving temporal profiles'):
            engine = sqlalchemy.create_engine(
                f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}')
            df.to_sql('tempo_top3_p', engine, schema='description', index=False, if_exists='append',
                      method='multi', chunksize=5000)
