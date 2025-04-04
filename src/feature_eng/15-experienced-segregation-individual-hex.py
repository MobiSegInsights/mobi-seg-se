import sys
from pathlib import Path
import os
os.environ['USE_PYGEOS'] = '0'
import geopandas as gpd
import pandas as pd
import sqlalchemy
import numpy as np
import ast
import random
from tqdm import tqdm
from p_tqdm import p_map


ROOT_dir = Path(__file__).parent.parent.parent
sys.path.append(ROOT_dir)
sys.path.insert(0, os.path.join(ROOT_dir, 'lib'))

import preprocess as preprocess


def sim_expand(x):
    sim_list = []
    if ':' in x:
        sim_dict = ast.literal_eval(x)
        for k, v in sim_dict.items():
            sim_list += [k] * v
        random.Random(4).shuffle(sim_list)
    else:
        sim_list += [x] * 100
    return sim_list


class MobiSegAggregationIndividual:
    def __init__(self):
        self.mobi_data = None
        self.zonal_seg = None
        self.deso_sim_matrix = None
        self.zones = None
        self.user = preprocess.keys_manager['database']['user']
        self.password = preprocess.keys_manager['database']['password']
        self.port = preprocess.keys_manager['database']['port']
        self.db_name = preprocess.keys_manager['database']['name']

    def load_individual_data(self, grp_num=30, test=False):
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}?gssencmode=disable')
        print('Load mobility data and add hexagons.')
        self.zones = gpd.GeoDataFrame.from_postgis(sql="""SELECT deso, hex_id, geom FROM spatial_units;""",
                                                   con=engine)
        if test:
            self.mobi_data = pd.read_sql(sql='''SELECT uid, lat, lng, holiday, weekday, wt_total, deso, time_span
                                                FROM segregation.mobi_seg_deso_raw
                                                LIMIT 1000000;''', con=engine)
        else:
            self.mobi_data = pd.read_sql(sql='''SELECT uid, lat, lng, holiday, weekday, wt_total, deso, time_span
                                                FROM segregation.mobi_seg_deso_raw;''', con=engine)
        columns2keep = ['uid', 'holiday', 'weekday', 'wt_total', 'time_span', 'deso']
        deso_list = self.zones.loc[self.zones['hex_id'] == '0', 'deso'].values
        hex_deso_list = self.zones.loc[self.zones['hex_id'] != '0', 'deso'].unique()
        geo_deso = self.mobi_data.loc[self.mobi_data['deso'].isin(deso_list), columns2keep]
        geo_hex = self.mobi_data.loc[self.mobi_data['deso'].isin(hex_deso_list), :]

        geo_deso.loc[:, 'hex'] = geo_deso.loc[:, 'deso']

        def find_hex(data):
            # uid, lat, lng, wt_total, dur, hex_id*
            deso = data.deso.values[0]
            d = data.copy()
            gdf_d = preprocess.df2gdf_point(d, x_field='lng', y_field='lat', crs=4326, drop=False)
            gdf_d = gpd.sjoin(gdf_d,
                              self.zones.loc[self.zones['deso'] == deso, :].drop(columns=['deso']),
                              how='inner')
            return gdf_d[['uid', 'holiday', 'weekday', 'wt_total', 'time_span', 'hex_id']].\
                rename(columns={'hex_id': 'hex'})

        print("Find hexagons for geolocations by DeSO zone.")
        tqdm.pandas()
        geo4graph = geo_hex.groupby('deso').progress_apply(find_hex).reset_index()
        if test:
            self.mobi_data = geo4graph
        else:
            self.mobi_data = pd.concat([geo4graph, geo_deso])

        # Group users
        random.seed(1)
        uids = self.mobi_data.uid.unique()
        gps = np.random.randint(1, grp_num + 1, len(uids))
        gp_dict = dict(zip(uids, gps))
        self.mobi_data.loc[:, 'gp'] = self.mobi_data['uid'].apply(lambda x: gp_dict[x])

        if test:
            print('Test mode, only look at 10000 users.')
            self.mobi_data = self.mobi_data.loc[self.mobi_data['gp'] == 1, :]
        print(self.mobi_data.iloc[0])
        if not test:
            print('Save mobility data at mixed-hexagon level.')
            self.mobi_data.to_sql('mobi_seg_hex_raw', engine, schema='segregation', index=False,
                      method='multi', if_exists='append', chunksize=10000)

    def load_saved_individual_data(self, test=False):
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}?gssencmode=disable')
        if test:
            self.mobi_data = pd.read_sql(sql='''SELECT * FROM segregation.mobi_seg_hex_raw 
                                                LIMIT 100000;''', con=engine)
        else:
            self.mobi_data = pd.read_sql(sql='''SELECT * FROM segregation.mobi_seg_hex_raw;''', con=engine)

    def load_zonal_data(self):
        print('Load segregation metrics at mixed-hexagon zones...')
        engine = sqlalchemy.create_engine(
            f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}?gssencmode=disable')
        zonal_seg_ = pd.read_sql(sql='''SELECT hex, time_seq, ice_birth, weekday, holiday
                                            FROM segregation.mobi_seg_hex;''', con=engine)
        self.zonal_seg = dict()
        lbs = [['w0h0', 'w0h1'], ['w1h0', 'w1h1']]
        for weekday in (0, 1):
            for holiday in (0, 1):
                temp_ = zonal_seg_.loc[(zonal_seg_.weekday == weekday) & (zonal_seg_.holiday == holiday)].\
                    drop(columns=['weekday', 'holiday'])
                self.zonal_seg[lbs[weekday][holiday]] = temp_.pivot(index='time_seq', columns='hex', values='ice_birth')

    def aggregating_metrics_indi(self):
        def time_seq_median(data):
            return pd.Series({"time_seq": data.time_seq.values[0],
                              'ice_birth': data.ice_birth.median()})

        def by_time(data):
            return data.groupby(['weekday', 'holiday', 'uid']).apply(time_seq_median).reset_index()

        def span2seq(time_seq_list):
            seq = list(range(time_seq_list[0], time_seq_list[1] + 1))
            if len(time_seq_list) > 2:
                seq2 = list(range(time_seq_list[2], time_seq_list[3] + 1))
                seq = seq2 + seq
            return seq

        for gp_id, df in self.mobi_data.groupby('gp'):
            print(f'Processing group: {gp_id}.')
            print(f'Exploding on time sequence.')
            df.loc[:, 'time_span'] = df.loc[:, 'time_span'].apply(lambda x: ast.literal_eval(
                x.replace("{", "(").replace("}", ")")
            ))
            df.loc[:, 'time_seq'] = df.loc[:, 'time_span'].apply(span2seq)
            df = df.explode('time_seq')
            df.drop(columns=['time_span'], inplace=True)

            print(f'Merge experienced segregation level.')

            def hex_time_to_seg(row):
                try:
                    x = self.zonal_seg[f"w{int(row['weekday'])}h{int(row['holiday'])}"].\
                        loc[(row['time_seq'], row['hex'])]
                except:
                    x = 9
                return x
            tqdm.pandas()
            df.loc[:, 'ice_birth'] = df.progress_apply(lambda row: hex_time_to_seg(row), axis=1)
            share = len(df.loc[df.ice_birth == 9, :]) / len(df) * 100
            print(f"Share of missing values: {share} %.")
            df = df.loc[df.ice_birth != 9, :]
            print(f'Calculate an average day individually by weekday and holiday.')
            rstl = p_map(by_time, [g for _, g in df.groupby('time_seq', group_keys=True)])
            df = pd.concat(rstl)

            engine = sqlalchemy.create_engine(
                f'postgresql://{self.user}:{self.password}@localhost:{self.port}/{self.db_name}?gssencmode=disable')
            df.to_sql('mobi_seg_hex_individual', engine, schema='segregation', index=False,
                      method='multi', if_exists='append', chunksize=10000)


if __name__ == '__main__':
    # Aggregating metrics individually (experienced segregation)
    seg_indi = MobiSegAggregationIndividual()
    # seg_indi.load_individual_data(grp_num=12, test=False)
    seg_indi.load_saved_individual_data(test=False)
    seg_indi.load_zonal_data()
    seg_indi.aggregating_metrics_indi()
