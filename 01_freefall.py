import multiprocessing as mp
from pathlib import Path
from misc.fylgpartikul import fylgpartikul

import pandas as pd
import matplotlib.dates as mdate
import numpy as np

SEK_PER_DAG = 24*60*60
PRINT_PER = 100

##################### Setup ####################################################
Path_to_Data      = Path('SORD1702/std_ascii') #current measurement folder
sigma             = 1
H                 = 5  # starthaddin í [m] 0 er miðal haddin á sjónum, upp er posetift
step              = 0.01 / SEK_PER_DAG
ferdir            = [0.075, 0.032, 0.015, 0.001]  # m/s
logga_pr_dypi     = 0.01 # m
logga             = np.arange(-5-logga_pr_dypi, -29-logga_pr_dypi, -logga_pr_dypi)
tid_immillum_kast = 60*2 / SEK_PER_DAG
kast_per_tid      = 1
antal_process     = 100
#fyrstadato        = mdate.date2num(pd.Timestamp("2017-04-01"))  # None so verður hettar tiki frá data
#sistadato         = mdate.date2num(pd.Timestamp("2017-05-01"))  # None so verður hettar tiki frá data
fyrstadato        = None
sistadato         = None

ferdir = [-x for x in ferdir]
ymisk_dypir = len(logga)
H = -H
inskriving = []


##################### Inles Data ###############################################
uvdata = pd.read_csv(Path_to_Data / 'uvdata.csv', index_col='dateTime', parse_dates=True)
qualidata = pd.read_csv(Path_to_Data / 'quali.csv', index_col='dateTime', parse_dates=True)
uvdata = uvdata.loc[(qualidata.min(1) < 3),:]
meta = pd.read_csv(Path_to_Data / 'meta.csv', index_col='key')['value']
del qualidata
my_bins = [int(x[3:])
           for x, y in zip(meta.index, meta.index.str.contains('^bin\d+$'))
           if y and float(meta[x]) < float(meta['median_dypid']) - 5]
uvdata = uvdata.loc[:, [f'{ratning}{binnr}' for binnr in my_bins for ratning in list('uv')]]

uvdata.index = mdate.date2num(uvdata.index)
uvdata.interpolate(inplace=True)

#################### Samla metadata ############################################

last_bin = max(my_bins)

if not sistadato:
    sistadato = mdate.date2num(pd.Timestamp(meta['tikin_upp'])) - 1
if not fyrstadato:
    fyrstadato = mdate.date2num(pd.Timestamp(meta['settur_ut'])) + 1

dypid = -float(meta['median_dypid'])
listdep = [dypid + float(meta[f'bin{x}']) for x in my_bins]



################## Funktiónirnar ###############################################

def init_pool(l):
    global lock
    lock = l

def fylgpartikulcall(indput):

    col = indput[5]
    filename = indput[6]

    starttid = indput[1]
    out = {'starttid' : starttid}

    if indput[2] % PRINT_PER == 0:
        print(indput[2], indput[3], indput[0])

    startindex = uvdata.loc[starttid - 0.1: starttid].index[-1]
    df = uvdata.loc[startindex:starttid + indput[4] + 0.1].copy().reset_index()

    u, v = fylgpartikul(date=indput[1], ymisk_dypir=indput[3], df=df,
                       logga=logga, H=H, dypid=dypid, listdep=listdep,
                       speed=indput[0], step=step)
    
    for nr in range(ymisk_dypir):
        out[str(nr) + 'u'] = u[nr]
        out[str(nr) + 'v'] = v[nr]

    line = ','.join([f'{out[x]}' for x in col])
    with lock:
        with open(filename, 'a') as f:
            f.write(line)
            f.write('\n')

def ger_fil(outofpool, speed, col):
    df = pd.DataFrame(outofpool).loc[:, col]
    df.to_csv(f'out_{-speed}.csv', index=False)



################################################################################

if __name__ == '__main__':
    with open('logga.txt', 'w') as f:
        for index, dypid_col in enumerate(logga):
            f.write(f'{index}, {dypid_col}\n')
    for speed in ferdir:
        filename = f'out_{-speed}.csv'
        time_to_reach_the_bottom = dypid/speed

        col = ['starttid']
        col.extend(
            [f'{i}{ratning}'
             for ratning in list('uv')
             for i in range(ymisk_dypir)
            ]
        )

        with open(filename, 'w') as f:
            f.write(','.join(col))
            f.write('\n')
        lock = mp.Lock()
        #  Bygg elementini sum skullu inn í pool
        indtopool = np.array([x
                              for x in np.arange(fyrstadato, sistadato, tid_immillum_kast)
                              for _ in range(kast_per_tid)])

        indtopool = [
            (speed,        #  hvussu skjótt partikulin fellur
             x,            #  nær partikkulin verður sleptur
             i,            #  Hvat nummar partikkul hann er
             ymisk_dypir,  #  Hvat fyri dypir skullu noterast
             time_to_reach_the_bottom,
             col,
             filename
            )
            for i, x in enumerate(indtopool)]

        print(len(indtopool))
        with mp.Pool(processes=antal_process, initializer=init_pool, initargs=(lock,)) as pool:
            pool.map(fylgpartikulcall, indtopool)

        #inskriving.append(mp.Process(target=ger_fil, args=(out_of_pool, speed, col)))
        #inskriving[-1].start()

for x in inskriving:
    x.join()
