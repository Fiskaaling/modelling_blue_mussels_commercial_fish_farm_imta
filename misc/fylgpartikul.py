from bisect import bisect_right

import numpy as np
from pandas import NA

def fylgpartikul(date, ymisk_dypir, df, logga, H, dypid, listdep, speed, step):
    """
    koda til at fylgja einum partikli
    :param out:         strukturin til at returna hettar burdi ikki verði her men ja
    :param Ens_start:   hjálp til streym2 kodina fyri at skjótari finna hvar í dataiðinum man skal leita
    :param date:        nar verður partikulin slopin
    :return:            out har hann er útfyltur
    """

    start_index = 0
    u = [NA for _ in range(ymisk_dypir)]
    v = [NA for _ in range(ymisk_dypir)]

    #  Ein vector sum sigur hvar partikulin er og hvar hann var sídsta tíðsskirt
    Xold = np.array([0., 0., H])
    X = Xold.copy()
    nr = 0
    test = logga[nr]
    stop_loop_0 = 0
    #temp_df = df.loc[0:1,:].copy().reset_index(drop=True)
    temp_df = df.loc[0:1,:].copy().reset_index(drop=True).to_numpy()
    while X[2] > dypid and not stop_loop_0==1:

        while X[2] <= test:
            u[nr] = X[0]
            v[nr] = X[1]
            nr += 1
            if nr == ymisk_dypir:
                return u, v
            test = logga[nr]
        Xold = X.copy()
        #print('her',Xold[0],Xold[1],)
        streym, temp_df = streymur(date=date, depth=X[2], start_index=start_index,
                            listdep=listdep, df=df, temp_df=temp_df)
        
        X += (streym + np.array([0., 0., speed])) * step * 86400 # umrokna til sek
        date += step
    return u, v

def streymur2(date, depth, start_index, listdep, df, temp_df):
    """
    gevur eina interperlatión av hvussu streymurin er í givna dypið og givnu tíð uttan z rætning

    :param date:    mdate av nar vit eru intriserðaði í datanum
    :param depth:   dýpið av hvar vit eru intriserðaði í dýpiðinum
    :param Ens:     hvat fyri index í datasettinum vit higgja fyrst eftri vit higgja bara eftri tí
                        givna og tað ið kemur eftri
    :param listdep  listin yvir hvussu djúpar allar bininar eru.
    :return:        (np.array([streymur Estur, Sreymur Norð, Streymur upp]), hvat Ens eg skal brúka nastuferð)
    """

    if date >= temp_df.loc[:, 'index'].iloc[1]:
        temp_df = df.loc[start_index:start_index + 1,:].copy().reset_index(drop=True)
        start_index += 1

    #  Finn s og t fyri Ens (s * udf['date'][Ens] + t * udf['date'][Ens+1] = date) (s+t = 1)
    t = (date - temp_df.loc[0, 'index']) / (temp_df.loc[1, 'index'] - temp_df.loc[0, 'index'])
    T = 1-t
    #  Finn dýpið
    #  Rokna eina interpalatión
    if listdep[0] >= depth:
        u_ut = T * temp_df.loc[0, 'u1'] + t * temp_df.loc[1, 'u1']
        v_ut = T * temp_df.loc[0, 'v1'] + t * temp_df.loc[1, 'v1']
    elif listdep[-1] <= depth:
        l = str(len(listdep))
        u_ut = T * temp_df.loc[0, f'u{l}'] + t * temp_df.loc[1, f'u{l}']
        v_ut = T * temp_df.loc[0, f'v{l}'] + t * temp_df.loc[1, f'v{l}']
    else:
        dybdin1 = bisect_right(listdep, depth)
        d0, d1 = str(dybdin1), str(dybdin1 + 1)
        t2 = (depth - listdep[dybdin1 - 1]) / (listdep[dybdin1] - listdep[dybdin1 - 1])
        T2 = 1-t2
        u_ut = T * (T2 * temp_df.loc[0, 'u' + d0] + t2 * temp_df.loc[0, 'u' + d1]) + \
               t * (T2 * temp_df.loc[1, 'u' + d0] + t2 * temp_df.loc[1, 'u' + d1])
        v_ut = T * (T2 * temp_df.loc[0, 'v' + d0] + t2 * temp_df.loc[0, 'v' + d1]) + \
               t * (T2 * temp_df.loc[1, 'v' + d0] + t2 * temp_df.loc[1, 'v' + d1])

    return np.array([u_ut, v_ut, 0]), temp_df

def streymur(date, depth, start_index, listdep, df, temp_df):
    """
    gevur eina interperlatión av hvussu streymurin er í givna dypið og givnu tíð uttan z rætning

    :param date:    mdate av nar vit eru intriserðaði í datanum
    :param depth:   dýpið av hvar vit eru intriserðaði í dýpiðinum
    :param Ens:     hvat fyri index í datasettinum vit higgja fyrst eftri vit higgja bara eftri tí
                        givna og tað ið kemur eftri
    :param listdep  listin yvir hvussu djúpar allar bininar eru.
    :return:        (np.array([streymur Estur, Sreymur Norð, Streymur upp]), hvat Ens eg skal brúka nastuferð)
    """

    if date >= temp_df[1, 0]:
        temp_df = df.loc[start_index:start_index + 1,:].copy().reset_index(drop=True).to_numpy()
        start_index += 1

    #  Finn s og t fyri Ens (s * udf['date'][Ens] + t * udf['date'][Ens+1] = date) (s+t = 1)
    t = (date - temp_df[0, 0]) / (temp_df[1, 0] - temp_df[0, 0])
    T = 1-t
    #  Finn dýpið
    #  Rokna eina interpalatión
    if listdep[0] >= depth:
        u_ut = T * temp_df[0, 1] + t * temp_df[1, 1]
        v_ut = T * temp_df[0, 2] + t * temp_df[1, 2]
    elif listdep[-1] <= depth:
        u_ut = T * temp_df[0, -2] + t * temp_df[1, -2]
        v_ut = T * temp_df[0, -1] + t * temp_df[1, -1]
    else:
        d = bisect_right(listdep, depth)
        t2 = (depth - listdep[d - 1]) / (listdep[d] - listdep[d - 1])
        T2 = 1-t2
        u_ut = T * (T2 * temp_df[0, 2*d-1] + t2 * temp_df[0, 2*d+1]) + \
               t * (T2 * temp_df[1, 2*d-1] + t2 * temp_df[1, 2*d+1])
        v_ut = T * (T2 * temp_df[0, 2*d  ] + t2 * temp_df[0, 2*d+2]) + \
               t * (T2 * temp_df[1, 2*d  ] + t2 * temp_df[1, 2*d+2])

    return np.array([u_ut, v_ut, 0]), temp_df
