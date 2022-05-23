import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt




def standardization(x):
    sample_mean = np.mean(x)
    sample_sd = np.std(x)
    return [(i-sample_mean)/sample_sd for i in x]


df = pd.read_csv('~/my_disk/git/data/sample_dataset.csv')
print(df)



x_raw = np.log(df['x3'].values)
mean_raw = np.mean(x_raw)
sd_raw = np.std(x_raw)

x_standardized = standardization(x_raw)
mean_std = np.mean(x_standardized)
sd_std = np.std(x_standardized)



print('raw mean: {:<15}   raw sd: {:<15}'.format(mean_raw, sd_raw))
print('raw mean: {:<15}   raw sd: {:<15}'.format(mean_std, sd_std))


plt.hist(x_raw)
plt.savefig('raw.png')
plt.clf()

plt.hist(x_standardized)
plt.savefig('standardization.png')
plt.clf()






