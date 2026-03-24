import pandas as pd
import numpy as np
import json, requests, re, os, math, csv
from datetime import datetime, timezone
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt



#



def get_marginal_effect(reg_result, change_X = None):
    '''
    Compute marginal effect at mean.
    reg_result: regression result, i.e., logit().fit()
    change_X: a dictionary contains the name of the variable and its value.
              The purpose is to compute the marginal effect of other variables when variable "change_X" is changed.
              change_X = {'var_name':, 'var_value':}
              If change_X is None, report the marginal effect of original dataset.
    '''
    model = reg_result.model # smf() without .fit()

    bs = reg_result.params.values.reshape(-1,1) # get estimated betas, a column vector
    V = reg_result.cov_params().values # get variance covariance matrix

    Xs = model.exog.copy() # get exogenous variables used for regression
    X_name = model.exog_names # get name of exogenous variable
    X_df = pd.DataFrame(Xs)
    X_df.columns = X_name

    X_mean = X_df.mean() # a row vector

    if change_X is not None:
        X_mean[change_X['var_name']] = change_X['var_value']
    X_mean = X_mean.values.reshape(1, -1)   # a row vector


    ###------Get estimated probability and partial effect at mean------###
    phat = 1/(1+np.exp(-X_mean@bs))[0]
    PEA = phat*(1-phat)*bs  # Get partial effect at mean.
    df_PEA = pd.DataFrame(PEA)
    df_PEA.index, df_PEA.columns = X_name, ['PEA']

    ###------Get standard error for PEA using the Delta method------###
    term1 = (phat*(1-phat)) ** 2
    I = np.identity(len(bs))
    term2 = I + (1-2*phat)*bs@X_mean
    xb = X_mean.reshape(-1,1) @ bs.reshape(1, -1)   # get xb'
    term3 = I + (1-2*phat)*xb
    var_PEA = term1 * term2 @ V @ term3
    se_PEA = np.sqrt(var_PEA)
    se_PEA = np.diag(se_PEA)
    df_PEA['std err'] = se_PEA
    
    ###------Get 95% confidence interval------###
    df_PEA['0.025'] = df_PEA['PEA'] - 1.96*df_PEA['std err']
    df_PEA['0.975'] = df_PEA['PEA'] + 1.96*df_PEA['std err']



    print(df_PEA)







###------Generate a dataset------###
np.random.seed(42)
n = 500  # No. of obs
x1 = np.random.normal(0,1,n)
x2 = np.random.normal(1,0.4,n)

# Define true b
b0, b1, b2 = 0.5, 1.2, 0.8
# get Prob(Y = 1|x) = \Lambda(x'b) in two steps:
# step 1: get latent variable y (named lin_pred)
lin_pred = b0 + b1*x1 + b2*x2
# step 2: get Prob(Y = 1|x)
p = np.exp(lin_pred)/(1+np.exp(lin_pred))

y = np.random.binomial(1, p, n)

df = pd.DataFrame({'y':y, 'x1':x1, 'x2':x2})


###------Run logit model------###
cluster_method = ['x2']
cluster_col = [f'{i}_id' for i in cluster_method]
for one_cluster in cluster_method:
    df[f'{one_cluster}_id'] = df[one_cluster].astype('category').cat.codes


#reg_result = smf.logit('y ~ x1 + x2', data = df).fit()
reg_result = smf.logit('y ~ x1 + x2', data = df).fit(cov_type = 'cluster', cov_kwds = {'groups':df[cluster_col]})
APE = reg_result.get_margeff(at = 'mean').summary()
print(reg_result.summary())
print(APE)

###------Get marginal effect------###
change_X = {
        'var_name':'x2',
        'var_value':0.5,
        }
get_marginal_effect(reg_result, change_X = change_X)






