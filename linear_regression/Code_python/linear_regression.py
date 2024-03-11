import os
import pandas as pd
import numpy as np
import statsmodels.api as sm




class LinearRegression():
    def __init__(self, x, y, var_name_list, constant = True):
        ###x, y must be a ndarray
        self.x = x
        self.y = y
        self.var_name = var_name_list
        self.constant = constant

        self.std_err = 0        # std err for each estimators
        self.beta_t_value = 0   # t value for each estimators


    def add_constant(self):
        const_matrix = np.ones(len(self.x)).reshape(len(self.x), 1)
        # self.x_ : matrix X with constant column used for regression
        self.x = np.hstack((const_matrix, self.x))
        self.var_name = ['const'] + self.var_name

    def get_statistics(self):
        self.y_hat = self.x.dot(self.beta)
        self.ee = np.sum((self.y - self.y_hat)**2)
        # s2 = sigma2_hat = ee/(n-k), here sigma2_hat is the estimate of sigma2, since we cannot observe sigma2
        #### Var(e) = sigma2 = ee/
        self.sigma2_hat = self.ee/(self.x.shape[0] - self.x.shape[1])       
        variance = self.sigma2_hat * np.linalg.inv((self.x.T).dot(self.x))
        self.std_err_matrix = np.sqrt(variance)
        # it is the values on the diagonal. 
        self.std_err = np.diag(self.std_err_matrix)
        self.beta_t_value = self.beta/self.std_err
        
        #print(self.std_err_matrix)
        #print(self.std_err)
        #print(self.beta_t_value)


    def OLS(self):
        if self.constant:
            self.add_constant()

        self.beta = ((np.linalg.inv((self.x.T).dot(self.x))).dot(self.x.T)).dot(self.y)
        self.get_statistics()


        print('-'*60)
        print(('{:>15} '*4).format('X', 'coef', 'std_err', 't_value'))
        for var, estimator, std_err, t_value in zip(self.var_name, np.round_(self.beta, 5), np.round_(self.std_err, 5), np.round_(self.beta_t_value, 5)):
            print(('{:>15} '*4).format(var, estimator, std_err, t_value))








if __name__ == '__main__':


    df = pd.read_csv('~/my_disk/git/data/sample_dataset.csv')
    x = df.iloc[:, 3:5].values
    y = df.iloc[:, 0].values
    var_name_list = ['x1', 'x2']
    LinearRegression(x, y, var_name_list).OLS()
    #x = sm.add_constant(x)
    #model = sm.OLS(y, x)
    #model = model.fit()
    #print(model.summary())
    #[5.47080711 1.04023284 2.99146809]
    #0.019
    #0.019
    #0.019
