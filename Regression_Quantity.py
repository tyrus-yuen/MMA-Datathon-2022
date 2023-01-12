import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression as LinReg
from sklearn.linear_model import PoissonRegressor
from sklearn.preprocessing import PolynomialFeatures as PolyReg

directory = '/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon'

transactions=pd.read_csv(directory+'/transactions.csv')
demographics=pd.read_csv(directory+'/demographics.csv')

products=pd.read_excel(directory+'/UoT Datathon 2022 Shared Drive/Tyrus_working files/discount by product ID.xlsx',header=2,)

#Descriptive Statistics
pd.pivot_table(products,values=[''])


#Multiple Linear Regression
list_all=[]
list_temp=[]
for i in range(products['MANUFACTURER'].nunique()):
    x=products[products['MANUFACTURER']==products['MANUFACTURER'].unique()[i]][['mean_PERC_COUPON_DISC','mean_PERC_RETAIL_DISC','count_STORE_ID']]
    y=products[products['MANUFACTURER']==products['MANUFACTURER'].unique()[i]][['sum_QUANTITY']]

    linreg = LinReg(fit_intercept=True, normalize=False, copy_X=True, n_jobs=-1)
    linreg.fit(x, y)
    print('coeff: {0}; intercept: {1}'.format(str(linreg.coef_[0,0]), str(linreg.intercept_[0])))

    linreg.score(x, y)
    from sklearn.metrics import mean_squared_error, r2_score
    prediction = linreg.predict(x)
    print("Variance score: %.6f"
          % r2_score(y.values.reshape(-1, 1), prediction))
    print("Mean squared error: %.6f"
          % mean_squared_error(y.values.reshape(-1, 1), prediction))

    list_temp=(linreg.coef_[0].tolist())
    list_temp.append(linreg.intercept_.tolist()[0])
    list_temp.append(r2_score(y.values.reshape(-1, 1), prediction))
    list_temp.append(mean_squared_error(y.values.reshape(-1, 1), prediction))
    list_temp.append(products['MANUFACTURER'].unique()[i])
    list_all.append(list_temp)
    list_temp=[]

regression_MANUFACTURER=pd.DataFrame(columns=['beta1','beta2','beta3','beta0','Variance','MSW','MANUFACTURER'],data=list_all)
regression_MANUFACTURER=pd.DataFrame(list_all)
regression_MANUFACTURER.to_csv('/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/Linear_regression_Manufacturer.csv')

#Generalized Linear Model - Poisson Regressor
list_all=[]
list_temp=[]
for i in range(products['MANUFACTURER'].nunique()):

    x=products[products['MANUFACTURER']==products['MANUFACTURER'].unique()[i]][['mean_PERC_COUPON_DISC','mean_PERC_RETAIL_DISC','count_STORE_ID']]
    y=products[products['MANUFACTURER']==products['MANUFACTURER'].unique()[i]][['sum_QUANTITY']]

    glm_pr = PoissonRegressor(alpha=0, fit_intercept=False)
    y_pred_pr = glm_pr.fit(x, y).predict(x)

    linreg = LinReg(fit_intercept=True, normalize=False, copy_X=True, n_jobs=-1)
    linreg.fit(x, y)
    print('coeff: {0}; intercept: {1}'.format(str(linreg.coef_[0,0]), str(linreg.intercept_[0])))

    linreg.score(x, y)
    from sklearn.metrics import mean_squared_error, r2_score
    prediction = linreg.predict(x)
    print("Variance score: %.6f"
          % r2_score(y.values.reshape(-1, 1), prediction))
    print("Mean squared error: %.6f"
          % mean_squared_error(y.values.reshape(-1, 1), prediction))

    list_temp=(linreg.coef_[0].tolist())
    list_temp.append(linreg.intercept_.tolist()[0])
    list_temp.append(r2_score(y.values.reshape(-1, 1), prediction))
    list_temp.append(mean_squared_error(y.values.reshape(-1, 1), prediction))
    list_temp.append(products['MANUFACTURER'].unique()[i])
    list_all.append(list_temp)
    list_temp=[]

regression_MANUFACTURER=pd.DataFrame(columns=['beta1','beta2','beta3','beta0','Variance','MSW','MANUFACTURER'],data=list_all)
regression_MANUFACTURER=pd.DataFrame(list_all)
regression_MANUFACTURER.to_csv('/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/Linear_regression_Manufacturer.csv')
