#List out the manufacturers which list more than 1 products in the supermarket

import pandas as pd
import matplotlib
import xlwings as xw

from matplotlib import pyplot as plt

directory = '/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon'

transactions=pd.read_csv(directory+'/transactions.csv')
demographics=pd.read_csv(directory+'/demographics.csv')

transactions_full=transactions.merge(demographics,how='left',on='household_key')
transactions_full_national=transactions_full[transactions_full['BRAND']=='National']

pd.pivot_table(transactions_full_national,index='MANUFACTURER',values='COUPON_MATCH_DISC',aggfunc='sum')

for i in range(len(transactions_full_national['PRODUCT_ID'].unique())):
    product_size=transactions_full_national[transactions_full_national['PRODUCT_ID']==transactions_full_national['PRODUCT_ID'].unique()[i]]['CURR_SIZE_OF_PRODUCT'].unique()
    if len(product_size)>1:
        print(transactions_full_national['PRODUCT_ID'].unique()[i])
        print(product_size)

