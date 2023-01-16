#Summarize the total promotion budget spent on each products

import pandas as pd
import matplotlib
import xlwings as xw
import numpy as np

from matplotlib import pyplot as plt

directory = '/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon'

transactions=pd.read_csv(directory+'/transactions.csv')
demographics=pd.read_csv(directory+'/demographics.csv')

transactions_full=transactions.merge(demographics,how='left',on='household_key')
transactions_full_national=transactions_full[transactions_full['BRAND']=='National']

transactions_full_national['ORG_PRICE']=(transactions_full_national['SALES_VALUE']-transactions_full_national['COUPON_MATCH_DISC']-transactions_full_national['RETAIL_DISC'])/transactions_full_national['QUANTITY']
transactions_full_national['PERC_RETAIL_DISC']=transactions_full_national['RETAIL_DISC']/transactions_full_national['ORG_PRICE']
transactions_full_national['PERC_COUPON_DISC']=transactions_full_national['COUPON_MATCH_DISC']/transactions_full_national['ORG_PRICE']

pd.pivot_table(transactions_full_national,index=['MANUFACTURER','PRODUCT_ID'],values=['SALES_VALUE','QUANTITY','COUPON_MATCH_DISC','RETAIL_DISC','ORG_PRICE'],aggfunc='sum')

x=pd.pivot_table(transactions_full_national,index=['PRODUCT_ID','MANUFACTURER','DEPARTMENT','COMMODITY_DESC','SUB_COMMODITY_DESC'],values=['SALES_VALUE','QUANTITY','COUPON_MATCH_DISC','RETAIL_DISC','ORG_PRICE','PERC_COUPON_DISC','PERC_RETAIL_DISC'],aggfunc=['sum','mean','max'])
x.to_csv('/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/export.csv')

y=pd.pivot_table(transactions_full_national,index=['PRODUCT_ID'],values=['household_key','BASKET_ID','STORE_ID'],aggfunc=[pd.Series.nunique,'count'])
y.to_csv('/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/export.csv')
