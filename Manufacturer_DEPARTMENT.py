#Summarize the main department of the goods sold by the manufacturer

#open "/Applications/IntelliJ IDEA CE.app/Contents/MacOS/idea"

import pandas as pd
import matplotlib
import xlwings as xw

from matplotlib import pyplot as plt

directory = '/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon'

transactions=pd.read_csv(directory+'/transactions.csv')
demographics=pd.read_csv(directory+'/demographics.csv')

transactions_full=transactions.merge(demographics,how='left',on='household_key')
transactions_full_national=transactions_full[transactions_full['BRAND']=='National']

category_temp=pd.DataFrame()
list_MFT_DPT=[]
for i in range(len(transactions_full_national['MANUFACTURER'].unique())):
    print(transactions_full_national['MANUFACTURER'].unique()[i])
    category_temp=[x for x in transactions_full_national[transactions_full_national['MANUFACTURER']==transactions_full_national['MANUFACTURER'].unique()[i]]['DEPARTMENT'].unique() if pd.isnull(x)==False]
    print(category_temp)
    list_MFT_DPT.append(";".join(str(elem) for elem in category_temp))
df_MFT_DPT=pd.DataFrame({'Manufacturer':transactions_full_national['MANUFACTURER'].unique(),'DEPARTMENT':list_MFT_DPT})

a=xw.Book()
a1=a.sheets[0]
a1.range("A1").value=pd.pivot_table(transactions_full_national,index='MANUFACTURER',values=['COUPON_MATCH_DISC','RETAIL_DISC'],aggfunc='sum')
a1.range("K1").value=df_MFT_DPT

for i in range(17):
    transactions_national.iloc[:,i:i+1].plot(kind="bar")
