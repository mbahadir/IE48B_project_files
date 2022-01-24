#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import matplotlib.pyplot as plt
import os


# In[2]:


os.getcwd()


# In[3]:


data_path="D:/Datasets/IE48B_project/sample_data_files/"
initial_data="First_Data.csv"


# In[4]:


# data=pd.read_csv(f"{data_path}{initial_data}")


# In[5]:


data=pd.read_csv("bulk_imbalance.csv")


# In[6]:


data.head()


# In[7]:


data.info()


# ## Data Inspection

# In[8]:


import pandas_profiling


# In[9]:


profile = pandas_profiling.profile_report.ProfileReport(data, explorative=True)


# In[10]:


profile


# # Correlation

# In[11]:


data.columns


# In[12]:


corr_df=data.corr()


# In[13]:


corr_df


# In[15]:


corr_df[["net"]].sort_values("net")


# ## Result

# yal_one aslinda buradaki en kritik değer olarak kabul edilebilir çünkü o belirleyici bir rol oynuyor diğerlerine nazaran.

# # Visual Contorl for Sum Yal

# In[18]:


plt.plot(data["net"])
plt.show()


# # Manuel Anomaly Detection

# In[19]:


import numpy as np


# In[20]:


data.head()


# In[21]:


data_req_hour=data[(data["hour"]<=23) & (data["hour"]>=12)]


# ## Aggregation

# In[22]:


data_daily=data_req_hour.loc[:, (data_req_hour.columns!="hour") ].groupby(["date"]).agg("sum")


# In[23]:


data_daily


# # Events

# In[24]:


from datetime import datetime, timedelta


# In[26]:


data_daily["event"]=""


# In[27]:


events={"2019-01-01":"Yeni Yil Tatili", 
        "2019-04-23":"Ulusal Egemenlik ve cocuk Bayrami",
        "2019-05-01":"Emek ve Dayanisma Gunu",
        "2019-05-19":"Ataturk’u Anma Genclik ve Spor Bayrami", 
        "2019-06-03":"Ramazan Bayrami Arifesi",
        "2019-06-04":"Ramazan Bayrami",
        "2019-06-05":"Ramazan Bayrami", 
        "2019-06-06":"Ramazan Bayrami",
        "2019-07-15":"Demokrasi ve Milli Birlik Gunu", 
        "2019-08-10":"Kurban Bayrami Arifesi",
        "2019-08-11":"Kurban Bayrami", 
        "2019-08-12":"Kurban Bayrami",
        "2019-08-13":"Kurban Bayrami", 
        "2019-08-14":"Kurban Bayrami",
        "2019-08-30":"Zafer Bayrami", 
        "2019-10-28":"Cumhuriyet Bayrami",
        "2019-10-29":"Cumhuriyet Bayrami", 
        
        "2020-01-01":"Yeni Yil Tatili", 
        "2020-04-23":"Ulusal Egemenlik ve cocuk Bayrami",
        "2020-05-01":"Emek ve Dayanisma Gunu",
        "2020-05-19":"Ataturk’u Anma Genclik ve Spor Bayrami", 
        "2020-05-23":"Ramazan Bayrami Arifesi",
        "2020-05-24":"Ramazan Bayrami",
        "2020-05-25":"Ramazan Bayrami", 
        "2020-05-26":"Ramazan Bayrami",
        "2020-07-15":"Demokrasi ve Milli Birlik Gunu", 
        "2020-07-30":"Kurban Bayrami Arifesi",
        "2020-07-31":"Kurban Bayrami", 
        "2020-08-01":"Kurban Bayrami",
        "2020-08-02":"Kurban Bayrami", 
        "2020-08-03":"Kurban Bayrami",
        "2020-08-30":"Zafer Bayrami", 
        "2020-10-28":"Cumhuriyet Bayrami",
        "2020-10-29":"Cumhuriyet Bayrami",
        
        "2021-01-01":"Yeni Yil Tatili", 
        "2021-04-23":"Ulusal Egemenlik ve cocuk Bayrami",
        "2021-05-01":"Emek ve Dayanisma Gunu",
        "2021-05-12":"Ataturk’u Anma Genclik ve Spor Bayrami", 
        "2021-05-13":"Ramazan Bayrami Arifesi",
        "2021-05-14":"Ramazan Bayrami",
        "2021-05-15":"Ramazan Bayrami", 
        "2021-05-19":"Ramazan Bayrami",
        "2021-07-15":"Demokrasi ve Milli Birlik Gunu", 
        "2021-07-19":"Kurban Bayrami Arifesi",
        "2021-07-20":"Kurban Bayrami", 
        "2021-07-21":"Kurban Bayrami",
        "2021-07-22":"Kurban Bayrami", 
        "2021-07-23":"Kurban Bayrami",
        "2021-08-30":"Zafer Bayrami", 
        "2021-10-28":"Cumhuriyet Bayrami",
        "2021-10-29":"Cumhuriyet Bayrami"}


# In[28]:


for key, value in events.items():
    if key in data_daily.index:
        data_daily.at[key, "event"]= value


# In[29]:


data_daily


# In[30]:


data_daily.to_csv(data_path+"data_daily_event.csv")


# # All Extreme Points

# In[33]:


upper_limit=np.quantile(data_daily["net"], 0.95)
lower_limit=np.quantile(data_daily["net"], 0.05)


# In[34]:


upper_limit


# In[35]:


lower_limit


# In[38]:


ext_data=data_daily[(data_daily["net"]>=upper_limit) | (data_daily["net"]<=lower_limit)]


# In[39]:


ext_data.head()


# In[40]:


ext_data.info()


# ## Important Date Finder

# In[41]:


important_dates_2=[]
important_dates_3=[]
important_dates_4=[]
for i in range(ext_data.shape[0]-1):
    current_date=datetime.strptime(ext_data.index[i], '%Y-%m-%d')
    next_day=current_date + timedelta(days=1)
    two_day=current_date + timedelta(days=2)
    three_day=current_date + timedelta(days=3)
    if datetime.strptime(ext_data.index[i+1], '%Y-%m-%d')==next_day:
        important_dates_2.append(ext_data.index[i])
        if i < ext_data.shape[0]-2:
            if datetime.strptime(ext_data.index[i+2], '%Y-%m-%d')==two_day:
                important_dates_3.append(ext_data.index[i])
                if i < ext_data.shape[0]-3:
                    if datetime.strptime(ext_data.index[i+3], '%Y-%m-%d')==three_day:
                        important_dates_4.append(ext_data.index[i])


# In[42]:


important_dates_2


# In[43]:


important_dates_3


# In[44]:


important_dates_4


# ### Date Controls

# In[45]:


events_df=pd.DataFrame.from_dict(events,orient='index')
events_df.reset_index(level=0, inplace=True)


# In[46]:


events_df[events_df.isin(important_dates_2).loc[:,"index"]]


# In[47]:


events_df[events_df.isin(important_dates_3).loc[:,"index"]]


# In[48]:


events_df[events_df.isin(important_dates_4).loc[:,"index"]]


# # Postive Imbalance

# In[50]:


pot_ext_df=ext_data[ext_data["net"]>0]
pot_ext_df


# ## Event Names

# In[52]:


pot_ext_df[pot_ext_df["event"]!=""]


# ## Sorted Values

# In[53]:


pot_ext_df.sort_values("net", ascending=False).iloc[1:10,:]


# # Negative Imbalance

# In[54]:


neg_ext_df=ext_data[ext_data["net"]<0]
neg_ext_df


# In[60]:


neg_ext_df[neg_ext_df["event"]!=""].iloc[:,[0,-1]]


# In[ ]:





# In[ ]:




