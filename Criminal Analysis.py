#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# In[2]:


ccd = pd.read_csv("ccd2020.csv")
ccd.head()


# In[3]:


ccd.shape


# In[4]:


list(ccd.columns)


# In[5]:


ccd.info()


# In[6]:


ccd.isnull().sum()


# In[21]:


ccd['cdi'].value_counts()


# In[22]:


ccd['casestatus'].value_counts()


# In[7]:


ccd['race'].value_counts().plot(kind='bar')


# In[9]:


ccd['ucrviolentflg'].value_counts().plot(kind='bar')


# In[10]:


ccd['sex'].value_counts()


# In[23]:


ccd['cmcourt'].value_counts()


# In[24]:


ccd['instrumenttype'].value_counts()


# In[19]:


ccd['race'].value_counts()


# In[26]:


ccd['level_degree'].value_counts()


# In[25]:


ccd['ethnicity'].value_counts()


# In[27]:


ccd['offenseliteral'].value_counts()


# In[11]:


ccd['ucrgroup'].value_counts()


# In[29]:


ccd['bondtypecd'].value_counts()


# In[31]:


ccd['primaryoffenselevelatbooking'].value_counts()


# In[32]:


ccd['firstwarrantreason'].value_counts()


# In[33]:


ccd['firstwarranttype'].value_counts()


# In[34]:


ccd['bondtypedsc'].value_counts()


# In[36]:


ccd['warrantlist'].value_counts()


# In[39]:


ccd['holdforoffenseliteral'].value_counts()


# In[40]:


ccd['probablecause'].value_counts()


# In[37]:


ccd['attorneystatus'].value_counts()


# In[42]:


ccd['initialdisposition'].value_counts()


# In[43]:


ccd['casedisposition'].value_counts()


# In[45]:


ccd['judgementdsc'].value_counts()


# In[46]:


ccd['psareleaserecommendation'].value_counts()


# In[48]:


ccd['disposition1'].value_counts()


# In[47]:


ccd['psascorerisk'].value_counts()


# In[12]:


ccd = ccd.set_index(pd.to_datetime(ccd['birthyyyymm']))


# In[13]:


ccd['year'] = pd.DatetimeIndex(ccd['birthyyyymm']).year


# In[14]:


ccd.info()


# In[15]:


ccd['age'] = ccd['downloadyr'] - ccd['year']


# In[16]:


ccd.hist(column = 'age')


# In[17]:


ccd['age'].describe()


# In[18]:


ccd['age'].plot(kind='box', title='Age of the defendant')


# In[53]:


setting = pd.read_csv("setting2020.csv")
setting.head()


# In[55]:


setting.shape


# In[56]:


list(setting.columns)


# In[58]:


bond = pd.read_csv("bond2020.csv")
bond.shape


# In[59]:


list(bond.columns)


# In[62]:


booking = pd.read_csv("booking2020.csv")
booking.shape


# In[63]:


list(booking.columns)


# In[ ]:




