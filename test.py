#%%
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import warnings
import seaborn as sns

import matplotlib.pyplot as plt
import mplcursors 

warnings.filterwarnings("ignore")
os.chdir("/Users/yj.noh/Documents/GitHub")
print(os.getcwd())
plt.rcParams['font.family'] = 'AppleGothic'


data = pd.read_excel("/Users/yj.noh/Desktop/new_rider_need_rgn2.xlsx")
data['is_restrict'] = data.apply(lambda row : 1 if row['baemin1_single_limited_time'] > 0 else 0, axis =1)
data['region'] = data['rgn1_nm'] + " " +  data['rgn2_nm']

avg_r_values = data.groupby(['region', 'is_restrict']).r_value.mean().unstack()
plt.scatter(avg_r_values[0], avg_r_values[1])

cursor = mplcursors.cursor(hover=True)
cursor.connect("add", lambda sel: sel.annotation.set_text(avg_r_values.index[sel.target.index]))

# 축 레이블 및 타이틀 설정
plt.xlabel('Average r_value when is_restrict = 0')
plt.ylabel('Average r_value when is_restrict = 1')
plt.title('Average r_value by Region and Restriction Status')

plt.show()

# %%
