# -*- coding: utf-8 -*-
"""
Created on Mon Apr 27 22:47:58 2020

@author: RAVI
"""


# For reading data set
# importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

Salary_exp = pd.read_csv("C:/RAVI/Data science/Assignments/Module 6 Simple linear regression/DataSets/Salary_Data.csv")   #C:/RAVI/Data science/Assignments/Module 6 Simple linear regression/DataSets/Salary_Data.csv")

import matplotlib.pylab as plt #for different types of plots

plt.scatter(x=Salary_exp['YearsExperience'], y=Salary_exp['Salary'],color='green')# Scatter plot

np.corrcoef(Salary_exp.YearsExperience, Salary_exp.Salary) #correlation

help(np.corrcoef)

import statsmodels.formula.api as smf
plt.hist(Salary_exp["YearsExperience"])

model = smf.ols('Salary ~ YearsExperience', data=Salary_exp).fit()
model.summary()

#values prediction
#Confidence interval Calculation
pred1 = model.predict(pd.DataFrame(Salary_exp['YearsExperience']))
pred1
print (model.conf_int(0.95)) # 95% confidence interval

res = Salary_exp.Salary - pred1
sqres = res*res
mse = np.mean(sqres)
rmse = np.sqrt(mse)

######### Model building on Transformed Data#############

# Log Transformation
# x = log(YearsExperience); y = Salary
plt.scatter(x=np.log(Salary_exp['YearsExperience']),y=Salary_exp['Salary'],color='brown')
np.corrcoef(np.log(Salary_exp.YearsExperience), Salary_exp.Salary) #correlation

model2 = smf.ols('Salary ~ np.log(YearsExperience)',data=Salary_exp).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(Salary_exp['YearsExperience']))
pred2
print(model2.conf_int(0.95)) # 95% confidence level

res2 = Salary_exp.Salary - pred2
sqres2 = res2*res2
mse2 = np.mean(sqres2)
rmse2 = np.sqrt(mse2)

# Exponential transformation
plt.scatter(x=Salary_exp['YearsExperience'], y=np.log(Salary_exp['Salary']),color='orange')

np.corrcoef(Salary_exp.YearsExperience, np.log(Salary_exp.Salary)) #correlation

model3 = smf.ols('np.log(Salary) ~ YearsExperience',data=Salary_exp).fit()
model3.summary()

pred_log = model3.predict(pd.DataFrame(Salary_exp['YearsExperience']))
pred_log
pred3 = np.exp(pred_log)
pred3
print(model3.conf_int(0.95)) # 95% confidence level

res3 = Salary_exp.Salary - pred3
sqres3 = res3*res3
mse3 = np.mean(sqres3)
rmse3 = np.sqrt(mse3)

############Polynomial model with 2 degree (quadratic model)  ;x = YearsExperience*YearsExperience; y = Salary############
#### input=x & X^2 (2-degree); output=y  ####
model4 = smf.ols('Salary ~ YearsExperience+I(YearsExperience*YearsExperience)', data=Salary_exp).fit()
model4.summary()

pred_p2 = model4.predict(pd.DataFrame(Salary_exp['YearsExperience']))
pred_p2

print(model3.conf_int(0.95)) # 95% confidence level

res4 = Salary_exp.Salary - pred_p2
sqres4 = res4*res4
mse4 = np.mean(sqres4)
rmse4 = np.sqrt(mse4)

###########Polynomial model with 3 degree (quadratic model)  ;x = YearsExperience*YearsExperience*YearsExperience; y = Salary############
#### input=x & X^2 (2-degree); output=y  ####
model5 = smf.ols('Salary ~ YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience)', data=Salary_exp).fit()
model5.summary()

pred_p3 = model5.predict(pd.DataFrame(Salary_exp['YearsExperience']))
pred_p3

print(model5.conf_int(0.95)) # 95% confidence level

res5 = Salary_exp.Salary - pred_p3
sqres5 = res5*res5
mse5 = np.mean(sqres5)
rmse5 = np.sqrt(mse5)


















