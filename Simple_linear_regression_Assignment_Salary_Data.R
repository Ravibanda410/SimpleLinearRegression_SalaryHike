#  Simple Linear Regression Assignment #
#  4) Salary_hike -> Build a prediction model for Salary_hike
#  Do the necessary transformations for input variables for getting better R^2 value for the model prepared.  #

library(readr)
library(ggplot2)
library(moments)
Salary_exp <- read_csv("C:/RAVI/Data science/Assignments/Module 6 Simple linear regression/DataSets/Salary_Data.csv")   #C:/RAVI/Data science/Assignments/Module 6 Simple linear regression/DataSets/Salary_Data.csv")
View(Salary_exp)
attach(Salary_exp)

summary(Salary_exp)
mean(YearsExperience)
skewness(YearsExperience)
skewness(Salary_exp$Salary)

#Exploratory Data Analysis
boxplot(YearsExperience)
boxplot(Salary,horizontal = T)

#scatter plot for YearsExperience vs Salary (Plot x,y)
plot(Salary_exp$YearsExperience,Salary_exp$Salary)

#calculate correlation coefficient
cor(Salary_exp$YearsExperience,Salary_exp$Salary)

#Simple Regression model
reg <- lm(Salary_exp$Salary ~ Salary_exp$YearsExperience,data = Salary_exp)
summary(reg)

#values prediction

#Confidence interval Calculation
confint(reg,level = 0.95)

pred <- predict(reg,interval ="predict")
#predict function gives fit value and its lower and upeer values as a range

pred <- as.data.frame(pred)
pred

#####Plot Graph for both Actual values and also the predicted linear Graph(Actual:Red,Predicted:Blue)#########

ggplot() + 
  geom_point(aes(x = Salary_exp$YearsExperience, y = Salary_exp$Salary),
             colour='red') + 
  geom_line(aes(x = Salary_exp$YearsExperience, y = predict(reg, newdata=Salary_exp)),
            colour='blue') + 
  ggtitle('Salary vs Experience (Salary_exp)') +
  xlab('Years of experience') +
  ylab('Salary')


cor(pred$fit,Salary_exp$Salary)

#Calculate Residuals "Errors"
reg$residuals
reg$residuals^2
mean(reg$residuals^2)
rmse <- sqrt(mean(reg$residuals^2))
rmse

############ Applying transformations##############
############ lOGORITHMIC MODEL    x = log(YearsExperience); y = Salary ############
plot(log(Salary_exp$YearsExperience),Salary_exp$Salary)
cor(log(Salary_exp$YearsExperience),Salary_exp$Salary)

log_reg <- lm(Salary_exp$Salary ~ log(Salary_exp$YearsExperience))
summary(log_reg)

#values prediction

#Confidence interval Calculation
confint(log_reg,level = 0.95)

pred_log <- predict(log_reg,interval ="predict")
#predict function gives fit value and its lower and upeer values as a range

pred_log <- as.data.frame(pred_log)
pred_log
log_reg$residuals
 
cor(pred_log$fit,Salary)

rmse_log <- sqrt(mean(log_reg$residuals^2)) 
rmse_log

##########Plot Graph for both Actual values and also the predicted linear Graph(Actual:Red,Predicted:Blue)#########
ggplot() + 
  geom_point(aes(x = Salary_exp$YearsExperience, y = Salary_exp$Salary),
             colour='red') + 
  geom_line(aes(x = Salary_exp$YearsExperience, y = predict(log_reg, newdata=Salary_exp)),
            colour='blue') + 
  ggtitle('Salary vs Experience (Salary_exp)') +
  xlab('Years of experience') +
  ylab('Salary')

############ EXPONENTIAL MODEL   x = YearsExperience; y = log(Salary) ############
plot(Salary_exp$YearsExperience,log(Salary_exp$Salary))
cor(Salary_exp$YearsExperience,log(Salary_exp$Salary))

log_reg2 <- lm(log(Salary_exp$Salary) ~ Salary_exp$YearsExperience)
summary(log_reg2)

#values prediction

#Confidence interval Calculation
confint(log_reg2,level = 0.95)

pred_log2 <- predict(log_reg2,interval ="predict")
#predict function gives fit value and its lower and upeer values as a range

pred_log2 <- as.data.frame(pred_log2)

log_reg2$residuals #output is log(AT) so we are getting less values apply antilog

pred<- exp(pred_log2)  #anti-log=exponential
pred

cor(pred$fit,Salary_exp$Salary)

res_log2=Salary_exp$Salary-pred$fit
rmse2 <- sqrt(mean(res_log2^2))
rmse2


##########Plot Graph for both Actual values and also the predicted linear Graph(Actual:Red,Predicted:Blue)#########
ggplot() + 
  geom_point(aes(x = Salary_exp$YearsExperience, y = Salary_exp$Salary),
             colour='red') + 
  geom_line(aes(x = Salary_exp$YearsExperience, y =predict(log_reg2,newdata=Salary_exp)) ,
            colour='blue') + 
  ggtitle('Salary vs Experience (Salary_exp)') +
  xlab('Years of experience') +
  ylab('Salary')

############Polynomial model with 2 degree (quadratic model)  ;x = YearsExperience*YearsExperience; y = log(Salary)############
#### input=x & X^2 (2-degree); output=y  ####
reg_quad2<- lm(Salary_exp$Salary ~ Salary_exp$YearsExperience+I(Salary_exp$YearsExperience*Salary_exp$YearsExperience),data =Salary_exp)
summary(reg_quad2)

#prediction

#Confidence interval Calculation
confint(reg_quad2,level = 0.95)

pred_quad2<-predict(reg_quad2,interval = "predict")
pred_quad2  <- as.data.frame(pred_quad2)
pred_quad2

cor(pred_quad2$fit,Salary_exp$Salary)

resq=Salary_exp$Salary-pred_quad2$fit
rmse_quad<-sqrt(mean(resq$fit^2))
rmse_quad

##########Plot Graph for both Actual values and also the predicted linear Graph(Actual:Red,Predicted:Blue)#########
ggplot() + 
  geom_point(aes(x = Salary_exp$YearsExperience, y = Salary_exp$Salary),
             colour='red') + 
  geom_line(aes(x = Salary_exp$YearsExperience, y = predict(reg_quad2, newdata=Salary_exp)),
            colour='blue') + 
  ggtitle('Salary vs Experience (Salary_exp)') +
  xlab('Years of experience') +
  ylab('Salary')

############Polynomial model with 3 degree (quadratic model)  ;x = YearsExperience*YearsExperience*YearsExperience; y = Salary############
#### input=x & X^2 & x^3 (3-degree); output=y  ####
reg_quad3<- lm(Salary_exp$Salary ~ Salary_exp$YearsExperience+I(Salary_exp$YearsExperience*Salary_exp$YearsExperience)+I(Salary_exp$YearsExperience*Salary_exp$YearsExperience*Salary_exp$YearsExperience),data =Salary_exp)
summary(reg_quad3)

#prediction

#Confidence interval Calculation
confint(reg_quad3,level = 0.95)

pred_quad3<-predict(reg_quad3,interval = "predict")
pred_quad3  <- as.data.frame(pred_quad3)
pred_quad3

cor(pred_quad3$fit,Salary_exp$Salary)

resq3=Salary_exp$Salary-pred_quad3$fit
rmse_quad3<-sqrt(mean(resq3^2))
rmse_quad3
##########Plot Graph for both Actual values and also the predicted linear Graph(Actual:Red,Predicted:Blue)#########
ggplot() + 
  geom_point(aes(x = Salary_exp$YearsExperience, y = Salary_exp$Salary),
             colour='red') + 
  geom_line(aes(x = Salary_exp$YearsExperience, y = predict(reg_quad3, newdata=Salary_exp)),
            colour='blue') + 
  ggtitle('Salary vs Experience (Salary_exp)') +
  xlab('Years of experience') +
  ylab('Salary')

#### input=x & X^2 & x^3 (3-degree); output=y  ####
#not random sampling
train  <- Salary_exp[1:24,] 
test  <- Salary_exp[25:30,]

#random sampling
#n  <- nrow(Salary_exp)
#n1 <- n*0.8
#n1
#n2 <- n-n1
#n2

#train_ind  <-sample(1:n,n1)
#train <- Salary_exp[train_ind,]
#test  <-Salary_exp[-train_ind,]

model<- lm(Salary~YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience*YearsExperience*YearsExperience),data = train)
summary(model)

#prediction
confint(model,level = 0.95)

#test data
res<-predict(model,interval = "confidence",newdata = test)

predict_original <- as.data.frame(res)
predict_original

test_error <- test$Salary-predict_original$fit  #calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

#training data
res_train <-predict(model,interval = "confidence",newdata = train)

predict_original_train <- as.data.frame(res_train)

train_error <- train$Salary - predict_original_train$fit  #calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse
