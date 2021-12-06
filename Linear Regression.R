################# Questions on simple linear regression #################

# 1.Build a simple linear model: a.Divide the data-set into train & test sets in 70:30 ratio. Splitting criteria would be determined by the 'tenure' column.
library(readr)
churn = read.csv("C:\\Users\\user\\Downloads\\Module-01_Introduction to Data Science _ R Programming_Module_1\\Data-Set\\customer_churn.csv")
library(caTools)

split = sample.split(churn$tenure,SplitRatio = 0.70)
train = subset(churn, split==T)
test = subset(churn, split==F)

nrow(train)
nrow(test)


# b.Build a simple linear model on the 'train' set, where the dependent variable is 'tenure' & the independent variable is 'Contract' & store the result in 'model1'
model1 = lm(tenure~Contract, data=train) 
predicted_values = predict(model1, newdata=test)


# d.Bind the actual values & predicted values & store the result in 'final_data'
final_data = cbind(Actual=test$tenure,Predicted=predicted_values)
head(final_data)

class(final_data)
as.data.frame(final_data)->final_data
class(final_data)


# e.Find out the error in prediction & store the result in 'error'
error = final_data$Actual - final_data$Predicted
head(error)


# f.Bind 'error' to 'final_data' object
final_data = cbind(final_data,error) 
head(final_data)


# g.Find the root mean square error
rmse = sqrt(mean((final_data$error)^2))


######### Assumptions ######### 
library(ggplot2)  

ggplot(data = churn, aes(x=Contract, y=tenure)) + geom_point()
ggplot(data = churn, aes(x=Contract, y=tenure)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = churn, aes(x=tenure, y=Contract)) + geom_point()
ggplot(data = churn, aes(x=tenure, y=Contract)) + geom_point() + geom_smooth(method = "lm")

mod1 = lm(tenure~Contract, data = churn)
result1 = predict(mod1,data=churn)
final_data1 = cbind(Actual=churn$tenure, Predicted=result1)
head(final_data1)
final_data1 = as.data.frame(final_data1)

error1 = final_data1$Actual - final_data1$Predicted
final_data1 = cbind(final_data1,error1)

ggplot(data= final_data1, aes(x=result1, y=error1)) + geom_point()

qqnorm(final_data1$error1)




################# Questions on multiple linear regression #################

# 1.Build a multiple linear regression model: a.Divide the data-set into train & test sets in 75:25 ratio. Splitting criteria would be determined by the 'MonthlyCharges' column.
split2 = sample.split(churn$MonthlyCharges,SplitRatio = 0.75)
train2 = subset(churn, split2==T)
test2 = subset(churn, split2==F)

nrow(train2)
nrow(test2)


# b.Build a linear model on the 'train' set, where the dependent variable is 'MonthlyCharges' & the independent variables are 'Dependents', 'MultipleLines', 'OnlineSecurity','OnlineBackup' & 'DeviceProtection' and store the result in 'mod_multi_linear'
colnames(churn)
mod_multi_linear = lm(MonthlyCharges~Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection,data=train2)


# c.Predict the values on top of the 'test' set & store the result in 'predicted_multi_linear'
predicted_multi_linear = predict(mod_multi_linear,newdata=test2)


# d.Bind the actual values & predicted values & store the result in 'final_data'
final_data2 = cbind(Actual=test$MonthlyCharges,Predicted=predicted_multi_linear)
head(final_data2)
class(final_data2)
final_data2 = as.data.frame(final_data2)
class(final_data2)


# e.Find out the error in prediction & store the result in 'error'
error2 = (final_data2$Actual-final_data2$Predicted)


# f.Bind 'error' to 'final_data' object
final_data2 = cbind(final_data2,error2)


# g.Find the root mean square error
rmse2 = sqrt(mean((final_data2$error2)^2))


######### Assumptions ######### 
library(ggplot2)  

#ggplot(data = churn, aes(x=MonthlyCharges, y=)) + geom_point()
#ggplot(data = churn, aes(x=MonthlyCharges, y=)) + geom_point() + geom_smooth(method = "lm")

mod2 = lm(MonthlyCharges~Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data = churn)
result2 = predict(mod1,data=churn)

final_data3 = cbind(Actual=churn$MonthlyCharges, Predicted=result2)
head(final_data3)
final_data3 = as.data.frame(final_data3)

error3 = final_data1$Actual - final_data3$Predicted
final_data3 = cbind(final_data3,error3)

ggplot(data= final_data3, aes(x=result2, y=error3)) + geom_point()

qqnorm(final_data3$error3)

