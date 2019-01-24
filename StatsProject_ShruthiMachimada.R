###Load the data

data<-data.frame(read.csv(file="Desktop/Stats Project/insurance.csv",header = TRUE, sep = ","))
head(data,n=10)

#--------------------------------------------------
library(tidyverse)
library(funModeling)
library(Hmisc)
library(ggplot2)
#View the data, data types, number of observations
glimpse(data)

#Check the quantity and percentage of zeros, NAs and infinite values. Gives the data types and number of unique values
df_status(data)

#Distribution of categorical variables
freq(data$sex)
freq(data$smoker)
freq(data$region)

#Distribution of numeric variables
plot_num(data)

summary(data)
describe(data)


#Impact of region on charges
ggplot(data = data,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
ggtitle("Medical Charges per Region")

#Impact of smoking on charges
ggplot(data = data,aes(smoker,charges)) + 
  geom_boxplot(fill = c(2:3)) + 
  ggtitle("Medical Charges for Smokers vs Non-smokers")

#Impact of gender on charges
ggplot(data = data,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
ggtitle("Medical Charges by Gender")


library(psych)
pairs.panels(data[c("age","bmi","children","region","sex","charges")])


#----------------------------------------------------------------------
#Split the data into training and test sets

library(ISLR)

training_size = floor(0.75*nrow(data))
training_size

set.seed(123)   # set seed to ensure you always have same random numbers generated
training_ind = sample(seq_len(nrow(data)),size = training_size) 

data_train =data[training_ind,]
data_test= data[-training_ind,]

#---------------------------------------------------------------------
#Using all 6 variables
#Fit a Linear Regression model using the Training set
linear_model6<-lm(charges~.,data=data_train)
summary(linear_model6)

anova(charges~.,data=data)

#use the model to predict the test set
pred_6var <- predict(linear_model6, data_test)
pred_6var

#Evaluate the Model
residual<-pred_6var - data_test$charges
plot(residual)
boxplot(residual)

require(miscTools)
r2 <- rSquared(pred_6var, resid = residual)
r2

n<-nrow(data_test)
n
adj_r2<-r2 * ( (n- 1) / (n - 5))
adj_r2

#------------------------------------------------------------------
#Gender has a p value > 0.05, we Reject the Null B=0 for gender
#Using all vcariables except gender
linear_model5<-lm(charges~age+bmi+children+smoker+region,data=data_train)
summary(linear_model5)

#use the model to predict the test set
pred_5var <- predict(linear_model5, data_test)
pred_5var

#Evaluate the Model
residual_5<-pred_5var - data_test$charges
plot(residual_5)
boxplot(residual_5)

r2_noGender <- rSquared(pred_5var, resid = residual_5)
r2_noGender
adj_r2_noGender<-r2_noGender * ( (n- 1) / (n - 4))
adj_r2_noGender

#-------------------------------------------------------------------
#With no BMI since its distribution is all over
plot(data$charges,data$bmi,xlab="charges",ylab="BMI")

linear_modelBMI<-lm(charges~age+sex+children+smoker+region,data=data_train)
summary(linear_modelBMI)

pred_BMI <- predict(linear_modelBMI, data_test)


residual_BMI<-pred_BMI - data_test$charges
plot(residual_BMI)
boxplot(residual_BMI)

r2_noBMI <- rSquared(pred_BMI, resid = residual_BMI)
r2_noBMI
adj_r2_noBMI<-r2_noBMI * ( (n- 1) / (n - 4))
adj_r2_noBMI
#-------------------------------------------------------------------
#With no BMI and Gender
lm_noBMIGender<-lm(charges~age+children+smoker+region,data=data_train)
summary(lm_noBMIGender)

pred_BMIGender <- predict(lm_noBMIGender, data_test)
pred_BMIGender

residual_BMIGender<-pred_BMIGender - data_test$charges
plot(residual_BMIGender)
boxplot(residual_BMIGender)

r2_noBMIGender <- rSquared(pred_BMIGender, resid = residual_BMIGender)
r2_noBMIGender
adj_r2_noBMIGender<-r2_noBMIGender * ( (n- 1) / (n - 3))
adj_r2_noBMIGender

#------------------------------------------------------------------
#With no region, since p value is > 0.05
lm_noRegion<-lm(charges~age+sex+bmi+children+smoker,data=data_train)
summary(lm_noRegion)

pred_Region <- predict(lm_noRegion, data_test)


residual_Region<-pred_Region - data_test$charges
plot(residual_Region)
boxplot(residual_Region)

r2_noRegion <- rSquared(pred_Region, resid = residual_Region)
r2_noRegion
adj_r2_noRegion<-r2_noRegion * ( (n- 1) / (n - 4))
adj_r2_noRegion


#----------------------------------------------------------------
#with age polynomial eqn
lm_polyAge<-lm(charges~age+I(age^2)+sex+bmi+children+smoker+region,data=data_train)
summary(lm_polyAge)

pred_polyAge <- predict(lm_polyAge, data_test)
pred_polyAge

residual_polyAge<-pred_polyAge - data_test$charges

r2_polyAge <- rSquared(pred_polyAge, resid = residual_polyAge)
r2_polyAge
adj_r2_polyAge<-r2_polyAge * ( (n- 1) / (n - 5))
adj_r2_polyAge
#-------------------------------------------------------------------
#Poly Age + No Gender

lm_polyAgeNoGender<-lm(charges~age+I(age^2)+bmi+children+smoker+region,data=data_train)
summary(lm_polyAgeNoGender)

pred_polyAgeNoGender <- predict(lm_polyAgeNoGender, data_test)


residual_polyAgeNoGender<-pred_polyAgeNoGender - data_test$charges

r2_polyAgeNoGender <- rSquared(pred_polyAgeNoGender, resid = residual_polyAgeNoGender)
r2_polyAgeNoGender
adj_r2_polyAgeNoGender<-r2_polyAgeNoGender * ( (n- 1) / (n - 4))
adj_r2_polyAgeNoGender


#-------------------------------------------------------------------
#Poly Age + No Region + No Sex

lm_polyAgeNoRegionSex<-lm(charges~age+I(age^2)+bmi+children+smoker,data=data_train)
summary(lm_polyAgeNoRegionSex)

pred_polyAgeNoRegionSex <- predict(lm_polyAgeNoRegionSex, data_test)
pred_polyAgeNoRegionSex

residual_polyAgeNoRegionSex<-pred_polyAgeNoRegionSex - data_test$charges


r2_polyAgeNoRegionSex <- rSquared(pred_polyAgeNoRegionSex, resid = residual_polyAgeNoRegionSex)
r2_polyAgeNoRegionSex
adj_r2_polyAgeNoRegionSex<-r2_polyAgeNoRegionSex * ( (n- 1) / (n - 3))
adj_r2_polyAgeNoRegionSex
#--------------------------------------------------------------------
#Comparing models

r_values<-data.frame(model=c("All variables","- Gender","- BMI","- Gender and BMI","- region","poly Age","polyAge - Gender","polyAge - Region - Gender"),
                     rSquaredValue=c(r2,r2_noGender,r2_noBMI,r2_noBMIGender,r2_noRegion,r2_polyAge,r2_polyAgeNoGender, r2_polyAgeNoRegionSex),
                     adjrSquared=c(adj_r2,adj_r2_noGender,adj_r2_noBMI,adj_r2_noBMIGender,adj_r2_noRegion,adj_r2_polyAge,adj_r2_polyAgeNoGender, adj_r2_polyAgeNoRegionSex))
r_values


