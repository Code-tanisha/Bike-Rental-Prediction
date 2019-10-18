rm(list = ls(all = T))

# Set working directory
setwd("C:\\Edwisor\\R")
# Get working directory
getwd()

# Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees")

# Install Packages(x)
lapply(x, require, character.only = TRUE)

for (i in x) {
  print(i)
  library("ggplot2")
  
}

install.packages(c("dplyr", "plyr", "reshape", "ggplot2", "data.table"))
install.packages("GGally")

# Install these libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")

# Load bike rental data 
df = read.csv("day.csv")
View(df)

# Summarizing data
head(df)
# Target variable='cnt' and other variables are independent variables

# Verify summary of data
summary(df)

# It shows variables like 'month', 'weekday', 'weathersit', are categorical variables and already encoded
# Numeric variables like 'temp', 'atemp', 'hum', 'windspeed are in standardized form
# data contains no missing values
# Outliers might be present in variables

# Structure od data
str(df)

# Analyze variables by visualize
# Function to create univariate distribution of numeric variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(df)+
    geom_histogram(aes(x=num_x,y=..density..), binwidth = 0.05,
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}
# analyse the distribution of target variables 'cnt'
univariate_numeric(df$cnt)
# analyse the distribution of independence variable temp
univariate_numeric(df$temp)
# analyse the distribution of independence variable 'atemp'
univariate_numeric(df$atemp)
# analyse the distribution of independence variable 'hum'
univariate_numeric(df$hum)
# analyse the distribution of independence variable 'casual'
univariate_numeric(df$casual)
# analyse the distribution of independence variable 'registered'
univariate_numeric(df$registered)

# the above graph shows that the cnt data is normally distributed
# Visualize categorical variable 'mnth' with the target variable 'cnt'
ggplot(df, aes(x = as.factor(mnth), y = cnt), fill = 'grey')+
  stat_summary(fun.y = 'mean', geom = 'bar')

ggplot(df)+
  geom_histogram(aes(x=cnt, y=..density..),
                 fill = 'grey')+
  geom_density(aes(x=cnt, y=..density..))

# Visualize categorical variable 'holiday'
ggplot(df)+
  geom_bar(aes(x=holiday),fill='grey')
# it is showing that almost all the bike rentals are happening on holidays

# Visualize categorical variable 'weekday'
ggplot(df)+
  geom_bar(aes(x=weekday),fill='grey')
# It is showing that counts are same on all the weekdays

# Visualize categorical variable 'weathersit'
ggplot(df)+
  geom_bar(aes(x=weathersit),fill='grey')
# Count is varying according to the weather

##################################
# Bivariate relationship between numeric variables
# Cheq the relationship between temp and atemp variable

ggplot(df, aes(x=temp, y=atemp))+
  geom_point()+
  geom_smooth()
# This graph shows that there is a very strong relationship between temp and atemp

# Check the relationship between 'temp' and 'hum' variable
ggplot(df, aes(x=temp, y=hum))+
  geom_point()+
  geom_smooth()
# it shows that humidity increases till temprature 0.7 and it is decreasing gradually

# cheq relationship between 'temp' and 'windspeed' variable
ggplot(df, aes(x=temp, y=windspeed))+
  geom_point()+
  geom_smooth()
# it is showing that very less negative correlation between temp and windspeed

# Cheq the relationship between all numeric variables using pair plot
ggpairs(df[,c('atemp', 'temp', 'hum', 'windspeed', 'cnt')])
# this shows that above plot stating less negative relationship between 'cnt'-'hum' and 'cnt'-'windspeed'
# There is a stong positive relationship between temp-cnt and atemp-cnt

#################################################
# VISUALIZE the Relationship between Categorical Variable

# check relationship between season and holiday
rel_mnth_holi = table(df$season,df$holiday)
rel_mnth_holi
barplot(rel_mnth_holi)
# here contegency table shuowing holiday = 0 is same for almost all the seasons

# Cheq the relation ship between season and weekday
rel_cat_2 = table(df$season, df$weekday)
barplot(rel_cat_2)

# Cheq relationship between season ans weathersit
rel_cat_3 = table(df$weathersit, df$season)
rel_cat_3
prop.table(rel_cat_3, 2)
barplot(rel_cat_3)
# It is stating that all the seasons whether 1 type is large numbers

# Cheq relationship between holiday and weathersit
rel_cat_4 = table(df$weathersit, df$holiday)
rel_cat_4
# to check in proportions
prop.table(rel_cat_4)
barplot(rel_cat_4)
# it is stating that holiday type '0' and weathersit tyoe '1' is almost coveres 0.63%

######################## MISSING VALUE ANALYSIS ###################################

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$columns = row.names(missing_val)
missing_val

names(missing_val)[1] = "missing_percentage"

missing_val$missing_percentage = (missing_val$missing_percentage/nrow(df))*100
missing_val = missing_val[order(-missing_val$missing_percentage),]

row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val
# so no missing values are present in the data set

########################### OUTLIER ANALYSIS ###################################3

# detect outliers in 'actual', 'registered', and 'cnt' variables

ggplot(data = df, aes(x="", y=casual))+
  geom_boxplot()

# It is showing that there are a few outliers in casual variable

# boxplot for registered variable
ggplot(data = df, aes(x = "", y = registered))+
  geom_boxplot()
# there are no outliers in registered variable

# boxplot for cnt variable
ggplot(data = df, aes(x = "", y = cnt))+
  geom_boxplot()
# there are no outliers for cnt variable

#################### TREAT OUTLIERS ############################

# analyse relationship between casual and cnt variables before outline treatment
ggplot(df, aes(x = casual, y=cnt))+
  geom_point()+
  geom_smooth()
df_day_out = df

#Remove outliers using boxplot method
val = df_day_out$casual[df_day_out$casual %in% boxplot.stats(df_day_out$casual)$out]
df_day_out = df_day_out[which(!df_day_out$casual %in% val),]

# boxplot after removing outliers
# boxplot for casual variable
ggplot(data = df_day_out, aes(x = "", y = casual))+
  geom_boxplot()

# verify the relationship after outliers 
ggplot(df_day_out, aes(x=casual,y=cnt))+
  geom_point()+
  geom_smooth()

cor(df$casual,df$cnt)
cor(df_day_out$casual,df_day_out$cnt)
# there is a difference in correlation between casual and cnt before and after outlier detection
# there are also losign no of observations

################# FEATURE SELECTION or DIMENTION REDUCTION #######################

library(corrgram)

# verify correlation between numeric variables
corrgram(df[,c('temp', 'atemp', 'hum', 'windspeed', 'cnt')],order = F,
         upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")
# correlation matrix stating 'temp', and 'atemp' having strong relationship
# and there is no relation between 'hum' and 'cnt'

# Dimention Reduction

df_day_features = subset(df, select=-c(atemp,hum))

############################# NORMALITY CHECK ##################################

# Normalization
cnames = c("casual", "registered")
for(i in cnames){
  print(i)
  df_day_features[,i] = (df_day_features[,i] - min(df_day_features[,i]))/
   (max(df_day_features[,i] - min(df_day_features[,i])))
}
df$casual
df_day_features

################################### MODEL DEVELOPMENT ##################################

colnames(train)

feature_train_columns = c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "windspeed", "casual", "registered", "cnt")

# Divide data into train and test using stratified sampling method

set.seed(1234)
library(caret)
library(rpart)
train.index = createDataPartition(df_day_features$cnt, p = 0.80, list = FALSE)
train = df_day_features[train.index,]
test = df_day_features[-train.index,]

train_feature = train[,c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "windspeed", "casual", "registered", "cnt")]
train_feature

test_features = test[,c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "windspeed", "casual", "registered", "cnt")]
test_features

###################### Develop decision tree model########################################

# rpart for regression
fit = rpart(cnt ~., data = train_feature, method = "anova")

# Predict for new test cases
predictions_DT = predict(fit, test_features[,-12])
print(fit)

# plotting decision tree

par(cex = 0.8)
plot(fit)
text(fit)

############################### EVALUATE DECISION TREE ###################################

# MAPE
# Calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
MAPE(test_features[,12], predictions_DT)
# Error rate : 0.147
# Accuracy : 85.21

# Evaluate model using RMSE

RMSE = function(y_test, y_predict){
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
}
RMSE(test_features[,12], predictions_DT)
#RMSE : 566.54

############################ RANDOM FOREST ################################
library(randomForest)
Rental_rf = randomForest(cnt ~ ., data = train_feature)
Rental_rf

############## EVALUATE RANDOM FOREST ###################
# Predict for new test cases
predictions_DT_two = predict(Rental_rf, test_features[,-12])

MAPE(test_features[,12], predictions_DT_two)
# Erroe rate : 0.06
# Accuracy :92.2
RMSE (test_features[,12], predictions_DT_two)
# RMSE : 265

################### PARAMETER TUNING FOR RANDOM FOREST ########################

Rental_rf_2 = randomForest(cnt~., data = train_feature, mtry = 7, ntree = 500, nodesize = 10, importance = TRUE)

Rental_rf_2

# Predict for new test cases
predictions_RF_two = predict(Rental_rf_2, test_features[,-12])

MAPE(test_features[,12], predictions_RF_two)

# Error rate: 0.028
# Accuracy : 99.43

RMSE(test_features[,12], predictions_RF_two)
# RMSE : 131

# Cheq variable Importance

varImp = importance(Rental_rf_2)
varImp

# Sort Variable

sort_var = names(sort(varImp[,1], decreasing = T))
sort_var
varImpPlot(Rental_rf_2, type = 2)

############################ Tuning Random Forest Dimention Reduction ########################################

# Remove 4 variables which is contributing less
# 'season', 'weathersit', 'windspeed', 'holiday', and removing and developing the new model
train_feature_two = train[,c("yr", "mnth", "weekday", "workingday", "temp", "casual", "registered", "cnt")]
test_features_two = test[,c("yr", "mnth", "weekday", "workingday", "temp", "casual", "registered", "cnt")]

# Develop Random Forest model
Rental_rf_3 = randomForest(cnt~., data = train_feature_two, mtry = 7, ntree = 500, nodesize = 10, importance = TRUE)
Rental_rf_3

# Predict for new test cases
predictions_RF_three = predict(Rental_rf_3, test_features_two[,-8])

MAPE(test_features_two[,8], predictions_RF_three)
# Error Rate : 0.022
# Accuracy : 97.8

RMSE(test_features_two[,8], predictions_RF_three)
# RMSE : 123.8

#################################### Develop Linear Regression Model #############################################

# check multicollinearity
install.packages('usdm')
library(usdm)
vif(train_feature[,-12])

vifcor(train_feature[,-12], th = 0.9)

# Correlation between two variables is 'season' and 'mnth' is 0.82 so, removing one variable from the model

train_feature_three = train[,c("yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "windspeed", "casual", "registered", "cnt")]
test_features_three = test[,c("yr", "mnth", "holiday", "weekday", "workingday", "weathersit", "temp", "windspeed", "casual", "registered", "cnt")]

# develop Linear Regression Model
# Run regression model

lm_model = lm(cnt~., data = train_feature_three)
# Summary of the model
summary(lm_model)

# observe the residuals and coefficients of the linear regression model
# Predict the test daa
# Predict
predictions_LR = predict(lm_model, test_features_three[-11])

# Evaluate Linear Regression
MAPE(test_features_three[,11], predictions_LR)
# Error Rate : 6.413e-16
# Accuracy : 00.9

RMSE(test_features_three[,11], predictions_LR)
# RMSE = 4.09e-16

# Conclusion for this dataset Linear Regression is accuracy is 99.9
# and RMSE = 4.09e-16


