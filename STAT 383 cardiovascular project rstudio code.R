# This code is for the analysis of cardiovascular disease data based on 
# logistical regression analysis.

setwd("/Users/mjyak/Documents/STAT 383 Project")
cat("\f") # clean the console
rm(list = ls()) # clean the environment
library(readxl)
library(pROC)
library(ggplot2)

# Imports the Heart Attack risk data
Data <- read_excel("cardio_train.xlsx")

# deleting unreasonable values from ap_hi
Data <- subset(Data, ap_hi > 0)
Data <- subset(Data, ap_hi < 241)

# deleting unreasonable values from ap_lo
Data <- subset(Data, ap_lo > 0)
Data <- subset(Data, ap_lo < 191)

# deleting unreasonable values from height
Data <- subset(Data, height > 100)
Data <- subset(Data, height < 208)

# splitting data into male and female 
male_data <- Data[Data$gender == 2, ]
female_data <- Data[Data$gender == 1, ]

male_data$w_h_ratio <- male_data$weight/male_data$height
female_data$w_h_ratio <- female_data$weight/female_data$height


################### Descriptive Statistical Analysis of Variables #####################
# Calculating means, standard deviations, minimum, and maximum values for all variables
desc_male_data <- data.frame(mean_value = colMeans(male_data), std_dev = apply(male_data,2,sd), 
  min = apply(male_data, 2, min), max = apply(male_data, 2, max))
desc_female_data <- data.frame(mean_value = colMeans(female_data), std_dev = apply(female_data,2,sd), 
  min = apply(female_data, 2, min), max = apply(female_data, 2, max))
#######################################################################################


################################# Getting ROC curve ###################################
# Splitting data into test and train data
set.seed(1)
male_sample <- sample(c(TRUE, FALSE), nrow(male_data), replace=TRUE, prob=c(0.7,0.3))
female_sample <- sample(c(TRUE, FALSE), nrow(female_data), replace=TRUE, prob=c(0.7,0.3))
male_train <- male_data[male_sample, ]
male_test <- male_data[!male_sample, ]
female_train <- female_data[female_sample, ]
female_test <- female_data[!female_sample, ]

#Logistical Regression Model for cardiovascular disease using training data
male_model <- glm(cardio ~ age + w_h_ratio + ap_hi + ap_lo + cholesterol + gluc + 
  smoke + alco + active, family = "binomial", data = male_train)
female_model <- glm(cardio ~ age + w_h_ratio + ap_hi + ap_lo + cholesterol + gluc + 
  smoke + alco + active, family = "binomial", data = female_train)

# Prediction 
male_predicted <- predict(male_model, male_test, type = "response")
female_predicted <- predict(female_model, female_test, type = "response")

#define object to plot and calculate AUC
male_rocobj <- roc(male_test$cardio, male_predicted)
male_auc <- round(auc(male_test$cardio, male_predicted),4)
female_rocobj <- roc(female_test$cardio, female_predicted)
female_auc <- round(auc(female_test$cardio, female_predicted),4)

#create ROC plot with minimal theme
ggroc(male_rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Male ROC Curve ', '(AUC = ', male_auc, ')')) +
  theme_minimal()
ggroc(female_rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Female ROC Curve ', '(AUC = ', female_auc, ')')) +
  theme_minimal()
######################################################################################



################# Accuracy of the training models using all the data #################
male_data$predicted_prob <- ifelse(predict(male_model, male_data, type = "response") > 0.5, 1, 0)
fullmale_correct_predictions <- sum(male_data$predicted_prob == male_data$cardio)
fullmale_total_predictions <- nrow(male_data)
fullmale_accuracy <- fullmale_correct_predictions / fullmale_total_predictions
cat("Accuracy of the male_model:", fullmale_accuracy, "\n")

female_data$predicted_prob <- ifelse(predict(female_model, female_data, type = "response") > 0.5, 1, 0)
fullfemale_correct_predictions <- sum(female_data$predicted_prob == female_data$cardio)
fullfemale_total_predictions <- nrow(female_data)
fullfemale_accuracy <- fullfemale_correct_predictions / fullfemale_total_predictions
cat("Accuracy of the female_model:", fullfemale_accuracy, "\n")
#######################################################################################



##################### Logistic Regression for male and female data ####################
# logistic regression models for all male and female data
fullmale_model <- glm(cardio ~ age + w_h_ratio + ap_hi + ap_lo + cholesterol + 
  gluc + smoke + alco + active, family = "binomial", data = male_data)
fullfemale_model <- glm(cardio ~ age + w_h_ratio + ap_hi + ap_lo + cholesterol + 
  gluc + smoke + alco + active, family = "binomial", data = female_data)

summary(fullmale_model)
summary(fullfemale_model)

#plots logistic regression curves for the male data
ggplot(male_data, aes(x=age/365, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  ggtitle('Male Cardio vs. Age Regression Curve') + 
  theme(plot.title = element_text(hjust=0.5, size=11)) + 
  labs(x='age (years)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=w_h_ratio, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Weight/Height Ratio Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='weight/height ratio (kg/cm)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=ap_hi, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Systolic BP Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='systolic blood pressure (mm Hg)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=ap_lo, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Diastolic BP Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='diastolic blood pressure (mm Hg)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=cholesterol, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Cholesterol Level Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='cholesterol level', y='cardiovascular disease probability')

ggplot(male_data, aes(x=gluc, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Glucose Level Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='glucose level', y='cardiovascular disease probability')

ggplot(male_data, aes(x=smoke, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Smkoking Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11))+
  labs(x='smoker (yes/no)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=alco, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Alcohol Consumption Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='alcohol consumption (yes/no)', y='cardiovascular disease probability')

ggplot(male_data, aes(x=active, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Male Cardio vs. Physical Activity Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='physical activity (yes/no)', y='cardiovascular disease probability')

# plots logistic regression curves for the female data
ggplot(female_data, aes(x=age/365, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Age Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='age (years)', y='cardiovascular disease probability')
  
ggplot(female_data, aes(x=w_h_ratio, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Weight/Height Ratio Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='weight/height (kg/cm)', y='cardiovascular disease probability')

ggplot(female_data, aes(x=ap_hi, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Systolic BP Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='systolic blood pressure (mm Hg)', y='cardiovascular disease probability')

ggplot(female_data, aes(x=ap_lo, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Diastolic BP Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='diastolic blood pressure (mm Hg)', y='cardiovascular disease probability')

ggplot(female_data, aes(x=cholesterol, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Cholesterol Level Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='cholesterol level', y='cardiovascular disease probability')

ggplot(female_data, aes(x=gluc, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Glucose Level Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='glucose level', y='cardiovascular disease probability')

ggplot(female_data, aes(x=smoke, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Smoking Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) +
  labs(x='smoker (yes/no)', y='cardiovascular disease probability')

ggplot(female_data, aes(x=alco, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Alcohol Consumption Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=10.5)) +
  labs(x='alcohol consumption (yes/no)', y='cardiovascular disease probability')

ggplot(female_data, aes(x=active, y=cardio)) + geom_point(alpha=.5) + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle('Female Cardio vs. Physical Activity Regression Curve') +
  theme(plot.title = element_text(hjust=0.5, size=11)) + 
  labs(x='physical activity (yes/no)', y='cardiovascular disease probability')
#########################################################################################
