library(ggplot2)
library(glmnet)
library(caret)
library(fastDummies)
library(cowplot)
library(ROCR)

# Load data set
heartD <- read.csv('heart_2020_cleaned.csv')
names(heartD)
dim(heartD)
str(heartD)
# Histogram of Numeric Data
h1 <- ggplot(data = heartD, aes(x = BMI))           +geom_histogram()
h2 <- ggplot(data = heartD, aes(x = PhysicalHealth))+geom_histogram()
h3 <- ggplot(data = heartD, aes(x = MentalHealth))  +geom_histogram()
h4 <- ggplot(data = heartD, aes(x = SleepTime))     +geom_histogram()
plot_grid(h1,h2,h3,h4,ncol = 2)
# bar plot for depicting the proportion of data indicating heart disease. 
table_=table(heartD$HeartDisease)
table_prop <- prop.table(table_) * 100
barplot(table_prop, beside = TRUE, col = c("lightblue", "pink"),
        main = "Histogram of Percentage Heart Disease ", ylab = "Percentage",
        xlab = "Heart Disease")

# Categorical variables into numerical factors
heartD$HeartDisease <- factor(heartD$HeartDisease, 
                              levels = c("No", "Yes"), 
                              labels = c(0, 1))
heartD$Smoking <- factor(heartD$Smoking, 
                         levels = c("Yes", "No"), 
                         labels = c(1, 2))
heartD$AlcoholDrinking <- factor(heartD$AlcoholDrinking, 
                                 levels = c("Yes", "No"), 
                                 labels = c(1, 2))
heartD$Stroke <- factor(heartD$Stroke, 
                        levels = c("Yes", "No"), 
                        labels = c(1, 2))
heartD$DiffWalking <- factor(heartD$DiffWalking, 
                             levels = c("Yes", "No"), 
                             labels = c(1, 2))
heartD$Sex <- factor(heartD$Sex, 
                     levels = c("Male", "Female"), 
                     labels = c(1, 2))
heartD$AgeCategory <- factor(heartD$AgeCategory, 
                             levels = c("18-24", "25-29", "30-34", "35-39", "40-44",
                                        "45-49", "50-54", "55-59", "60-64",
                                        "65-69", "70-74", "75-79", "80 or older"), 
                             labels = c(21, 27, 32, 37, 42, 47, 52, 57, 62, 67,
                                        72, 77, 80))
heartD$PhysicalActivity <- factor(heartD$PhysicalActivity, 
                                  levels = c("Yes", "No"), 
                                  labels = c(1, 2))
heartD$Asthma <- factor(heartD$Asthma, 
                        levels = c("Yes", "No"), 
                        labels = c(1, 2))
heartD$KidneyDisease <- factor(heartD$KidneyDisease, 
                               levels = c("Yes", "No"), 
                               labels = c(1, 2))
heartD$SkinCancer <- factor(heartD$SkinCancer, 
                            levels = c("Yes", "No"), 
                            labels = c(1, 2))

# Normalized numeric Variable 
heartD$BMI <- scale(heartD$BMI, center = TRUE, scale = TRUE)
heartD$PhysicalHealth <- scale(heartD$PhysicalHealth, center = TRUE, scale = TRUE)
heartD$MentalHealth <- scale(heartD$MentalHealth, center = TRUE, scale = TRUE)
heartD$SleepTime <- scale(heartD$SleepTime, center = TRUE, scale = TRUE)
heartD$AgeCategory <- scale(as.numeric(heartD$AgeCategory), center = TRUE, scale = TRUE)
# Change the name of some categorical Variables.to ignore predictors in glm() function should not be space in their names.
heartD$Race[heartD$Race=="American Indian/Alaskan Native"] <- "AmericanIndianAlaskan"
heartD$Diabetic[heartD$Diabetic=="No, borderline diabetes"] <- "No_borderline_diabetes"
heartD$Diabetic[heartD$Diabetic=="Yes (during pregnancy)"] <- "Yes_during_pregnancy"
heartD$GenHealth[heartD$GenHealth=="Very good"] <- "Very_good"
# Create Dummy Variable
heartD_ <- dummy_cols(heartD, select_columns = c('Race', 'Diabetic', 'GenHealth'), remove_selected_columns = TRUE)

# Split Data into train and Test Datasets.
set.seed(5420)
training.samples <- createDataPartition( heartD_$HeartDisease , p = 0.8, list = FALSE)
train <- heartD_[training.samples,]
test <- heartD_[-training.samples,]

# Implement GLM
glm.model <- glm(HeartDisease ~ . -PhysicalActivity 
                 -Race_Other -Race_AmericanIndianAlaskan
                 -Race_White -Diabetic_No -Diabetic_No_borderline_diabetes
                 -Diabetic_Yes_during_pregnancy -GenHealth_Very_good,
                 family = binomial,data = train)

# GLM Prediction on Test Data set
y_pred <- predict(glm.model, newdata = test, type = "response")
y_pred2<- ifelse(y_pred>0.5,1,0)

# Evaluate Metric on Test Data set
## 1-Confusion Matrix
acc <- confusionMatrix(as.factor(y_pred2),test$HeartDisease)
acc
## 2-Accuracy
acc$overall[1]
## 3-ROC
#  3.1-Create a prediction object
prediction <- prediction(y_pred, test$HeartDisease)

#  3.2-Calculate the performance measures
performance.test <- performance(prediction, "tpr", "fpr")

# 3.3-Plot the AUC-ROC curve
plot(performance.test, main = "AUC-ROC Curve", colorize = TRUE)

################################################################################
# GLM Prediction on Train Data set
# Evaluate Metric on Train Data set
## 1-Confusion Matrix
y_fitted<- ifelse(glm.model$fitted.values>0.5,1,0)
acc <- confusionMatrix(as.factor(y_fitted),train$HeartDisease)
acc
## 2-Accuracy
acc$overall[1]

