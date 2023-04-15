library(ggplot2)
library(glmnet)
library(caret)
library(fastDummies)
library(cowplot)
library(ROCR)
library(e1071)
library(ROSE)

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
barplot(table_prop, beside = TRUE, legend = TRUE, col = c("lightblue", "pink"))


# Categorical variables into numerical factors
heartD$HeartDisease <-     as.factor(heartD$HeartDisease)
heartD$Smoking <-          as.factor(heartD$Smoking)
heartD$AlcoholDrinking <-  as.factor(heartD$AlcoholDrinking)
heartD$Stroke <-           as.factor(heartD$Stroke)
heartD$DiffWalking <-      as.factor(heartD$DiffWalking)
heartD$Sex <-              as.factor(heartD$Sex)
heartD$AgeCategory <-      as.factor(heartD$AgeCategory)
heartD$Race <-             as.factor(heartD$Race)
heartD$Diabetic <-         as.factor(heartD$Diabetic)
heartD$PhysicalActivity <- as.factor(heartD$PhysicalActivity)
heartD$GenHealth <-        as.factor(heartD$GenHealth)
heartD$Asthma <-           as.factor(heartD$Asthma)
heartD$KidneyDisease <-    as.factor(heartD$KidneyDisease)
heartD$SkinCancer <-       as.factor(heartD$SkinCancer)

# Split Data into train and Test Datasets.
set.seed(5420)
training.samples <- createDataPartition( heartD$HeartDisease , p = 0.8, list = FALSE)
train <- heartD[training.samples,]
test <- heartD[-training.samples,]
# balance train data set
train <- ROSE(HeartDisease ~ ., data = train)$data

# Implement SVM
svm.model = svm(HeartDisease ~ ., data=train ,kernel = "radial", cost = 10 )


# SVM Prediction on Test Data set
y_pred <- predict(svm.model, newdata = test, type = "response")

# Evaluate Metric on Test Data set
## 1-Confusion Matrix
acc <- confusionMatrix(as.factor(y_pred),test$HeartDisease)
acc
## 2-Accuracy
acc$overall[1]
## 3-ROC
#  3.1-Create a prediction object
prediction <- prediction(as.integer(y_pred), test$HeartDisease)

#  3.2-Calculate the performance measures
performance.test <- performance(prediction, "tpr", "fpr")

#  3.3-Plot the AUC-ROC curve
plot(performance.test, main = "AUC-ROC Curve", colorize = TRUE)

################################################################################
# SVM Prediction on Train Data set

y_pred2 <- predict(svm.model, newdata = train, type = "response")

# Evaluate Metric on Train Data set
## 1-Confusion Matrix
acc <- confusionMatrix(as.factor(y_pred2),train$HeartDisease)
acc
## 2-Accuracy
acc$overall[1]

