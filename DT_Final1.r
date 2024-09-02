# title: "Decision Tree For Diabetics prediction"
# author: "Anas"
# date: "7/14/2024"
# output: html_document


pacman::p_load(tidyverse, GGally, gtools, skimr, mice, caret, pROC, class,car,CMplot, caret, ggplot2, caTools)


# Load the Diabetics prediction dataset.
diab <- read_csv("C:\\Users\\Anas\\Documents\\Workshops\\diabetes.csv")

# Convert the categorical data into numeric categories.
diab$hypertension <- as.numeric(as.factor(diab$hypertension))
diab$heart_disease  <- as.numeric(as.factor(diab$heart_disease))
diab$gender <- as.numeric(as.factor(diab$gender))
diab$smoking_history <- as.numeric(as.factor(diab$smoking_history))
#diab$diabetes <- as.numeric(as.factor(diab$diabetes))



# diab_bxplot <- diab[, c(2,6,7,8)]
# diab2 <- diab[,-c(1,3,4,5)]
# boxplot(diab_bxplot, las = 2, varwidth = T)
# ggcorr(diab, label = T)



## Correlation plot - for correlation structure of the variables.
# library(corrplot)
# cordiabetes <- cor(diab) #Getting the correlation matrix between variables
# corrplot(cordiabetes,
#         method = "color",
#         order = "hclust",
#         addCoef.col = "black",
#         number.cex = .6) # Visualizing the correlation matrix to identify patterns between variables.

# Modeling.
## Train and test data set splitting.

# We will split the dataset by 70/30 in accordance to our target proportion.
install.packages("parallelly")
library(rsample)
set.seed(100)


sample = sample.split(diab$bmi, SplitRatio = .7)
diab.train <- subset(diab, sample == TRUE)
diab.test  <- subset(diab, sample == FALSE)
diab.test_step  <- subset(diab, sample == FALSE)


# Decision tree - Training using cross validation approach
# --------------------------------------------------------
#library(rpart.plot)
set.seed(12345)
install.packages("caret")
library(caret)
diab.train$diabetes <-as.factor(diab.train$diabetes)

#trctrl <- trainControl(method = "cv", number = 10)
 trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

tree1 <- train(diabetes ~ ., method = "rpart", trControl = trctrl,
               tuneLength = 5, data = diab.train)
#rpart.plot(tree1$finalModel)

## Decision Trees prediction results
pred <- predict(tree1, newdata = diab.test)
test_y      <- (as.factor(diab.test$diabetes))

DT1 <- confusionMatrix(data = test_y,reference = pred, positive = "1")
DT1


# Model Interpretation
#summary(tree1)
tree1$results


# -------------------------------------------------------
# Decision tree on balanced data and testing on test data
# -------------------------------------------------------
#library(rpart.plot)
library(caret)
set.seed(12345)

train.balanced <- smote(diabetes ~ ., data = diab, perc.over = 1)

library(rsample)
set.seed(100)
sample = sample.split(train.balanced$bmi, SplitRatio = .7)
train <- subset(train.balanced, sample == TRUE)
test  <- subset(train.balanced, sample == FALSE)
dim(train)
dim(test)


trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Remove NA from train and test data.
train1 <- (train[complete.cases(train),])
test1 <- (test[complete.cases(test),])

train1$diabetes <- as.factor(train1$diabetes)
test1$diabetes <- as.factor(test1$diabetes)


tree1<- train(diabetes ~., method="rpart",trControl=trctrl,tuneLength=5,data = train1)

pred <- predict(tree1, newdata = test1)
#test_y  <- (as.factor(test1$diabetes))

library(plyr)

# Create a factor variable

test_y <- revalue(test1$diabetes, c("2" = "1", "1" = "0"))

test_y <- test1$diabetes

levels(test_y)
levels(pred)

# Confusion matrix for test data
DT1 <- confusionMatrix(data = test_y ,reference = pred, positive = "1")
print('The confusion matrix on balanced data is as follows')
DT1
