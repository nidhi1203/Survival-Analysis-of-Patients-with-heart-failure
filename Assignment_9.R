#Loading Packages
library(dplyr)
library(psych)
library(cluster)
library(fpc)
library(ggplot2)
library(MASS)
library(ROCR)
library(klaR)

#Loading dataset
rawdata <-read.csv("C:/Users/nidhi/OneDrive/Desktop/MVA/heart_failure_clinical_records_dataset.csv")
View(rawdata)

#Identifying different columns names
names(rawdata)

#Data Summary
str(rawdata)
summary(rawdata)
head(rawdata)
dim(rawdata)

#Data Cleaning

#Checking for missing values
is.null(rawdata)
##The "FALSE" output shows there is no missing data in the dataset.


data <- as.matrix(rawdata[,c(1,3,5,7,8,9,12)])
dim(rawdata)
dim(data)
dataset <- cbind(data, as.numeric(rawdata$DEATH_EVENT)-1)
dim(dataset)
colnames(dataset)[8] <- "death"

View(dataset)
str(dataset)

# Lets cut the data into two parts
smp_size_raw <- floor(0.75 * nrow(dataset))
train_ind_raw <- sample(nrow(dataset), size = smp_size_raw)
train_raw.df <- as.data.frame(dataset[train_ind_raw, ])
test_raw.df <- as.data.frame(dataset[-train_ind_raw, ])
# We have a training and a test set. Training is 75% and test is 25%

dataset.lda <- lda(formula = train_raw.df$death ~ ., data = train_raw.df)
dataset.lda
summary(dataset.lda)
print(dataset.lda)
plot(dataset.lda)
dataset.lda.predict <- predict(dataset.lda, newdata = test_raw.df)
dataset.lda.predict$class
View(dataset.lda.predict)
dataset.lda.predict$x

# Get the posteriors as a dataframe.
dataset.lda.predict.posteriors <- as.data.frame(dataset.lda.predict$posterior)

#create ROC/AUC curve
pred <- prediction(dataset.lda.predict.posteriors[,2], test_raw.df$death)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Since the AUC value 0.82 is close to 1.0, we can say, it is a good model

summary(dataset.lda)

(prop = dataset.lda$svd^2/sum(dataset.lda$svd^2))
#we can use the singular values to compute the amount of the between-group variance that is explained by each linear discriminant. 
#We see that the first linear discriminant (dataset.lda) explains almost all of the between-group variance in the dataset

data2<- as.data.frame(dataset)
View(data2)
dataset.lda2 <- lda(formula = death ~ ., data = data2, CV = TRUE)
dataset.lda2
head(dataset.lda2$class)

#the Maximum a Posteriori Probability (MAP) classification (a factor)
#posterior: posterior probabilities for the classes.
head(dataset.lda2$posterior, 3)

#Dividing the data into two parts 50% each
train <- sample(1:299, 150)

# training model
dataset.lda3 <- lda(death ~ ., 
          data2,
          prior = c(1,1)/2,
          subset = train)

# predictions
plda = predict(object = dataset.lda3, 
               newdata = data2[-train, ])
head(plda$class)

# posterior prob
head(plda$posterior, 6) 
head(plda$x, 3)
plot(dataset.lda)
plot(dataset.lda3)

dataset.lda <- lda(death ~ .,
         data2,
         prior = c(1,1)/2)
prop.lda = dataset.lda$svd^2/sum(dataset.lda$svd^2)
plda <- predict(object = dataset.lda,
                newdata = data2)
data3 = data.frame(death = data2[,"death"],lda = plda$x)


# Dividing the dataset using second approach

set.seed(101) # Nothing is random!!
sample_n(data2,10)

# Divinding dataset into 75/25 using Dplyr preserves class 
training_sample <- sample(c(TRUE, FALSE), nrow(data2), replace = T, prob = c(0.75,0.25))
train <- data2[training_sample, ]
test <- data2[!training_sample, ]

#lets run LDA like before
lda.data2 <- lda(death ~ ., train)

# Plotting the data to better understand the model
plot(lda.data2, col = as.integer(train$death))
plot(lda.data2, dimen = 1, type = "b")

View(data2)
# Partition plots
partimat(death ~ age + creatinine_phosphokinase + ejection_fraction + platelets + serum_creatinine + serum_sodium + time, data=train, method="lda")

# Checking the accuracy for training set to understand how good the model is
lda.train <- predict(lda.data2)
train$lda <- lda.train$class
table(train$lda,train$death)

#Checking the accuracy for testing set to understand how good the model is
lda.test <- predict(lda.data2,test)
test$lda <- lda.test$class
table(test$lda,test$death)