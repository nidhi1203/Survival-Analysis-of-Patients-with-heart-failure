#Loading Packages
library(dplyr)
library(ggplot2)
library(cowplot)
library(regclass)
library(caret)
library(e1071)
library(pROC)

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

#Transforming data (Converting 0,1's to meaningful form)

data <- rawdata %>% 
  mutate(anaemia = ifelse(anaemia ==1, "Yes", "No"),
         high_blood_pressure = ifelse(high_blood_pressure ==1, "Yes", "No"),
         diabetes = ifelse(diabetes ==1, "Yes", "No"),
         smoking =ifelse(smoking ==1,"Yes","No"),
         DEATH_EVENT=ifelse(DEATH_EVENT=="No Death", "Survived", "Death")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(age, anaemia, creatinine_phosphokinase, diabetes, ejection_fraction, high_blood_pressure, platelets,serum_creatinine, serum_sodium, sex, smoking, time, DEATH_EVENT)

View(data)
str(data)
summary(data)

dataset<-data
attach(dataset)

#Simple logistic model 1
#Implementing simple logistic model using our independent variable sex to predict patient's death or survival
xtabs(~ DEATH_EVENT + sex, data=dataset)

logistic_simple <- glm(DEATH_EVENT ~ sex, data=dataset, family="binomial")
summary(logistic_simple)

#The intercept is the log(odds) a female that can survive.
#sexmale is the log(odds ratio) that tells us if a sample has sex=male, the odds of survival are on a log scale 0.019 times lesser than if a sample has sex=female
#However, sex is not a significant predictor for death event

#Calculating the overall "Pseudo R-squared" and its p-value
ll.null <- logistic_simple$null.deviance/-2
ll.proposed <- logistic_simple$deviance/-2
ll.null
ll.proposed
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic_simple$null.deviance - logistic_simple$deviance), df=1)

#Checking what logistic regression is predicting, given that a patient is either female or male (and no other data about them).
predicted.data <- data.frame(probability.of.death_event=logistic_simple$fitted.values,sex=dataset$sex)
predicted.data

## There are only two probabilities (one for females and one for males),so we can summarize the predicted probabilities.
xtabs(~ probability.of.death_event + sex, data=predicted.data)

#Simple logistic model 2
#Implementing simple logistic model using our independent variable age to predict patient's death or survival
logistic_simple2 <- glm(DEATH_EVENT ~ age, data=dataset, family="binomial")
summary(logistic_simple2)

#Age is a significant predictor for death event
#For every one year increase in Age the odds of survival on a log scale is expected to increase by 0.047

#Calculating the overall "Pseudo R-squared" and its p-value
ll.null_age <- logistic_simple2$null.deviance/-2
ll.proposed_age <- logistic_simple2$deviance/-2
ll.null_age
ll.proposed_age
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null_age - ll.proposed_age) / ll.null_age
## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed_age - ll.null_age), df=1)
1 - pchisq((logistic_simple2$null.deviance - logistic_simple2$deviance), df=1)

#Checking what logistic regression is predicting with Age as the predictor
predicted.data_age <- data.frame(probability.of.death_event=logistic_simple2$fitted.values,age=dataset$age)
predicted.data_age


#Implementing multiple logistic model using all the variables
logistic <- glm(DEATH_EVENT ~ ., data=dataset, family="binomial")
summary(logistic)

#Based on the summary, we can say age, ejection_fraction, serum_creatinine and time are significant variables for predicting death event
#For every one year increase in Age the odds of survival on a log scale is expected to increase by 0.047, holding all the other variables constant
#For every one percent increase in ejection_fraction the odds of survival on a log scale is expected to decrease by 0.076, holding all the other variables constant
#For every one mg/dL increase in serum_creatinine the odds of survival on a log scale is expected to increase by 0.66, holding all the other variables constant
#For every one day increase in follow-up period the odds of survival on a log scale is expected to decrease by 0.021, holding all the other variables constant 

#Calculating the overall "Pseudo R-squared" and its p-value
ll.null2 <- logistic$null.deviance/-2
ll.proposed2 <- logistic$deviance/-2
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null2 - ll.proposed2) / ll.null2
## The p-value for the R^2
1 - pchisq(2*(ll.proposed2 - ll.null2), df=(length(logistic$coefficients)-1))

#Plot the data
predicted.data2 <- data.frame(probability.of.death_event=logistic$fitted.values,DEATH_EVENT=dataset$DEATH_EVENT)
predicted.data2 <- predicted.data2[order(predicted.data2$probability.of.death_event, decreasing=FALSE),]
predicted.data2$rank <- 1:nrow(predicted.data2)
ggplot(data=predicted.data2, aes(x=rank, y=probability.of.death_event)) +
  geom_point(aes(color=DEATH_EVENT), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of Survival")

#Confusion Matrix
confusion_matrix(logistic)

pdata <- predict(logistic,newdata=dataset,type="response" )
pdata
dataset$DEATH_EVENT
pdataF <- as.factor(ifelse(test=as.numeric(pdata>0.5) == 0, yes="Death", no="Survived"))

confusionMatrix(pdataF, dataset$DEATH_EVENT)

#ROC curve
roc(dataset$DEATH_EVENT,logistic$fitted.values,plot=TRUE)
par(pty = "s")
roc(dataset$DEATH_EVENT,logistic$fitted.values,plot=TRUE)
roc(dataset$DEATH_EVENT,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)

roc.info <- roc(dataset$DEATH_EVENT, logistic$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
                     fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
                     thresholds=roc.info$thresholds)
roc.df
head(roc.df)
tail(roc.df) 

#Setting thresholds between TPP 60% and 80%
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(dataset$DEATH_EVENT,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822", print.auc.x=45)

# Plotting and comparing two ROC's (one using Age as single predictor and other using all variables as predictors) 
roc(dataset$DEATH_EVENT, logistic_simple2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
plot.roc(dataset$DEATH_EVENT, logistic$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Simple", "Non Simple"), col=c("#377eb8", "#4daf4a"), lwd=4)

#Based on the plot, we can say the model using all variables as predictors is better