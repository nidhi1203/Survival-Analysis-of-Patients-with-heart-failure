#Loading Packages
library(dplyr)
library(GGally)
library(car)
library(MASS)
library(gvlma)
library(leaps)
library(relaimpo)

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
  mutate(DEATH_EVENT=ifelse(DEATH_EVENT=="Death",0,1),
         sex=ifelse(sex=="male",1,0)
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(age, anaemia, creatinine_phosphokinase, diabetes, ejection_fraction, high_blood_pressure, platelets,serum_creatinine, serum_sodium, sex, smoking, time, DEATH_EVENT)

View(data)
summary(data)

dataset<-data
attach(dataset)

#Performing Multiple Regression on our dataset
fit <- lm(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking+time, data=dataset)
#show the results
summary(fit)

#Summary has three sections 
#Section1: How well does the model fit the data 
#Based on Adjusted R-squared value (0.3924), our model can explain 39.24% of variation in Death_Event(Survived or not survived)

#Section2: Is the hypothesis supported?  
#Based on the p-value of each independent variable we found age, ejection_fraction, serum_creatinine and time have significant impact for predicting Death_Event at significance level of 0.05
#Age has significantly positive impact on Death_Event which means elder patients have higher chance of survival holding other independent variables constant, which is against our hypothesis
#Ejection_fraction has significantly negative impact on Death_Event which means patients with lower ejection fraction have higher chance of survival holding other independent variables constant, which supports our hypothesis
#Serum_creatinine has significantly positive impact on Death_Event which means patients with higher level of Serum_creatinine have a higher chance of survival holding other independent variables constant, which is against our hypothesis
#Time has significantly negative impact on Death_Event which means patients who have closer follow-up period have higher chance of survival holding other independent variables constant
#For other independent variables, no significant impact is found for Death_Event

#Section3: How well does data fit the model
#Based on F-statistic and its p-value, since p-value is smaller than 0.05, we can say our model has significant predictive power

#Plotting 
ggpairs(data=dataset, title="Heart Disease")

confint(fit,level=0.95)

# Predicted Values and residuals
fitted(fit)
residuals(fit)

#Anova Table
anova(fit)
#Based on p-values, age,creatinine_phosphokinase, ejection_fraction, serum_creatinine, serum_sodium and time variables have significant mean difference between Survived and Not survived patients at significance level of 0.05

#Covariance
vcov(fit)

#Correlation
cov2cor(vcov(fit))

#diagnostic plots
plot(fit)

# Checking the existence of Outliers
outlierTest(fit)
#Since p-value is smaller than 0.05, we can say there are some outliers

#Plotting the data, to identify outliers
qqPlot(fit, main="QQ Plot")
#Based on the plot, we can say record 187 and 196 are identified as outliers 

#Leverage plots
leveragePlots(fit)

# added variable plots
avPlots(fit)

# Cook's Distance plot
cutoff <- 4/((nrow(dataset)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
#Based on Cook's Distance plot, records 10, 132 and 218 have negative effect on our regression model

# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Based on the plot, we can identify records 10,187,196,218 as outliers

# distribution of studentized residuals
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#Based on the plot, we can see the Studentized residuals are almost normally distributed

#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
#Based on the p-value, since it is smaller than 0.05, we have the problem of heteroscedasticity

# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

#Multi-collinearity
# Evaluate Collinearity

# variance inflation factors(VIF)
vif(fit) 
#Based on VIF values, we don't have a problem of multi-collinearity as they have measure less than 5

#Using the cutoff value as 2 for checking multi-collinearity 
sqrt(vif(fit)) > 2 
#Again, we have no problem of multi-collinearity

#Nonlinearity
# component + residual plot
crPlots(fit)

#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)
#Since p-value is smaller than 0.05, we may have the problem of Autocorrelated errors

#Global test of model assumptions
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(DEATH_EVENT~age+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking+time, data=dataset)

# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova
#Based on the output, we can say the final model has variables age, creatinine_phosphokinase, ejection_fraction, serum_creatinine, serum_sodium, sex, and time included

leaps<-regsubsets(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking+time, data=dataset,nbest=10)
summary(leaps)
plot(leaps)
plot(leaps,scale="r2")
coef(leaps,1:5)

# Calculate Relative Importance for Each Predictor
calc.relimp(fit,type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (100 samples)
boot <- boot.relimp(fit, b = 100, type = c("lmg","last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot)
plot(booteval.relimp(boot,sort=TRUE))

summary(fit)
