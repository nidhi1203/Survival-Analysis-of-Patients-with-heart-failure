#Installing and Loading Packages
#install.packages(c("tidyverse", "ggplot2", "ggthemes", "RColorBrewer", "gridExtra", "kableExtra", "data.table", "dplyr", "corrplot"))
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
library(kableExtra)
library(data.table)
library(dplyr)
library(corrplot)


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

dataset <- rawdata %>% 
  mutate(anaemia = ifelse(anaemia ==1, "Yes", "No"),
         high_blood_pressure = ifelse(high_blood_pressure ==1, "Yes", "No"),
         diabetes = ifelse(diabetes ==1, "Yes", "No"),
         smoking =ifelse(smoking ==1,"Yes","No"),
         DEATH_EVENT=ifelse(DEATH_EVENT=="No Death", "Survived", "Death")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(age, anaemia, creatinine_phosphokinase, diabetes, ejection_fraction, high_blood_pressure, platelets,serum_creatinine, serum_sodium, sex, smoking, time, DEATH_EVENT)

View(dataset)
summary(dataset)

#Understanding how Age affects the Death event
a<-ggplot(dataset,aes(x = age))+geom_histogram(binwidth = 5, color = "white", fill = "grey",alpha = 0.5)+theme_fivethirtyeight()+labs(title = "Age Distribution", caption = "i. Age Distribution")+
  theme(plot.caption = element_text(hjust = 0.5,face = "italic"))+
  scale_x_continuous(breaks = seq(40,100,10))

b<-ggplot(dataset,aes(x = age, fill = DEATH_EVENT))+geom_histogram(binwidth = 5, position = "identity",alpha = 0.5,color = "white")+theme_fivethirtyeight()+scale_fill_manual(values = c("#999999", "#E69F00"))+
  labs(caption = "ii. Age Distribution with Death Event")+
  theme(plot.caption = element_text(hjust = 0.5,face = "italic"))+
  scale_x_continuous(breaks = seq(40,100,10))

gridExtra::grid.arrange(a,b)

##From the age distributions we can see: (1) the age of patients is right-skewed; (2) there are more younger patients dead than survived; (3) there are more elder patients survived than dead.


#Plotting Boxplot to understand relationship of each variable with Death event
attach(dataset)
par(mfrow=c(2,3))
boxplot(age~DEATH_EVENT, main="Boxplot of Age")
boxplot(creatinine_phosphokinase~DEATH_EVENT, main="Boxplot of creatinine_phosphokinase",ylim=c(0,3000))
boxplot(ejection_fraction~DEATH_EVENT, main="Boxplot of ejection_fraction")
boxplot(platelets~DEATH_EVENT, main="Boxplot of platelets", log="y")
boxplot(serum_creatinine~DEATH_EVENT, main="Boxplot of serum_creatinine",ylim=c(0,5))
boxplot(serum_sodium~DEATH_EVENT, main="Boxplot of serum_sodium")
boxplot(time~DEATH_EVENT, main="Boxplot of time")

##From the Box plots, we can see:
##Survived patients have a larger age range than dead patients;
##Creatinine Phosphokinase (CPK) has little difference between survived and dead patients;
##Survived patients have lower Ejection Fraction than dead patients;
##Survived patients have a larger range (with small lower bound) of platelets than dead patients;
##Survived patients have a larger range (with larger upper bound) of Serum Creatinine than dead patients;
##Survived patients have a slightly larger range of Serum Sodium than dead patients;
##Survived patients have shorter follow-up periods than dead patients.


#Understanding the correlation between the variables
correlations <- cor(dataset[c(1,3,5,7,8,9,12)])
corrplot(correlations)
correlations

##From the correlation plot and the table, we can say there exist little/weak relationship between the numerical variables

#Understanding relationship of other variables (non-numerical) with Death event
plot_1 <- ggplot(data = dataset, mapping = aes(x = sex, y = ..count.., fill = DEATH_EVENT)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "How gender affects death events?")
plot_1
##There are more male patients than females. The death:survival rate is about the same (2:1) for male and female.

plot_2 <- ggplot(data = dataset, mapping = aes(x = anaemia, y = ..count.., fill = DEATH_EVENT)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "Barplot of anaemia")+
  theme_bw()
plot_2
##Patients with a decrease in red blood cell have a higher proportion of survival. 

plot_3 <- ggplot(data = dataset, mapping = aes(x = diabetes, y = ..count.., fill = DEATH_EVENT)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "Barplot of diabetes")+
  theme_bw()
plot_3
##There are fewer patients with diabetes. The death:survival rate is about the same (2:1) for diabeters and non-diabeters.

plot_4 <- ggplot(data = dataset, mapping = aes(x = high_blood_pressure, y = ..count.., fill = DEATH_EVENT)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "Barplot of high_blood_pressure")+
  theme_bw()
plot_4
##There are fewer patients with high blood pressure. Patients with high blood pressure have a higher proportion of survival.

plot_5 <- ggplot(data = dataset, mapping = aes(x = smoking, y = ..count.., fill = DEATH_EVENT)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "Barplot of smoking")+
  theme_bw()
plot_5
##There are fewer smoking patients than non-smoking patients. The death:survival rate is about the same (2:1) for smokers and non-smokers.



#T-Test

with(data=dataset,t.test(age[DEATH_EVENT=="Survived"],age[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is smaller than alpha 0.05. There is a significant difference in mean age between dead patients and survived patients.

with(data=dataset,t.test(creatinine_phosphokinase[DEATH_EVENT=="Survived"],creatinine_phosphokinase[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is larger than alpha 0.05. There is no significant difference in the mean level of CPK enzyme in blood between dead patients and survived patients.

with(data=dataset,t.test(ejection_fraction[DEATH_EVENT=="Survived"],ejection_fraction[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is smaller than alpha 0.05. There is a significant difference in the mean ejection fraction between dead patients and survived patients.

with(data=dataset,t.test(platelets[DEATH_EVENT=="Survived"],platelets[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is larger than alpha 0.05. There is no significant difference in mean platelets between dead patients and survived patients.

with(data=dataset,t.test(serum_creatinine[DEATH_EVENT=="Survived"],serum_creatinine[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is smaller than alpha 0.05. There is a significant difference in the mean level of Serum Creatinine between dead patients and survived patients.

with(data=dataset,t.test(serum_sodium[DEATH_EVENT=="Survived"],serum_sodium[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is smaller than alpha 0.05. There is a significant difference in the mean level of Serum Sodium between dead patients and survived patients.

with(data=dataset,t.test(time[DEATH_EVENT=="Survived"],time[DEATH_EVENT=="Death"],var.equal=TRUE))
##p-value is smaller than alpha 0.05. There is a significant difference in the mean follow-up period between dead patients and survived patients.


#Hotelling's T2 test

#install.packages("Hotelling")
library(Hotelling)

T2Test <- hotelling.test(age + creatinine_phosphokinase + ejection_fraction + platelets + serum_creatinine + serum_sodium + time ~ DEATH_EVENT, data=dataset)
T2Test
##p-value is smaller than alpha 0.05. The mean of at least one of the numerical parameters (age, CPK, ejection fraction, serum creatinine, serum sodium, time), or a combination of one or more parameters working together, is significantly different between dead patients and survived patients.


#F-Test

var.test(age[DEATH_EVENT=="Survived"],age[DEATH_EVENT=="Death"])
##p-value is smaller than alpha 0.05. There is a significant difference in variance of age between dead patients and survived patients.

var.test(creatinine_phosphokinase[DEATH_EVENT=="Survived"],creatinine_phosphokinase[DEATH_EVENT=="Death"])
##p-value is smaller than alpha 0.05. There is a significant difference in variance of CPK level between dead patients and survived patients.

var.test(ejection_fraction[DEATH_EVENT=="Survived"],ejection_fraction[DEATH_EVENT=="Death"])
##p-value is larger than alpha 0.05. There is  no significant difference in variance of ejection fraction between dead patients and survived patients.

var.test(platelets[DEATH_EVENT=="Survived"],platelets[DEATH_EVENT=="Death"])
##p-value is larger than alpha 0.05. There is no significant difference in variance of platelets between dead patients and survived patients.

var.test(serum_creatinine[DEATH_EVENT=="Survived"],serum_creatinine[DEATH_EVENT=="Death"])
##p-value is smaller than alpha 0.05. There is a significant difference in variance of the level of Serum Creatinine between dead patients and survived patients.

var.test(serum_sodium[DEATH_EVENT=="Survived"],serum_sodium[DEATH_EVENT=="Death"])
##p-value is smaller than alpha 0.05. There is a significant difference in variance of the level of Serum Sodium between dead patients and survived patients.

var.test(time[DEATH_EVENT=="Survived"],time[DEATH_EVENT=="Death"])
##p-value is larger than alpha 0.05. There is no significant difference in variance of the follow-up period between dead patients and survived patients.