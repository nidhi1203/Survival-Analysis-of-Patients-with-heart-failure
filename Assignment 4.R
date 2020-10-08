#Loading Packages
library(knitr)
library(pander)
library(dplyr)

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


#Correlation

correlation<-cor(dataset[c(1,3,5,7,8,9,12)])
View(correlation)
#From the table, we can see all the continuous variables are uncorrelated


#Principal components 

dataset_pca <- prcomp(dataset[c(1,3,5,7,8,9,12)],scale=TRUE)
dataset_pca

#Recreating the summary table manually
(eigen_dataset <- dataset_pca$sdev^2)
names(eigen_dataset) <- paste("PC",1:7,sep="")
eigen_dataset
sumlambdas <- sum(eigen_dataset)
sumlambdas
propvar <- eigen_dataset/sumlambdas
propvar
cumvar_dataset <- cumsum(propvar)
cumvar_dataset
matlambdas <- rbind(eigen_dataset,propvar,cumvar_dataset)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,6)
summary(dataset_pca)
dataset_pca$rotation
print(dataset_pca)

#Option 1
#Based on retating components that account for 70% to 90% of the variance, we need to retain PC1 to PC5 or PC1 to PC6

#Option 2
#Based on the rule of sum to choose all components with eigen values larger than 0.7, we need to retain all the PC's

# Sample scores stored in dataset_pca$x (Calculating Sample scores for each record in the dataset)
dataset_pca$x

# Identifying the scores by their survival status
DEATH_EVENT <- data.frame(DEATH_EVENT=dataset$DEATH_EVENT)
survival_pca <- cbind(DEATH_EVENT, dataset_pca$x)
survival_pca


# Means of scores for all the PC's classified by Survival status

#Calculating the mean for all PC's based on Death Event
tabmeansPC <- aggregate(survival_pca[,2:8],by=list(DEATH_EVENT=dataset$DEATH_EVENT),mean)
tabmeansPC

#Swapping rows 1 and 2, putting Survived as row 1, Death as row 2
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$DEATH_EVENT)),]
tabmeansPC

#Transforming rows to columns and columns to rows
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans

#Changing column names from 2,1 to Survived and Death
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Survival status

#Calculating the Standard deviation for all the PC's based on DEATH_EVENT
tabsdsPC <- aggregate(survival_pca[,2:8],by=list(DEATH_EVENT=dataset$DEATH_EVENT),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

#T-Test
t.test(PC1~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC2~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC3~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC4~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC5~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC6~dataset$DEATH_EVENT,data=survival_pca)
t.test(PC7~dataset$DEATH_EVENT,data=survival_pca)

#From the results of T-test based on alpha=0.05, we can conclude - 
#PC1, PC3, and PC5 have significant difference in the means between patients who survived and who are dead
#PC2, PC4, PC6, and PC7 have no significant difference in the means between patients who survived and who are dead

#F-Test
var.test(PC1~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC2~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC3~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC4~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC5~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC6~dataset$DEATH_EVENT,data=survival_pca)
var.test(PC7~dataset$DEATH_EVENT,data=survival_pca)

#From the results of F-test based on alpha=0.05, we can conclude - 
#PC1, PC2, PC3, PC5 and PC7 have significant difference in the variance between patients who survived and who are dead
#PC4 and PC6 have no significant difference in the variance between patients who survived and who are dead

#Plotting the Scree diagram

plot(eigen_dataset, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Based on scree diagram, since the position of elbow is at PC6, we should keep PC1 to PC6 and discard PC7.

plot(log(eigen_dataset), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")

#Based on Log scree diagram, since the position of elbow is at PC6, we should keep PC1 to PC6 and discard PC7.

print(summary(dataset_pca))
View(dataset_pca)
diag(cov(dataset_pca$x))
dataset_pca$rotation[,1]
dataset_pca$rotation

plot(dataset[c(1,3,5,7,8,9,12)])
#Based on the plot, we can see our original continuous variables are not correlated

dataset_pca$x
plot(dataset_pca)
#Based on the plot, we can see variance for PC1 through PC7 is decreasing

#get the original value of the data based on PCA
center <- dataset_pca$center
scale <- dataset_pca$scale
new_dataset <- as.matrix(dataset[c(1,3,5,7,8,9,12)])
new_dataset
drop(scale(new_dataset,center=center, scale=scale)%*%dataset_pca$rotation[,1])
predict(dataset_pca)[,1]

#The aboved two gives us the same thing

out <- sapply(1:7, function(i){plot(dataset$DEATH_EVENT,dataset_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="DEATH_EVENT")})
#From the box plot we can see - 
#For PC1, the range for the survived patients is larger than the dead patients; and the survived patients overall have a higher value in PC1 than dead patients
#For PC2, the range for the survived patients is larger than the dead patients
#For PC3, the dead patients overall have a higher value than the survived patients
#For PC4, the range for the dead patients is slightly larger than the survived patients (with a smaller lower bound for dead patients)
#For PC5, the range for the survived patients is larger than the dead patients
#For PC6, the range for the dead patients is larger than the Survived patients
#For PC7, the range for the Survived patients is larger than the dead patients 

pairs(dataset_pca$x[,1:7], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,dataset$DEATH_EVENT)})
#From the graph, we can see all the PC's are uncorrelated