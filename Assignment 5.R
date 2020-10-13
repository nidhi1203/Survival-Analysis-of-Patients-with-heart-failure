#Loading Packages
library(cluster)
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
  mutate(DEATH_EVENT=ifelse(DEATH_EVENT=="Death",0,1),
         sex=ifelse(sex=="male",1,0)
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(age, anaemia, creatinine_phosphokinase, diabetes, ejection_fraction, high_blood_pressure, platelets,serum_creatinine, serum_sodium, sex, smoking, time, DEATH_EVENT)

View(dataset)
summary(dataset)


Heart_disease<-dataset
attach(Heart_disease)

#Scaling our dataset
scaled_dataset <- scale(Heart_disease[,1:13])
View(scaled_dataset)

#Checking if our scaled dataset has null values 
is.null(scaled_dataset)

# Creating a (Euclidean) distance matrix of the scaled/standardized data 
Distance <- dist(scaled_dataset, method="euclidean")
Distance

#Hirerarchical Methods
#1. Single Linkage Method
# Invoking hclust command (cluster analysis by single linkage method)      
clus_heart_disease.nn <- hclust(Distance, method = "single")
clus_heart_disease.nn

#Plotting of dendrogram using Single Linkage method

plot(as.dendrogram(clus_heart_disease.nn),ylab="Distance between patients",ylim=c(0,6),
     main="Dendrogram of 299 patients using Single Linkage method")


#2. Average Linkage Method
clus_heart_disease.avl <- hclust(Distance, method = "average")
clus_heart_disease.avl

#Plotting of dendrogram using Average Linkage method

plot(as.dendrogram(clus_heart_disease.avl),ylab="Distance between patients",ylim=c(0,10),
     main="Dendrogram of 299 patients using Average Linkage method")


#3. Complete Linkage Method
clus_heart_disease.fn <- hclust(Distance)
clus_heart_disease.fn

#Plotting of dendrogram using Complete Linkage method

plot(as.dendrogram(clus_heart_disease.fn),ylab="Distance between patients",ylim=c(0,14),
     main="Dendrogram of 299 patients using Complete Linkage method")


#Non-hirerarchical Method
#K-Means

# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans2.Heart_disease <- kmeans(scaled_dataset,2,nstart = 10))
kmeans2.Heart_disease

# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.Heart_disease$betweenss/kmeans2.Heart_disease$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

#Having two clusters will account for 88% of total variation
#We don't think Non-hirerachical method(K-means) is that suitable as we do not have identifier column for our dataset