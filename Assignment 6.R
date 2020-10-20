#Loading Packages
library(mvtnorm)
library(dplyr)
library(psych)

#Loading dataset
dataset <-read.csv("C:/Users/nidhi/OneDrive/Desktop/MVA/heart_failure_clinical_records_dataset.csv")
View(dataset)
attach(dataset)

#Identifying different columns names
names(dataset)

#Data Summary
str(dataset)
summary(dataset)
head(dataset)
dim(dataset)

#Checking for missing values
is.null(dataset)
##The "FALSE" output shows there is no missing data in the dataset.

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

#Based on Option 2 rule, which says components whose Eigenvalues are larger than 1 should be considered, so we need to keep PC1 through PC3

summary(dataset_pca)
eigvec.heart<-dataset_pca$rotation
print(eigvec.heart)

# Taking the first three PCs to generate linear combinations for all the variables with three factors
pcafactors.heart <- eigvec.heart[,1:3]
pcafactors.heart

# Multiplying each column of the eigenvector's matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.heart <- sweep(pcafactors.heart,MARGIN=2,dataset_pca$sdev[1:3],`*`)
unrot.fact.heart

# Computing communalities
communalities.heart <- rowSums(unrot.fact.heart^2)
communalities.heart

# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.heart <- varimax(unrot.fact.heart)
View(unrot.fact.heart)
rot.fact.heart

# The print method of varimax omits loadings less than abs(0.1). In order to display all the loadings, it is necessary to ask explicitly the contents of the object $loadings
fact.load.heart <- rot.fact.heart$loadings[1:7,1:3]
fact.load.heart

# Computing the rotated factor scores for the 299 Patients 
scale.heart <- scale(dataset[c(1,3,5,7,8,9,12)])
scale.heart
as.matrix(scale.heart)%*%fact.load.heart%*%solve(t(fact.load.heart)%*%fact.load.heart)

fit.pc <- principal(dataset[c(1,3,5,7,8,9,12)], nfactors=3, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings
# Loadings with more digits
pc.load.heart <- fit.pc$loadings[1:7,1:3]
print(pc.load.heart)

# Communalities
fit.pc$communality

# Rotated factor scores
fit.pc$scores

# Factor Analysis utilities

fa.parallel(dataset[c(1,3,5,7,8,9,12)]) 
#Based on the plot, we should retain two factors(based on the first elbow)

fa.plot(fit.pc) 
#Based on the plot, we can confirm that there is no correlation between RCs

fa.diagram(fit.pc) 
#This diagram visualizes the relationship

vss(dataset[c(1,3,5,7,8,9,12)]) 
#The Very Simple Structure recommends to retain 4 factors to achieve the maximum fit


detach(dataset)

