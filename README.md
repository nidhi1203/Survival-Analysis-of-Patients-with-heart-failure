# Survival-Analysis-of-Patients-with-Heart-Failure
## Problem Statement
>> Determine the features that can improve the chance of survival for patients with previous heart failure.
## Team Information
>> | Members  | Username |
>> | ------------- | ------------- |
>> |  Kun Wang |  @kunwangRU |
>> | Nidhi Thakkar  | @nidhi1203  |
## Hypotheses
>> _Age_: Younger patients have a higher chance to survive than elder patients, holding other features constant.\
>> \
>> _Anaemia_: If the red blood cells do not decrease, the patient has higher chance of survival compared to patients with decreased blood cells, holding other features constant.\
>> \
>> _High blood pressure_: Patients without high blood pressure have a higher chance of survival compared to patients with high blood pressure, holding other features constant.\
>> \
>> _Creatinine Phosphokinase (CPK)_: Patients with lower level of CPK have a higher chance of survival, holding other features constant.\
>> \
>> _Diabetes_: Patients without diabetes have a higher chance of survival than patients having diabetes, holding other features constant.\
>> \
>> _Ejection Fraction_: Patients with lower Ejection Fraction percent will have a higher chance of survival, holding other features constant. \
>> \
>> _Platelets_: Patients with higher platelets count have a higher chance of survival, holding other features constant.\
>> \
>> _Serum Creatinine_: Patients with a lower level of Serum Creatinine in blood have a higher chance of survival, holding other features constant.\
>> \
>> _Serum sodium_: Patients with normal sodium range(135-145 mEq/L) have a higher chance of survival, holding other features constant.\
>> \
>> _Sex_: Male patients have a higher chance of survival than female patients, holding other features constant.\
>> \
>> _Smoking_: Patients who don’t smoke have a higher chance of survival than non-smoking patients, holding other features constant.
## Data Dictionary
>> The dataset consists of 13 columns and 299 rows. It contains the medical records of 299 patients who had heart failure, collected during their follow-up period, where each patient profile has 13 demographic, time, and clinical features.
>> | Column Name  | Data Type |  Description  |
>> | ------------- | ------------- | ------------- |
>> | Age | Numerical |  Age of the patient  |
>> | Anaemia  | Binary |  If red blood cells decrease or not  |
>> | High Blood Pressure  | Binary |  If the patient has high blood pressure or not  |
>> | Creatinine Phosphokinase(CPK)  | Numerical |  Level of CPK enzyme in blood (mcg/L)  |
>> | Diabetes | Binary |  If the patient has diabetes or not  |
>> | Ejection Fraction  | Numerical |  Percentage of blood leaving the heart at each contraction (percentage)  |
>> | Platelets  | Numerical |  Platelets in the blood (kiloplatelets/mL)  |
>> | Serum Creatinine  | Numerical |  Level of Serum Creatinine in blood (mg/dL)  |
>> | Serum Sodium  | Numerical |  Level of serum sodium in the blood (mEq/L)  |
>> | Sex  | Binary |  Female or male  |
>> | Smoking  | Binary |  If the patient smokes or not  |
>> | Time  | Numerical |  Follow-up period (days)  |
>> | Death Event  | Binary |  If the patient survived during the follow-up period  |
## SPAP Plan for Our Project
### 1. Goal
>> Determine the features that can improve the chance of survival for patients with previous heart failure.
### 2. Dependant variables
>> Death Event: if the patient survived during the follow-up period. (0: deceased, 1: survived)
### 3. Specific Questions
>> **Q1**: Do the demographic features improve the chance of survival of heart failure patients?\
>> **Q2**: Do the clinical features improve the chance of survival of heart failure patients?
### 4. Independent variables
>> **Independent Variables for Q1**: Age, Sex\
>> \
>> **Independent Variables for Q2**: Anaemia, High blood pressure, Creatinine Phosphokinase(CPK), Diabetes, Ejection Fraction, Platelets, Serum Creatinine, Serum Sodium, Smoking\
>> \
>> **Other Independent Variable**: Time
### 5. Specific analyses and graphs
>> **Independent Variables for Q1**:\ 
>> _Age_: histogram, _Sex_: bar chart/pie chart\
>> \
>> **Independent Variables for Q2**: \
>> _Anaemia, High blood pressure, Diabetes, Smoking_: bar chart/pie chart\
>> _CPK, Ejection Fraction, Platelets, Serum Creatinine, Serum Sodium_: histogram\
>> \
>> **Other Independent Variable**: \
>> _Time_: histogram
## References:
>> https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records#
