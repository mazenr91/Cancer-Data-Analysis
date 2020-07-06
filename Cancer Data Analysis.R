#DATA CLEANING 
library(readxl)
library(mice)
library(dplyr)
library(tidyr)
# read csv
cancer<-read.csv("/Users/mulan17/Downloads/cancer_reg.csv")

# separate "Geography" column to "County" and "State"
cancer1<-separate(cancer,col=Geography,into = c("County","State"), sep = ", ") 

# replace PctSomeCol18_24 column with formula=100-(PctNoHS18_24+PctHS18_24+PctBachDeg18_24)
cancer1$PctSomeCol18_24<-100-(cancer$PctNoHS18_24+cancer$PctHS18_24+cancer$PctBachDeg18_24)

# check missing observations
md.pattern(cancer1)

# use MI mothod to fill the missing data for PctEmployed16_Over and PctPrivateCoverageAlone

imputed_data <- mice(cancer1, m=25, maxit=30, meth='pmm')
cancer2 <- complete(imputed_data, 1)
md.pattern(cancer2)

#Check to make sure all the values in PctPrivateCoverage>PctPrivateCoverageAlone
x<-sample(if_else(cancer2$PctPrivateCoverage>cancer2$PctPrivateCoverageAlone,TRUE, FALSE))
table(x)["TRUE"]

write.csv(cancer2,'/Users/mulan17/Desktop/MMA/MMA 860/cancer_clean2.csv', row.names = FALSE )
#FEATURE ENGINEERING
cancer <- read.csv('cancer_cleaned.csv')
cancer$State <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", cancer$State)
cancer$State[cancer$State == "Districtof Columbia"] <- "District Of Columbia"

# Merge with air quality data, roughly 60% of the county level information is missing 
aqi <- read.csv('/Users/justinlee/Downloads/annual_aqi_by_county_2016.csv')
aqi$Non.Good.Day.Ratio <- 1 - (aqi$Good.Days / aqi$Days.with.AQI)

# Lots of counties missing, average metrics by state is one option
# I just change the names of some of the columns to make merging easier
aqi_data <- aqi[c('Non.Good.Day.Ratio', 'Max.AQI', 'Median.AQI', 'State')]
state_level_aqi_data <- aggregate(aqi_data[,1:3], list(aqi_data$State), mean)
names(state_level_aqi_data)[1] <- "State"
names(state_level_aqi_data)[2] <- paste(names(state_level_aqi_data)[2], "StateLevel", sep=".")
names(state_level_aqi_data)[3] <- paste(names(state_level_aqi_data)[3], "StateLevel", sep=".")
names(state_level_aqi_data)[4] <- paste(names(state_level_aqi_data)[4], "StateLevel", sep=".")

# Multiply the ratio by 100, since all the other percetnages are 100
state_level_aqi_data$Non.Good.Day.Ratio.StateLevel <- state_level_aqi_data$Non.Good.Day.Ratio.StateLevel * 100


# Option 2, try to impute 60% missing AQI metrics at a county level, use joined data as predictors on the missing amount

aqi$Geography <- paste(aqi$County, aqi_data$State, sep = " County, ")
aqi_data <- aqi[c('Non.Good.Day.Ratio','Max.AQI', 'Median.AQI','County')]
county_level_aqi_cancer <- merge(cancer, aqi_data, by="County", all.x = TRUE)
 
# *** DON'T RUN THIS IT WILL TAKE A LONG TIME, I have the file ***
imputed_data <- mice(county_level_aqi_cancer, m=60, maxit=40, meth = 'pmm', remove_collinear = FALSE)

county_level_aqi_cancer <- complete(imputed_data, 1)
names(county_level_aqi_cancer)[38] <-  paste(names(county_level_aqi_cancer)[38], "CountyLevelImputed", sep=".")
names(county_level_aqi_cancer)[39] <-  paste(names(county_level_aqi_cancer)[39], "CountyLevelImputed", sep=".")
names(county_level_aqi_cancer)[40] <-  paste(names(county_level_aqi_cancer)[40], "CountyLevelImputed", sep=".")

# Multiply the ratio by 100, since all the other percetnages are 100
county_level_aqi_cancer$Non.Good.Day.Ratio.CountyLevelImputed <- county_level_aqi_cancer$Non.Good.Day.Ratio.CountyLevelImputed * 100

# Merge county level and state level dataframes

all_cancer_features <- merge(county_level_aqi_cancer, state_level_aqi_data, by = 'State', all.x = TRUE)
write.csv(all_cancer_features, "all_cancer_features.csv", row.names = FALSE)


# *** For only including dummy variables in the model, just run part below ***

# R's regression formula will treat categorical variables as indicator variables, aka will use each category as a predictor complete with statistics
# I'll make dummy variables here seperately in case anyone wants to remove specific states/only include specific states in the final model
# However, if you make all the dummy variables, fit a regression on them, you will get multicollinearity since we will have all states present
# Just do the initial fit using the "State" category, look at p-values, think about what/if you want to include anything
# Then generate the dummy variables below, and merge into the main data horizontally (instructions below)

cancer_data <- read.csv(<path for cancer data>)
cancer_states_dummy <- data.frame()
for (state in unique(cancer_data$State)) {
  cancer_states_dummy[1:nrow(cancer_data),state] <- ifelse(cancer_data$State == state, 1, 0)
}

# Then, whatever state dummy variable you want to include, just do something like this new_data <- cbind(cancer_data, cancer_states_dummy[c("Alabama", "Idaho", "Kentucky)])
# Replace with your state(s) of choice of course

# Predictive Modelling
library(tidyverse)
library(ggplot2)
library(data.table)
library (ggthemes)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(esquisse)
library(car)
library(estimatr)
library(caret)
library(GGally)
library(DAAG)
#load data
library(readr)
cancer <- read_csv(file.choose())

#Model selection
reg_all <- lm(TARGET_deathRate ~ . , cancer)
summary(reg_all)
#lets take the States and counties out. 

reg_most <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + medIncome + popEst2015 + povertyPercent 
              + AvgHouseholdSize + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24+ PctHS25_Over
              + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
              + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
              + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel, cancer)
summary(reg_most)
# GOing to remove State variable for now. 
# PctNoHS18_24 removed since it along with PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 = 100%


reg_1 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
              + PercentMarried + PctHS18_24 + PctSomeCol18_24 +PctBachDeg18_24+ PctHS25_Over + MedianAgeMale + MedianAgeFemale
              + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
              + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
              + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer)

summary(reg_1)
plot(reg_1)

# Should do an f-test on PctNoHS18_24, PctHS18_24, PctSomeCol18_24, PctBachDeg18_24
education_hyp <- linearHypothesis(reg_1, c("PctSomeCol18_24 = 0", "PctBachDeg18_24 = 0", "PctHS18_24 = 0")) 
education_hyp  

education_hyp2 <- linearHypothesis(reg_1, c(" PctHS25_Over = 0", " PctBachDeg25_Over = 0"))
education_hyp2

#P-value of 3.022e-07 and 2.2e-16 respectivly shows that education jointly between 18_24 is a factor. 
#Linear hypothesis test

#Hypothesis:
#  PctSomeCol18_24 = 0
#PctBachDeg18_24 = 0
#PctHS18_24 = 0

#Model 1: restricted model
#Model 2: TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + 
#  popEst2015 + povertyPercent + PercentMarried + PctHS18_24 + 
#  PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + PctBachDeg25_Over + 
#  PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + 
#  PctPrivateCoverageAlone + PctEmpPrivCoverage + PctPublicCoverage + 
#  PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + 
#  PctOtherRace + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel

#Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
#1   3024 1132257                                  
#2   3021 1119910  3     12348 11.103 3.022e-07 ***

#Now lets look at insurace coverage. 
summary(cancer$PctPrivateCoverage)
summary(cancer$PctPrivateCoverageAlone)
summary(cancer$PctPublicCoverage)
summary(cancer$PctPublicCoverageAlone)

#public_2_private <- cancer$PctPublicCoverageAlone/cancer$PctPrivateCoverageAlone
#PctPublicCoverage + PctPrivateCoverage = 1
#remove either PCtPubliccoverage or PctPrivateCoverage

reg_2 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015  
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + MedianAgeMale + MedianAgeFemale
            + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer)

summary(reg_2)

#should Public coverage alone be dropped?

reg_3 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015  
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctWhite + PctBlack + PctAsian + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer)
summary(reg_3)
#Public Coverage alone dropped

#lets take a look at Unemployment.
reg_4 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctWhite + PctBlack + PctAsian + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer)
summary(reg_4)
#unemployment can be dropped because the r-squared when it was removed only decreased by 0.0001

#like some of the other variables the sume of the race % is one. I will dropp PctWhite to compensate for this. 

reg_5 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctBlack + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer)
summary(reg_5)
par(mfrow = c(2, 2))
plot(reg_5)

#there are 3 obs that can be removed as outliers or leverage points.

cancer_2 <- cancer[-c(172, 919, 1408, 2788),]
length(cancer$Obs)
length(cancer_2$Obs)

reg_6 <- lm(TARGET_deathRate ~ incidenceRate + popEst2015  
            + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage  
            + PctEmpPrivCoverage + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel , cancer_2)

summary(reg_6)

plot(density(resid(reg_6)))
par(mfrow = c(2, 2))
plot(reg_6)
ncvTest(reg_6) # The results show heteroskedasticity and we need to run a robut regression
reg_new<-lm_robust(TARGET_deathRate ~ incidenceRate + popEst2015
          + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
          + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage
          + PctEmpPrivCoverage + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
          + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer_2, se_type="HC3")
summary(reg_new)





#Predicting Values: Test & Train 
#Split the data to train data and test data
sample <- sample.int(n = nrow(cancer_2), size = floor(.7*nrow(cancer_2)), replace = F)
train <- cancer_2[sample, ]
test  <- cancer_2[-sample, ]
nrow (train)+nrow (test)==nrow (cancer_2)

#K fold cross validation
ctrl <- trainControl (method = "cv", number = 5)

#train the model
reg_7<- train (TARGET_deathRate ~ incidenceRate + popEst2015  
               + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
               + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage  
               + PctEmpPrivCoverage + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
               + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.StateLevel, data = train, method= "lm", trControl = ctrl)

summary(reg_7)

#Predict the test result
pred_test<-predict(reg_7,test)
summary(pred_test)

data.frame( R2 = R2(pred_test, test$TARGET_deathRate),
            RMSE = RMSE(pred_test, test$TARGET_deathRate))

 
#Prediction Interval 
User_input<-function(){
  
  incidenceRt<<-readline(prompt="Enter the value of incidenceRt:") 
  popEt2015<<-readline(prompt="Enter the value of popEt2015:") 
  PerHS18_24<<-readline(prompt="Enter the value of PerHS18_24:") 
  PerSomeCol18_24<<-readline(prompt="Enter the value of PerSomeCol18_24:")
  PerBachDeg18_24<<-readline(prompt="Enter the value of PerBaBachDeg18_24:")
  PerHS25_Over<<-readline(prompt="Enter the value of of PerHS25_Over:")
  PerBachDeg25_Over<<-readline(prompt="Enter the value of PerBachDeg25_Over :")
  PerEmployed16_Over<<-readline(prompt="Enter the value of PerEmployed16_Over:")  
  PerPrivateCoverage<<-readline(prompt="Enter the value of PerPrivateCoverage :") 
  PerEmpPrivCoverage<<-readline(prompt="Enter the value of PerEmpPrivCoverage:") 
  PerOtherRace<<-readline(prompt="Enter the value of PerOtherRace:")
  PerWhite<<-readline(prompt="Enter the value of PerWhite:")
  MedAgeMale<<-readline(prompt="Enter the value of MedAgeMale:")
  MedAgeFemale<<-readline(prompt="Enter the value of MedAgeFemale :")
  PerMarriedHouseholds<<-readline(prompt="Enter the value of PerMarriedHouseholds:") 
  BirthRt<<-readline(prompt="Enter the value of BirthRt:")
  Non.Good.Day.Ratio.Sl<<-readline(prompt="Enter the value of Non.Good.Day.Ratio.StateLevel:")
  
  
    
  incidenceRt<<-as.integer(incidenceRt) 
  popEt2015<<-as.integer(popEt2015) 
  PerHS18_24<<-as.integer(PerHS18_24) 
  PerSomeCol18_24<<-as.integer(PerSomeCol18_24)
  PerBachDeg18_24<<-as.integer(PerBachDeg18_24)
  PerHS25_Over<<-as.integer(PerHS25_Over)
  PerBachDeg25_Over<<-as.integer(PerBachDeg25_Over)
  PerEmployed16_Over<<-as.integer(PerEmployed16_Over) 
  PerPrivateCoverage<<-as.integer(PerPrivateCoverage) 
  PerEmpPrivCoverage<<-as.integer(PerEmpPrivCoverage) 
  PerOtherRace<<-as.integer(PerOtherRace)
  PerWhite<<-as.integer(PerWhite)
  MedAgeMale<<-as.integer(MedAgeMale)
  MedAgeFemale<<-as.integer(MedAgeFemale)
  PerMarriedHouseholds<<-as.integer(PerMarriedHouseholds) 
  BirthRt<<-as.integer(BirthRt)
  Non.Good.Day.Ratio.Sl<<-as.integer(Non.Good.Day.Ratio.Sl)
  
  return(list(incidenceRt, 
              popEt2015, 
              PerHS18_24,PerSomeCol18_24,PerBachDeg18_24,PerHS25_Over,PerBachDeg25_Over
              ,PerEmployed16_Over,PerPrivateCoverage,
              PerEmpPrivCoverage,
              PerOtherRace,
              PerWhite,
              MedAgeMale,
              MedAgeFemale,
              PerMarriedHouseholds, 
              BirthRt,Non.Good.Day.Ratio.Sl))
  
}

User_input()

user_df<-data.frame(incidenceRate=incidenceRt, popEst2015= popEt2015
                  ,PctHS18_24=PerHS18_24 ,PctSomeCol18_24=PerSomeCol18_24,PctBachDeg18_24=PerBachDeg18_24,PctHS25_Over=PerHS25_Over
                  ,PctBachDeg25_Over=PerBachDeg25_Over, PctEmployed16_Over=PerEmployed16_Over,  PctPrivateCoverage=PerPrivateCoverage ,  
                  PctEmpPrivCoverage=PerEmpPrivCoverage ,PctOtherRace=PerOtherRace, PctWhite=PerWhite,MedianAgeMale=MedAgeMale, MedianAgeFemale=MedAgeFemale,PctMarriedHouseholds=PerMarriedHouseholds, BirthRate=BirthRt,Non.Good.Day.Ratio.StateLevel=Non.Good.Day.Ratio.Sl) 

predict(reg_6,newdata=user_df,interval="prediction",level=0.95)


