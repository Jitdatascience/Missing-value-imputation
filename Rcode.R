# Load library

library(tidyverse)
library(MASS)
library(dplyr)
library(rms)


# Reading data file


heart_data<-read.csv("heart.csv")

# see the data set
head(heart_data,3) # first 3 rows of the data set

# change the column names 
names(heart_data) <- c("age", "sex", "chest_pain", "resting_bp","cholestrol",
                       "fasting_sugar", "resting_ECG", "max_heart_rate", 
                       "exercise_agina", "oldpeak", "slope", "number_major_vessels",
                       "thal", "disease_status")

str(heart_data)

# Preprocess the variables


heart_data$sex<-factor(heart_data$sex)
heart_data$slope<-factor(heart_data$slope)
heart_data$chest_pain<-factor(heart_data$chest_pain)
heart_data$fasting_sugar<-factor(heart_data$fasting_sugar)
heart_data$resting_ECG<-factor(heart_data$resting_ECG)
heart_data$exercise_agina<-factor(heart_data$exercise_agina)

heart_data$number_major_vessels<-factor(heart_data$number_major_vessels)
heart_data$thal<-factor(heart_data$thal)
heart_data$disease_status<-factor(heart_data$disease_status)

# Create missing values in the data set


set.seed(123)
library(missForest)
library(VIM)
#Generate 10% missing values at Random
heart_data_mis <- prodNA(heart_data, noNA = 0.2)

#png("my_plot1.png")
miss_plot <- aggr(heart_data_mis, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(heart_data_mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


# 1. Multiple imputation

## impute missing values using multivariate imputation by chained equations (MICE)


library(mice)
heart_data_mis_imp <- mice(heart_data_mis, m=5, maxit = 50, print = FALSE, seed = 1234)


summary(heart_data_mis_imp)
MICE_data<-complete(heart_data_mis_imp,3)
heart_data_mis_imp_long <- complete(heart_data_mis_imp, 'long')
#str(heart_data_mis_imp_long)
#summary(heart_data_mis_imp_long)
dim(heart_data_mis_imp_long)
write.csv(heart_data_mis_imp_long, "long_data.csv")
new<-read.csv("long_data.csv")
#str(new)
new<-new[,-c(1:3)]


#Inspecting the distribution of original and imputed data


xyplot(heart_data_mis_imp,disease_status ~ age + sex + chest_pain +
         resting_bp + cholestrol + fasting_sugar + resting_ECG + max_heart_rate
       + exercise_agina + oldpeak +slope + number_major_vessels +thal,pch=18,cex=1)

#png("my_mice.png")
densityplot(heart_data_mis_imp, ~ age + resting_bp + cholestrol + max_heart_rate, main="MICE")
#dev.off()



# 2. Multiple imputation with PMM

heart_data_mis_imp_ppm <- mice(heart_data_mis, m=5, maxit = 50, method = 'pmm', seed = 1234, print=FALSE)



summary(heart_data_mis_imp_ppm)

PMM_data<-complete(heart_data_mis_imp_ppm,3)

heart_data_mis_imp_ppm_long <- complete(heart_data_mis_imp_ppm, 'long')


#png("my_pmm.png")
densityplot(heart_data_mis_imp_ppm,~ age + resting_bp + cholestrol + 
              max_heart_rate, main="PMM") 
#dev.off()


# 3. Multiple imputation with Mean


meth = c("mean", "logreg", "polyreg", "mean", "mean", "logreg", "polyreg", 
         "mean", "logreg", "mean", "polyreg", "polyreg", "polyreg", "logreg")
heart_data_mis_imp_mean <- mice(heart_data_mis, m=5, maxit = 50, method = meth,
                                seed = 1234, print=FALSE)

summary(heart_data_mis_imp_mean)
Mean_data<-complete(heart_data_mis_imp_mean,3)
heart_data_mis_imp_mean_long <- complete(heart_data_mis_imp_mean, 'long')
#str(heart_data_mis_imp_mean)

#png("mean.png")
xyplot(heart_data_mis_imp_mean,disease_status ~ age + sex + chest_pain +
         resting_bp + cholestrol + fasting_sugar + resting_ECG + max_heart_rate
       + exercise_agina + oldpeak +slope + number_major_vessels +thal,pch=18,cex=1)
#dev.off()

#summary(heart_data_mis_imp_mean_long)
#png("my_mean.png")
densityplot(heart_data_mis_imp_mean, ~ age + resting_bp + cholestrol + 
              max_heart_rate, main="Mean")
#dev.off()




# 4. Adding missing values only in numerical variables 

test<-prodNA(heart_data[, c(1,4,5,8,10)], noNA = 0.2)
summary(test)

new_data<-data.frame(age=test$age, sex=heart_data$sex, chest_pain=heart_data$chest_pain, 
                     resting_bp = test$resting_bp, cholestrol=test$cholestrol,
                     fasting_sugar= heart_data$fasting_sugar, resting_ECG=heart_data$resting_ECG,
                     max_heart_rate= test$max_heart_rate, exercise_agina=heart_data$exercise_agina,
                     oldpeak = test$oldpeak, slope=heart_data$slope, 
                     number_major_vessels= heart_data$number_major_vessels,thal=heart_data$thal,
                     disease_status=heart_data$disease_status
)

#summary(new_data)



# Impute numerical missing values using mean imputation 

heart_data_mis_mean_imp <- mice(new_data, m=5, maxit = 50, method = "mean",
                                print=FALSE, seed = 1234)

summary(heart_data_mis_mean_imp)
Mean_numeric_data<-complete(heart_data_mis_mean_imp,3)

heart_data_mis_mean_imp_long <- complete(heart_data_mis_mean_imp, 'long')
#str(heart_data_mis_mean_imp_long)

summary(heart_data_mis_mean_imp_long)

#png("mean_numeric.png")
xyplot(heart_data_mis_mean_imp,disease_status ~ age + sex + chest_pain +
         resting_bp + cholestrol + fasting_sugar + resting_ECG + max_heart_rate
       + exercise_agina + oldpeak +slope + number_major_vessels +thal,pch=18,cex=1)
#dev.off()



#png("my_mean_numeric.png")
densityplot(heart_data_mis_mean_imp, ~ age + resting_bp + cholestrol + 
              max_heart_rate, main="Mean_numeric")
#dev.off()

# Summary table for original and imputed data using gtsummary package

library(gtsummary)
#library(webshot2)
library(kableExtra)
ABC<-rbind(cbind(heart_data, Table = "Original"), cbind(PMM_data, Table = "PMM"),
           cbind(Mean_data, Table = "Mean"))



tbl_summary_2 <- ABC %>%
  tbl_summary(by = Table, statistic = list(all_continuous() ~ "{mean} ({sd})",
                                           all_categorical() ~ " {N} ({p}%)"))

tbl_summary_2




