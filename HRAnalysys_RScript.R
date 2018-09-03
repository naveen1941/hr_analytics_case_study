## LIBRARY
library(car)
library(ggplot2)
library(MASS)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(reshape2)
library(dplyr)
library(lubridate)

## user defined function

#Function converts the date columns to date rows
convert_empid_date_time <- function(df) {
  melted_df <- melt(df, id.vars = c("X"))
  melted_df <-
    rename(melted_df,
           EmployeeID = X,
           Date = variable,
           Time = value)
  melted_df$EmployeeID <- as.factor(melted_df$EmployeeID)
  melted_df$Date <- substring(melted_df$Date, 2)
  melted_df$Date <- as.Date(melted_df$Date, format = "%Y.%m.%d")
  return(melted_df)
}

#Function checks if the IN Time is NA and returns 1 if its NA otherwise Returns 0
is_emp_leave <- function(in_time) {
  if (is.na(in_time)) {
    return(1)
  } else{
    return(0)
  }
}


## DATA LOADING
employee_survey_data <-
  read.csv("employee_survey_Data.csv", stringsAsFactors = FALSE)

general_data <-
  read.csv("general_data.csv", stringsAsFactors = FALSE)

in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)

manager_survey_data <-
  read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)


## DATA CLEANING
in_time <-
  in_time[, colSums(is.na(in_time)) < nrow(in_time)] # remove all the columns with only NA values
out_time <-
  out_time[, colSums(is.na(out_time)) < nrow(out_time)] # remove all the columns with only NA values

#Using the in time and out time and deriving variables "Average Work Duration of the Employee"
#Assumption :
#if an employee IN Time is NA considering them as Leave.
in_time <- convert_empid_date_time(in_time)
out_time <- convert_empid_date_time(out_time)

in_time <- rename(in_time, IN_Time = Time)
out_time <- rename(out_time, OUT_Time = Time)

#Create a Temporary Data Frame by Merging the IN and OUT times data set into a single dataSet
emp_in_out_times_df <-
  merge(
    x = in_time,
    y = out_time,
    by = c("EmployeeID", "Date"),
    all = F
  )
rm(in_time, out_time) # remove the IN and OUT Times data set and clean the memory

#Using sapply and adding a column Leaves
emp_in_out_times_df$Leaves <-
  sapply(emp_in_out_times_df$IN_Time , is_emp_leave)

#Create a Data Frame with EmployeeID and Number of Annual Leaves
emp_annual_leaves <- aggregate(Leaves ~ EmployeeID,
                               data = emp_in_out_times_df,
                               sum)

#Removes rows in which IN Time is NA
emp_in_out_times_df <-
  emp_in_out_times_df[-which(is.na(emp_in_out_times_df$IN_Time)), ]

emp_in_out_times_df$IN_Time = as.POSIXlt(emp_in_out_times_df$IN_Time, format =
                                           "%Y-%m-%d %H:%M:%S")
emp_in_out_times_df$OUT_Time = as.POSIXlt(emp_in_out_times_df$OUT_Time, format =
                                            "%Y-%m-%d %H:%M:%S")

#Create a Temporary Data Frame with EmployeeID and Difference of IN Time & OUT Times
emp_in_out_times_df$work_duration <-
  as.numeric(
    difftime(
      emp_in_out_times_df$OUT_Time,
      emp_in_out_times_df$IN_Time,
      units = "hours"
    )
  )

#Create a Temporary Data Frame with EmployeeID and Work Duration
annual_emp_avg_workduration <-
  aggregate(work_duration ~ EmployeeID,
            data = emp_in_out_times_df,
            mean)
rm(emp_in_out_times_df)  # removing the temporarily created data

annual_emp_avg_workduration$work_duration <-
  signif(annual_emp_avg_workduration$work_duration, digits = 2) # Round off the average work duration to 2 Digits


#Create the Employee Data Frame with the Employee ID, Average work duration and Annual Leaves
employee_data <-
  merge(
    x = annual_emp_avg_workduration,
    y = emp_annual_leaves,
    by = c("EmployeeID"),
    all = F
  )
rm(annual_emp_avg_workduration, emp_annual_leaves) # clean Temporary Data frames created.

length(unique(employee_data$EmployeeID)) == length(unique(employee_survey_data$EmployeeID))#Prechecks for merging the survey data to employee_data
setdiff(employee_data$EmployeeID, employee_survey_data$EmployeeID)#Prechecks for merging the survey data to employee_data
employee_data <-
  merge(
    x = employee_data,
    y = employee_survey_data,
    by = c("EmployeeID"),
    all = F
  )
rm(employee_survey_data) # clean the survey data and release the memory

length(unique(employee_data$EmployeeID)) == length(unique(manager_survey_data$EmployeeID))
setdiff(employee_data$EmployeeID, manager_survey_data$EmployeeID)
employee_data <-
  merge(
    x = employee_data,
    y = manager_survey_data,
    by = c("EmployeeID"),
    all = F
  )
rm(manager_survey_data) # clean the manager survey data and release the memory

length(unique(employee_data$EmployeeID)) == length(unique(general_data$EmployeeID))
setdiff(employee_data$EmployeeID, general_data$EmployeeID)
employee_data <-
  merge(
    x = general_data,
    y = employee_data,
    by = c("EmployeeID"),
    all = F
  )
rm(general_data) # clean the general data and release the memory

#Handle the NAs with the value 'unknown'
employee_data$EnvironmentSatisfaction[is.na(employee_data$EnvironmentSatisfaction)] <- 'unknown'
employee_data$JobSatisfaction[is.na(employee_data$JobSatisfaction)] <- 'unknown'
employee_data$WorkLifeBalance[is.na(employee_data$WorkLifeBalance)] <- 'unknown'
employee_data$JobInvolvement[is.na(employee_data$JobInvolvement)] <- 'unknown'
employee_data$PerformanceRating[is.na(employee_data$PerformanceRating)] <- 'unknown'

#Factor the categorical Variables
employee_data$Attrition <- as.factor(employee_data$Attrition)
employee_data$BusinessTravel <- as.factor(employee_data$BusinessTravel)
employee_data$Department <- as.factor(employee_data$Department)
employee_data$Education <- as.factor(employee_data$Education)
employee_data$EducationField <- as.factor(employee_data$EducationField)
employee_data$Gender <- as.factor(employee_data$Gender)
employee_data$JobRole <- as.factor(employee_data$JobRole)
employee_data$JobLevel <- as.factor(employee_data$JobLevel)
employee_data$MaritalStatus <- as.factor(employee_data$MaritalStatus)
employee_data$Over18 <- as.factor(employee_data$Over18) # can remove
employee_data$EnvironmentSatisfaction <- as.factor(employee_data$EnvironmentSatisfaction)
employee_data$JobSatisfaction <- as.factor(employee_data$JobSatisfaction)
employee_data$WorkLifeBalance <- as.factor(employee_data$WorkLifeBalance)
employee_data$JobInvolvement <- as.factor(employee_data$JobInvolvement)
employee_data$PerformanceRating <- as.factor(employee_data$PerformanceRating)

#Creating dummies for BusinessTravel
dummy <- model.matrix( ~ BusinessTravel - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for Department
dummy <- model.matrix( ~ Department - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for Education
dummy <- model.matrix( ~ Education - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for EducationField
dummy <- model.matrix( ~ EducationField - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for JobRole
dummy <- model.matrix( ~ JobRole - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for MaritalStatus
dummy <- model.matrix( ~ MaritalStatus - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for EnvironmentSatisfaction
dummy <- model.matrix( ~ EnvironmentSatisfaction - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for JobSatisfaction
dummy <- model.matrix( ~ JobSatisfaction - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for WorkLifeBalance
dummy <- model.matrix( ~ WorkLifeBalance - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for JobInvolvement
dummy <- model.matrix( ~ JobInvolvement - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Creating dummies for JobLevel
dummy <- model.matrix( ~ JobLevel - 1, data = employee_data)
dummy <- dummy[, -1]
employee_data <- cbind(employee_data, dummy)

#Remove the categorical variables as dummies are created for each of them.
employee_data <- subset(
  employee_data,
  select = -c(
    WorkLifeBalance,
    BusinessTravel,
    Department,
    Education,
    EducationField,
    JobLevel,
    JobRole,
    MaritalStatus,
    Over18,
    EmployeeCount,
    EnvironmentSatisfaction,
    JobSatisfaction,
    WorkLifeBalance,
    JobInvolvement,
    StandardHours,
    EmployeeID
  )
)

#Convert the Gender to 1s and 0s
employee_data$Gender <- ifelse(employee_data$Gender == "Male",1,0)
#Convert the Attrition to 1s and 0s
employee_data$Attrition <- ifelse(employee_data$Attrition == "Yes",1,0)
#Convert the PerformanceRating to 1s and 0s
employee_data$PerformanceRating <- ifelse(employee_data$PerformanceRating == 3,1,0)

#Scale the continous variables
employee_data$Age       	<- scale(employee_data$Age)
employee_data$DistanceFromHome       	<- scale(employee_data$DistanceFromHome)
employee_data$MonthlyIncome          	<- scale(employee_data$MonthlyIncome)
employee_data$NumCompaniesWorked     	<- scale(employee_data$NumCompaniesWorked)     
employee_data$PercentSalaryHike      	<- scale(employee_data$PercentSalaryHike)    
employee_data$StockOptionLevel       	<- scale(employee_data$StockOptionLevel)     
employee_data$TotalWorkingYears      	<- scale(employee_data$TotalWorkingYears)
employee_data$TrainingTimesLastYear  	<- scale(employee_data$TrainingTimesLastYear)  
employee_data$YearsAtCompany         	<- scale(employee_data$YearsAtCompany)
employee_data$YearsSinceLastPromotion	<- scale(employee_data$YearsSinceLastPromotion)
employee_data$YearsWithCurrManager   	<- scale(employee_data$YearsWithCurrManager)  
employee_data$work_duration      	<- scale(employee_data$work_duration)
employee_data$Leaves                 	<- scale(employee_data$Leaves)               

#Write the cleaned data to a csv file.
write.csv(employee_data, file="employee_data.csv")

Attrition <- sum(employee_data$Attrition)/nrow(employee_data)
Attrition

## DATA MODELING
set.seed(100)

# Sample the Data
trainindices = sample(1:nrow(employee_data), 0.7 * nrow(employee_data))

# Traning Dataset
train_employee_data <- employee_data[trainindices, ]

# Test Dataset
test_employee_data <- employee_data[-trainindices, ]

model_1 <- glm(Attrition ~ . , data=train_employee_data, family="binomial")
summary(model_1)

#Step AIC Model
stepModel <- stepAIC(model_1, direction = "both")
summary(stepModel)

model_3 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion
    + YearsWithCurrManager
    + work_duration
    + BusinessTravelTravel_Frequently
    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`
    + DepartmentSales
    + Education5
    + `EducationFieldLife Sciences`
    + EducationFieldMarketing
    + EducationFieldMedical
    + EducationFieldOther
    + `EducationFieldTechnical Degree`
    + JobRoleManager
    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`
    + `JobRoleSales Executive`
    + MaritalStatusMarried
    + MaritalStatusSingle
    + EnvironmentSatisfaction2
    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4
    + JobSatisfaction2
    + JobSatisfaction3
    + JobSatisfaction4
    + WorkLifeBalance2
    + WorkLifeBalance3
    + WorkLifeBalance4
    + WorkLifeBalanceunknown
    + JobInvolvement3
    + JobLevel5 ,
    family = "binomial",
    data = train_employee_data
  )
summary(model_3)
vif(model_3)

model_4 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + EducationFieldMarketing  + EducationFieldMedical    + EducationFieldOther
    + `EducationFieldTechnical Degree`    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_4)
vif(model_4)

model_5 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + EducationFieldMedical    + EducationFieldOther
    + `EducationFieldTechnical Degree`    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_5)
vif(model_5)

model_6 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + EducationFieldMedical + `EducationFieldTechnical Degree`    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_6)
vif(model_6)


model_7 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + `EducationFieldTechnical Degree`    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_7)
vif(model_7)

model_8 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleResearch Director`    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_8)
vif(model_8)


model_9 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + WorkLifeBalanceunknown    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_9)
vif(model_9)


model_10 <-
  glm(
    formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_10)
vif(model_10)

model_11 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + `JobRoleSales Executive`    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    + JobLevel5 ,    family = "binomial",    data = train_employee_data
  )
summary(model_11)
vif(model_11)

model_12 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + MaritalStatusMarried
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_12)
vif(model_12)

model_13 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + StockOptionLevel +
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_13)
vif(model_13)


model_13 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales  + Education5
    + JobRoleManager    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_13)
vif(model_13)

model_14 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany
    + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + JobRoleManager    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_14)
vif(model_14)

model_15 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + JobRoleManager    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_15)
vif(model_15)

model_16 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4    + JobInvolvement3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_16)
vif(model_16)

model_17 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction2    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4,    family = "binomial",    data = train_employee_data
  )
summary(model_17)
vif(model_17)

model_18 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4    + JobSatisfaction3
    + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4,    family = "binomial",    data = train_employee_data
  )
summary(model_18)
vif(model_18)

model_19 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4    + WorkLifeBalance2    + WorkLifeBalance3
    + WorkLifeBalance4,    family = "binomial",    data = train_employee_data
  )
summary(model_19)
vif(model_19)

model_20 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4    + WorkLifeBalance3
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_20)
vif(model_20)

model_21 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently    + BusinessTravelTravel_Rarely
    + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_21)
vif(model_21)

model_22 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_22)
vif(model_22)

model_23 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction2    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_23)
vif(model_23)

model_24 <-
  glm(
    formula = Attrition ~ Age + NumCompaniesWorked  + 
      TotalWorkingYears + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_24)
vif(model_24)

model_25 <-
  glm(
    formula = Attrition ~ NumCompaniesWorked  + 
      TotalWorkingYears + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction3
    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_25)
vif(model_25)

model_26 <-
  glm(
    formula = Attrition ~ NumCompaniesWorked  + 
      TotalWorkingYears + YearsSinceLastPromotion    + YearsWithCurrManager + work_duration    
    + BusinessTravelTravel_Frequently + `DepartmentResearch & Development`+ DepartmentSales 
    + `JobRoleManufacturing Director`
    + MaritalStatusSingle    + EnvironmentSatisfaction4  +
      + JobSatisfaction4
    ,    family = "binomial",    data = train_employee_data
  )
summary(model_26)
vif(model_26)

# Final Model
final_model <- model_26

## Evaluating the Model
#Predict the Model
test_predicted <- predict(final_model,type="response",newdata = test_employee_data[-2])
test_employee_data$predicted_prob <- test_predicted

#Comparing the Predicted and Actual Attrition of the Model
test_predicted_Attrition <- factor(ifelse(test_employee_data$predicted_prob >= 0.40, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test_employee_data$Attrition == 1, "Yes", "No"))
attritionData <- table(test_actual_Attrition,test_predicted_Attrition)
attritionData

#						test_predicted_Attrition
#test_actual_Attrition	No  Yes
#						No  1036   73
#						Yes  136   70

accuracy_value <- (attritionData[1] +attritionData[4])/nrow(test_employee_data)
accuracy_value #0.8359788

sensitivity_value <- (attritionData[4])/(attritionData[4] + attritionData[2])
sensitivity_value #0.3398058

specificity_value <- attritionData[1] / ( attritionData[1] + attritionData[3] )
specificity_value #0.9341749

