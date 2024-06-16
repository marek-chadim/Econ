install.packages("dummies")
#install.packages("AER")
library(dummies)
library(AER)

# Set workspace
setwd("data")

# Read in the data
data <- read.csv("inpres_data.csv")

# OLS regression
summary(lm(log_wage~education, data=data))

# DD Tables: Education
data$exposed <- data$birth_year>67
mean(data[data$high_intensity==0&data$exposed==FALSE,"education"])
mean(data[data$high_intensity==0&data$exposed==TRUE,"education"])
mean(data[data$high_intensity==1&data$exposed==FALSE,"education"])
mean(data[data$high_intensity==1&data$exposed==TRUE,"education"])

# DD Tables: Wages
mean(data[data$high_intensity==0&data$exposed==FALSE,"log_wage"])
mean(data[data$high_intensity==0&data$exposed==TRUE,"log_wage"])
mean(data[data$high_intensity==1&data$exposed==FALSE,"log_wage"])
mean(data[data$high_intensity==1&data$exposed==TRUE,"log_wage"])

# DD by regression
data$exp_int <- data$exposed*data$high_intensity
summary(lm(education~exp_int+exposed+high_intensity, data=data))
summary(lm(log_wage~exp_int+exposed+high_intensity, data=data))

# IV version
summary(ivreg(log_wage~education+exposed+high_intensity|exp_int+exposed+high_intensity, data=data))

# Better instrument and controls
rgns <- fixest::to_integer(data$birth_region)
byrs <- fixest::to_integer(data$birth_year)
ivdat <- data.frame(rgns,byrs)
ivdat$log_wage <- data$log_wage
ivdat$education <- data$education
ivdat$exp_nsch <- data$exposed * data$num_schools
ivdat$ch_exp <- data$children71 * data$exposed

# IV Regression
ivreg(log_wage~.-exp_nsch|.-education,data=ivdat)
