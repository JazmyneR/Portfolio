#read the data
ODL_data <- read.csv("Data - DIB - ODL.csv")
#descriptive statistics 
#calculating the mean for every variable 
apply(ODL_data, 2, mean)
#calculating the sd for every variable 
apply(ODL_data, 2, sd)
#calculate the correlation
cor(ODL_data)
library("lavaan")
attach(ODL_data)
#create model 1
model1 <- ' performance =~ PE1 + PE2 + PE3 + FC1 + FC2 + BI1 + BI3 
            effort      =~ PE4 + EE1 + EE2 + EE3 + EE4 + BI2 + BI4
            social      =~ SI1 + SI2 + SI3 + SI4 + FC3 + FC4'
#run model1 
result <- cfa(model1, data =ODL_data, estimator = "MLM")
#summarize the results 
summary(result, standardized=TRUE, fit.measures = TRUE)
#create model 2
model2 <- ' facilitate =~ FC1 + FC2 + FC3 + FC4 + PE1 + PE2 + PE3 + SI1 + SI2 + SI3 + SI4
            accept     =~ BI1 + BI2 + BI3 + BI4 + PE4 + EE1 + EE2 + EE3 + EE4 '
#run model2 
result2 <- cfa(model2, data = ODL_data, estimator = "MLM")
#summarize the results 
summary(result2, standardized = TRUE, fit.measures = TRUE)
#create model 3
model3 <- ' performance =~ PE1 + PE2 + PE3
            effort      =~ PE4 + EE1 + EE2 + EE3 + EE4
            social      =~ SI1 + SI2 + SI3 + SI4 
            facilitate  =~ FC1 + FC2 + FC3 + FC4
            accept      =~ BI1 + BI2 + BI3 + BI4'
# run model3 
result3 <- cfa(model3, data = ODL_data, estimator = "MLM")
#summarize the results 
summary(result3, standardized = TRUE, fit.measures = TRUE)
#chi-square difference test
anova(result, result3)
#chi-square difference test 
anova(result2, result3)
#modification indices 
modindices(result3)[order(modindices(result3)[,4], decreasing = TRUE),]
