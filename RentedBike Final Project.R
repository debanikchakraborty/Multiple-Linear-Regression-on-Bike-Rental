rm(list=ls())
setwd("C:/Users/deban/OneDrive - Oklahoma A and M System/Spring 2023/Applied Regression Analysis - Rudra/Final Project")

library("car")
library("MASS")
data <- read.csv("Bikerental.csv", header=T)
sum(is.na(data)) #There is no missing value in this dataset, so no need to take subset. 


#Data Visualization 

range(data$RentedBikeCount) #0 to 3556
hist(data$RentedBikeCount, main = 'Histogram of RentedBikes', xlab="Rented Bikes Count") 
#Positively Skewed

boxplot(data$RentedBikeCount, main="Boxplot of RentedBikes", ylab="Rented Bikes Count") 
#Significant no. of outliers on right tail
mean(data$RentedBikeCount) #705


par(mfcol=c(1,3))
boxplot(data$Hour, main="Hour")
boxplot(data$Temperature, main="Temperature")
boxplot(data$Humidity, main="Humidity")
boxplot(data$Windspeed, main="Windspeed")
boxplot(data$Visibility, main="Visbility")
boxplot(data$Dewpoint, main="DewPointTemp")
boxplot(data$SolarRadiation, main="SolarRadiation")
boxplot(data$Rainfall, main="Rainfall")
boxplot(data$Snowfall, main="Snowfall")
#SolarRadiation is positively skewed.
#However, Rainfall is a bit skewed to right. Outliers effect might be prominent
#These variables can be a problem if included in actual model.

boxplot(RentedBikeCount ~ Seasons, data=data, main="RentedBike by Seasons") #No Pattern
boxplot(RentedBikeCount ~ Holiday, data=data, main="RentedBike by Holiday") #Linear Pattern
boxplot(RentedBikeCount ~ FunctioningDay, data=data, main="RentedBike by FunctioningDay")
table(data$Seasons)
table(data$Holiday)
table(data$FunctioningDay)
#All of these catagorical variables have enough sample size
table(data[, c(11,12)])
table(data[, c(11,13)])
table(data[, c(12,13)])
#Seems good apart from table between Seasons and Functioning Day.
#Two of the combinations of these variables do not have any sample 
#(No FunctioningDay of Summer and Winter )
#I would not have considered any interaction between these two even if my sample size was smaller.
#However my overall sample size is large, so I will not exclude this interaction if the step function suggest.
par(mfcol=c(1,1))
pairs(data[, -c(11,12,13)])
#Many predictors are positively related and some response vs predictor plot shows non-linear pattern
cor(data[, -c(11,12,13)])#Strong Colrel betwn Dewpoint-Temp(0.91),Humidity-Dewpoint(0.53), 
#Humidity-Visibility(-0.54), Humidity-SolarRadiation(-0.46)

#Model Diagnostic

fullfit <- lm(RentedBikeCount ~ Hour + Temperature + Humidity + Windspeed + Visibility + Dewpoint + SolarRadiation 
              + Rainfall + Snowfall+ factor(Seasons) + factor(Holiday) + factor(FunctioningDay), data = data)
summary(fullfit) #I don't see any Nan value, which means that no two predictors are perfectly inter-correlated
#R squared is 0.5504, 0.5497
plot(fullfit$fitted.values, studres(fullfit), xlab="Fitted Values", ylab="Studentized Residuals", main="Full 
Fitted Model Residual Plot")
abline(h=c(0, 2, -2))
#Presence of Funnel Shape as well as a pattern in fitted value vs Error, 
#seems like presence of Non-linearity & Heteroscedasticity

#Individual Predictors vs Residuals Analysis
par(mfcol=c(1,4))
plot(data$Hour, studres(fullfit), xlab="Hour", ylab="Studentized Residuals", main="Hour vs Residuals")
abline(h=c(0, 3, -3)) #unclear pattern
plot(data$Temperature, studres(fullfit), xlab="Temperature", ylab="Studentized Residuals", main="Temperature vs Residuals")
abline(h=c(0, 3, -3))#Funnel shaped, Heteroscedastic
plot(data$Humidity, studres(fullfit), xlab="Humidity", ylab="Studentized Residuals", main="Humidity vs Residuals")
abline(h=c(0, 3, -3))
plot(data$Windspeed, studres(fullfit), xlab="Windspeed", ylab="Studentized Residuals", main="Windspeed vs Residuals")
abline(h=c(0, 3, -3)) #Reverse Funnel Shape, Heteroscedastic, but not strong
plot(data$Visibility, studres(fullfit), xlab="Visibility", ylab="Studentized Residuals", main="Visibility vs Residuals")
abline(h=c(0, 3, -3))#There is a pattern but not clear
plot(data$Dewpoint, studres(fullfit), xlab="Dewpoint", ylab="Studentized Residuals", main="Dewpoint vs Residuals")
abline(h=c(0, 3, -3)) #Funnel Shaped, Heteroscedastic
plot(data$SolarRadiation, studres(fullfit), xlab="SolarRadiation", ylab="Studentized Residuals", main="SolarRadiation vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, a reverse funnel shape has formed
plot(data$Rainfall, studres(fullfit), xlab="Rainfall", ylab="Studentized Residuals", main="Rainfall vs Residuals")
abline(h=c(0, 3, -3)) #Outlier effect is clearly visible and highly heteroscedastic
plot(data$Snowfall, studres(fullfit), xlab="Snowfall", ylab="Studentized Residuals", main="Snowfall vs Residuals")
abline(h=c(0, 3, -3)) #Highly heteroscedastic and has outlier effect
boxplot(studres(fullfit)~data$Seasons, main='Seasons vs Residuals') #Seems fine, slight heteroscedasticity
boxplot(studres(fullfit)~data$Holiday, main='Holiday vs Residuals') #Seems fine, slight heteroscedasticity
boxplot(studres(fullfit)~data$FunctioningDay, main='FunctioningDay vs Residuals') #Seems fine, slight heteroscedasticity
#Most of these heterscedasticity can be due to non-linearity as we saw in pairs plot

#Normality of Residual Analysis
par(mfcol=c(1,1))
qqnorm(fullfit$residuals)
qqline(fullfit$residuals) #departure in right tail almost from the midway, 
#indicating non-normality of errors
norm <- qqnorm(fullfit$residuals)
cor(norm$x, norm$y) #97.51< 98.5%)


dataseq<-c(1:nrow(data))
plot(dataseq, studres(fullfit), xlab='Time Sequence', ylab='Residuals', main='Checking Independence of Residuals')
#No clear pattern, we can say residuals are independent from one another


plot(cooks.distance(fullfit), type='b')
cooks.distance(fullfit)[which.max(cooks.distance(fullfit))]
#the highest value is 0.1055, so nothing to worry about influentiality

vif(fullfit)
# Have to drop DewPoint as it has 100+ VIF value, vif=117.30
#It was expected as Dewpoint had strong correlation with some predictors

#Drop Dewpoint from Dataset
newdata<- data[,-7]

#Model Diagnostic 2

fullfit <- lm(RentedBikeCount ~ Hour + Temperature + Humidity + Windspeed + Visibility + SolarRadiation 
              + Rainfall + Snowfall+ factor(Seasons) + factor(Holiday) + factor(FunctioningDay), data = newdata)
summary(fullfit) #R square remains almost same
plot(fullfit$fitted.values, studres(fullfit), xlab="Fitted Values", ylab="Studentized Residuals", main="Full 
Fitted Model Residual Plot")
abline(h=c(0, 3, -3))
#Heteroscedasticity still remains as the plot remains same as before


#Individual Predictors vs Residuals Analysis
par(mfcol=c(1,2))
plot(newdata$Hour, studres(fullfit), xlab="Hour", ylab="Studentized Residuals", main="Hour vs Residuals")
abline(h=c(0, 3, -3)) #Unclear Pattern
plot(newdata$Temperature, studres(fullfit), xlab="Temperature", ylab="Studentized Residuals", main="Temperature vs Residuals")
abline(h=c(0, 3, -3))#Heteroscedastic, funnel shaped
plot(newdata$Humidity, studres(fullfit), xlab="Humidity", ylab="Studentized Residuals", main="Humidity vs Residuals")
abline(h=c(0, 3, -3))
plot(newdata$Windspeed, studres(fullfit), xlab="Windspeed", ylab="Studentized Residuals", main="Windspeed vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, but not strong
plot(newdata$Visibility, studres(fullfit), xlab="Visibility", ylab="Studentized Residuals", main="Visibility vs Residuals")
abline(h=c(0, 3, -3))#There is a pattern but not clear,cannot decide it to be heterosc
plot(newdata$SolarRadiation, studres(fullfit), xlab="SolarRadiation", ylab="Studentized Residuals", main="SolarRadiation vs Residuals")
abline(h=c(0, 3, -3)) #Funnel Shapped, Heteroscedastic
plot(newdata$Rainfall, studres(fullfit), xlab="Rainfall", ylab="Studentized Residuals", main="Rainfall vs Residuals")
abline(h=c(0, 3, -3)) #Outlier effect is clearly visible and highly heteroscedastic
plot(newdata$Snowfall, studres(fullfit), xlab="Snowfall", ylab="Studentized Residuals", main="Snowfall vs Residuals")
abline(h=c(0, 3, -3)) #Highly heteroscedastic and has outlier effect
boxplot(studres(fullfit)~newdata$Seasons, main='Seasons vs Residuals') #Seems fine
boxplot(studres(fullfit)~newdata$Holiday, main='Holiday vs Residuals') #Seems fine
boxplot(studres(fullfit)~newdata$FunctioningDay, main='FunctioningDay vs Residuals') #Seems fine
#All plots are almost same as before


#Nonrmality of residuals
par(mfcol=c(1,1))
qqnorm(fullfit$residuals)
qqline(fullfit$residuals) #departure in right tail almost from the midway, 
#indicating non-normality of errors
norm <- qqnorm(fullfit$residuals)
cor(norm$x, norm$y) #97.50< 99.5%)

dataseq<-c(1:nrow(newdata))
plot(dataseq, studres(fullfit)) #Same plot as before, independent errors

plot(cooks.distance(fullfit), type='b')
cooks.distance(fullfit)[which.max(cooks.distance(fullfit))]
#highest value is 0.118, so nothing to worry about influentiality
vif(fullfit) #Seems okay now

#To fix heteroscedasticity due to non-linearity and non-normality of error, lets do log transformation of response
newdata$logRentedBike <- log(newdata$RentedBikeCount)
fullfit <- lm(logRentedBike ~ Temperature + Humidity + Windspeed + Visibility + SolarRadiation 
              + Rainfall + Snowfall+ factor(Seasons) + factor(Holiday) + factor(FunctioningDay), data = data)
#Got error due to presence of negative inf value in logRentedBike col, 
#which directed to the fact that actual response is having many values equal to 0
nrow(data[data$RentedBikeCount == 0,])  #295 rows with response=0

#Those 0 values can also be useful, 
#as we need to know and include when the demand for bike is absent as well. 
#We cannot drop it, so lets try the square root transformation of response
#We cannot have any negative value as response (as it is count of bikes)
#So, square root transformation is fine

newdata$sqrtRentedBike <- sqrt(newdata$RentedBikeCount)

hist(newdata$RentedBikeCount)
hist(newdata$sqrtRentedBike) #much more symmetric and better than actual values distribution
boxplot(newdata$sqrtRentedBike) #Box plot also seems perfect, almost 0 outliers

#Model Diagnostic 3

fullfit <- lm(sqrtRentedBike ~ Hour + Temperature + Humidity + Windspeed + Visibility + SolarRadiation 
              + Rainfall + Snowfall+ factor(Seasons) + factor(Holiday) + factor(FunctioningDay), data = newdata)
summary(fullfit) #Much improved R square 0.65
par(mfcol=c(1,1))
plot(fullfit$fitted.values, studres(fullfit), xlab="Fitted Values", ylab="Studentized Residuals", main="Full 
Fitted Model Residual Plot")
abline(h=c(0, 2, -2))
#For a few data points the a linear negative correlation pattern is still there, 
#however for most of the dataset, heteroscedasticity is resolved

#Individual Predictors vs Residuals Analysis
par(mfcol=c(1,2))
plot(newdata$Hour, studres(fullfit), xlab="Hour", ylab="Studentized Residuals", main="Hour vs Residuals")
abline(h=c(0, 3, -3))
plot(newdata$Temperature, studres(fullfit), xlab="Temperature", ylab="Studentized Residuals", main="Temperature vs Residuals")
abline(h=c(0, 3, -3))#No pattern
plot(newdata$Humidity, studres(fullfit), xlab="Humidity", ylab="Studentized Residuals", main="Humidity vs Residuals")
abline(h=c(0, 3, -3))
plot(newdata$Windspeed, studres(fullfit), xlab="Windspeed", ylab="Studentized Residuals", main="Windspeed vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, but not strong
plot(newdata$Visibility, studres(fullfit), xlab="Visibility", ylab="Studentized Residuals", main="Visibility vs Residuals")
abline(h=c(0, 3, -3))#Unclear Pattern
plot(newdata$SolarRadiation, studres(fullfit), xlab="SolarRadiation", ylab="Studentized Residuals", main="SolarRadiation vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, funnel shape
plot(newdata$Rainfall, studres(fullfit), xlab="Rainfall", ylab="Studentized Residuals", main="Rainfall vs Residuals")
abline(h=c(0, 3, -3)) #Outlier effect still remains-heteroscedastic, but pattern is weaker than before
plot(newdata$Snowfall, studres(fullfit), xlab="Snowfall", ylab="Studentized Residuals", main="Snowfall vs Residuals")
abline(h=c(0, 3, -3)) #Outlier effect still remains- heteroscedastic
boxplot(studres(fullfit)~newdata$Seasons, main='Seasons vs Residuals') #Seems fine
boxplot(studres(fullfit)~newdata$Holiday, main='Holiday vs Residuals') #Seems fine
boxplot(studres(fullfit)~newdata$FunctioningDay, main='FunctioningDay vs Residuals') #Seems fine, better than before

#Checking Normality of residuals
par(mfcol=c(1,1))
qqnorm(fullfit$residuals)
qqline(fullfit$residuals) #much better than before, 
#some systematic depurture in right tail, due to outliers probably 
norm <- qqnorm(fullfit$residuals)
cor(norm$x, norm$y) #0.9960 indicates that it residuals are normal now

dataseq<-c(1:nrow(data))
plot(dataseq, studres(fullfit)) #Independence of residuals over time, better than before

plot(cooks.distance(fullfit), type='b')
cooks.distance(fullfit)[which.max(cooks.distance(fullfit))]
#highest value seems like 0.3453, so nothing to worry about influentiality
vif(fullfit) #Seems fine, all bellow 10

#Presence of some outlier effects and slight but negligable non-linearity in some predictors vs residuals plots, 
#negligable non-linearity in fittedvalue vs residual plot
#Having said that, I think we can proceed to model building with current updates
#Limitations of the study: Outlier effects

cor(newdata[, -c(1,10,11,12,13)]) #Visibility:Humidity & Humidity:SolarRadiation 
#are concerning, everything else is fine
table(newdata[, c(10,11)])
table(newdata[, c(11,12)])
table(newdata[, c(10,12)]) #Seasons: FunctioningDay is a concern, everyhting else is fine

#Model Building 

fit0 <- lm(sqrtRentedBike ~ 1, data = newdata)
step(fit0, sqrtRentedBike ~ Hour + Temperature + Humidity + Windspeed + Visibility + SolarRadiation 
     + Rainfall + Snowfall+ factor(Seasons) + factor(Holiday) + factor(FunctioningDay), 
     direction = "both", trace = 1)
#Dropped Snowfall and Visibility
fitmain<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
              Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
              SolarRadiation + Windspeed, data = newdata)
#Let go forward and include possible two way interactions of these predictors
step(fitmain, scope = .~.^2,
     direction = "both", trace = 0)
fitfinal<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
               Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
               Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
               factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
               factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay) + 
               factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Temperature:Rainfall + factor(FunctioningDay):factor(Seasons) + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)

vif(fitfinal)
#Showing error message as two predictors are having almost perfect linear relation (aliased coefficients)
#(proabably Seasons:FunctioningDay), making it impossible to estimate their individual effects
#However, this message was not shown while fitting fitmain
#To solve the error and seeing a probability of strong correlation between two categorical variables
#I decided to drop their interaction, but kept the main effect as sample size is large
#And both the predictors(FunctioningDay and Seasons) are important for our study

fitfinal<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
               Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
               Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
               factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
               factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay) + 
               factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Temperature:Rainfall + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)





#Final Diagnostic

vif(fitfinal) #Now it is running without error. 
#However, many big GVIF values
vif(fitmain) #vif of only main effects seems fine
#Rainfall (while interactions are in the model) is having the highest GVIF = 48^2 
#which is due to structural multicolinearity
#Some Interaction term are also showing big figure, however, all these can be ignored

summary(fitfinal) #R squared is much better with 0.7426,0.7411 value
par(mfcol=c(1,1))
plot(fitfinal$fitted.values, studres(fitfinal), xlab="Fitted Values", ylab="Studentized Residuals", main="Final 
Fitted Model Residual Plot")
abline(h=c(0, 2, -2))
#For few data points: The length of the previously found linear negative correlation line has declined, 
#However for most of the dataset, heteroscedasticity was resolved before, now its almost homoscedastic
#Overall, this graph is much better than previous diagnostic graph
#Indicating that it is so far best model

#Individual Predictors vs Residuals Analysis
par(mfcol=c(1,3))
plot(newdata$Hour, studres(fitfinal), xlab="Hour", ylab="Studentized Residuals", main="Hour vs Residuals")
abline(h=c(0, 3, -3))
plot(newdata$Temperature, studres(fitfinal), xlab="Temperature", ylab="Studentized Residuals", main="Temperature vs Residuals")
abline(h=c(0, 3, -3))#No pattern
plot(newdata$Humidity, studres(fitfinal), xlab="Humidity", ylab="Studentized Residuals", main="Humidity vs Residuals")
abline(h=c(0, 3, -3))
plot(newdata$Windspeed, studres(fitfinal), xlab="Windspeed", ylab="Studentized Residuals", main="Windspeed vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, but not strong
plot(newdata$SolarRadiation, studres(fitfinal), xlab="SolarRadiation", ylab="Studentized Residuals", main="SolarRadiation vs Residuals")
abline(h=c(0, 3, -3)) #Heteroscedastic, funnel shape
plot(newdata$Rainfall, studres(fitfinal), xlab="Rainfall", ylab="Studentized Residuals", main="Rainfall vs Residuals")
abline(h=c(0, 3, -3)) #Outlier effect still remains-heteroscedastic, but pattern is weaker than before
boxplot(studres(fitfinal)~newdata$Seasons, main='Seasons vs Residuals') #Seems fine
boxplot(studres(fitfinal)~newdata$Holiday, main='Holiday vs Residuals') #Seems fine
boxplot(studres(fitfinal)~newdata$FunctioningDay, main='FunctioningDay vs Residuals') #Seems fine

#Checking Normality of residuals
par(mfcol=c(1,1))
qqnorm(fitfinal$residuals)
qqline(fitfinal$residuals) #worse than previous diagnostic due to including interaction probably, 
#some departure in both tail but not systematic, due to outliers probably 
norm <- qqnorm(fitfinal$residuals)
cor(norm$x, norm$y) #0.9893 indicates that residuals are almost normal

dataseq<-c(1:nrow(newdata))
plot(dataseq, studres(fitfinal)) #Independence of residuals over time: Seems fine

plot(cooks.distance(fitfinal), type='b')
cooks.distance(fitfinal)[which.max(cooks.distance(fitfinal))]
#Max cooks distance is 0.63 < 1, it is not a matter of concern

summary(fitfinal) #Open for interpretation
summary(fitmain) #All predictors beta seems significantly different from 0 
#in main effect except for Windspeed #0.65 R squared
#So model with both interaction and main effects seems to be better model 
#in terms of explanation of the variability.
#FunctioningDay, Humidity, Seasons, Ranifall and Holiday 
#seems significantly associated to square root of rented bike counts 
#in both main effect and interaction models
#However we can only be sure after checking both main effect and interactions 
#related to a specific variable
#There is no term which only has a main effect but no interactions

#Testing for FunctioningDay predictability
fit3<-lm(formula = sqrtRentedBike ~ Temperature + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
               Temperature:Hour + Humidity:factor(Seasons) + 
               Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
              + Humidity:Rainfall + Temperature:Humidity + 
               factor(Seasons):SolarRadiation + 
              + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Temperature:Rainfall + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)

anova(fit3, fitfinal) #P-value: 2.2e-16 *** (FunctioningDay is Significantly associated to rented bike counts)



#Testing for Humidity predictability
fit4<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
               Temperature:Hour + factor(FunctioningDay):Hour + Temperature:SolarRadiation + 
               factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay) + 
               factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               factor(Holiday):SolarRadiation +
               Temperature:Rainfall + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)
anova(fit4, fitfinal) #P-value< 2.2e-16 *** (Humidity is Significantly associated to rented bike counts)


#Testing for Seasons predictability
fit5<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
               Hour + Humidity + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + 
               Temperature:Hour + factor(FunctioningDay):Hour+ 
               Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
               factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
               Temperature:factor(FunctioningDay) + 
               factor(FunctioningDay):Rainfall 
               + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Temperature:Rainfall + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)
anova(fit5, fitfinal) #P-value< 2.2e-16 *** (Seasons is Significantly associated to rented bike counts)

#Testing for Rainfall predictability
fit6<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
           Hour + Humidity + factor(Seasons) + factor(Holiday) + 
           SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
           Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
           Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
           factor(FunctioningDay):Humidity + Temperature:Humidity + 
           factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay)+ 
           factor(Seasons):Windspeed + SolarRadiation:Windspeed + 
           Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
           Hour:factor(Holiday) + Hour:factor(Seasons) + 
           Temperature:Windspeed + Temperature:factor(Holiday), 
         data = newdata)
anova(fit6, fitfinal) #P-value< 2.2e-16 *** (Rainfall is Significantly associated to rented bike counts)


#Testing for Holiday predictability
fit7<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
           Hour + Humidity + factor(Seasons) + Rainfall + 
           SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
           Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
           Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
           factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
           factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay) + 
           factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
           factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
           Humidity:Windspeed + 
           Temperature:Rainfall + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
           Temperature:Windspeed + Hour:Rainfall, 
         data = newdata)
anova(fit7,fitfinal) #P-value< 2.2e-16 *** (Holiday is Significantly associated to rented bike counts)

#Testing for Temperature predictability
fit8<-lm(formula = sqrtRentedBike ~ factor(FunctioningDay) + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
               Hour:Humidity + Humidity:SolarRadiation + 
               factor(FunctioningDay):Humidity + Humidity:Rainfall + 
               factor(Seasons):SolarRadiation +
               factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Hour:Rainfall, 
             data = newdata)
anova(fit8,fitfinal) #P-value< 2.2e-16 *** (Temperature also is Significantly associated to rented bike counts)
#Indicate to the fact that interactions in this model has significant effect. Even if the main effect does not seem significant, 
#considering its associated interactions can make the overall predictor significant. 

#Testing for some significant interactions -Difference of the difference exists or not.
#Temperature: Humidity 
#Temperature:factor(FunctioningDay)Yes (No vs Yes)
#factor(Seasons)Winter:Rainfall (Autumn vs Winter)

fit9<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
                     Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
                     SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
                     Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
                     Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
                     factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
                     factor(Seasons):SolarRadiation + Temperature:factor(FunctioningDay) + 
                     factor(FunctioningDay):Rainfall + 
                     factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
                     Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
                     Temperature:Rainfall + 
                     Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
                     Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
                   data = newdata)
anova(fit9,fitfinal) #P-Value = 5.753e-10 ***   Either of seasons: rainfall significant.
#When testing for the seasons:rainfall either one to be significant, it is becoming significant and most probably the term is winter: rainfall
#Lets interpret Autumn vs Winter for Rainfall.

fit11<-lm(formula = sqrtRentedBike ~ Temperature + factor(FunctioningDay) + 
               Hour + Humidity + factor(Seasons) + Rainfall + factor(Holiday) + 
               SolarRadiation + Windspeed + Temperature:factor(Seasons) + 
               Temperature:Hour + factor(FunctioningDay):Hour + Humidity:factor(Seasons) + 
               Hour:Humidity + Humidity:SolarRadiation + Temperature:SolarRadiation + 
               factor(FunctioningDay):Humidity + Humidity:Rainfall + Temperature:Humidity + 
               factor(Seasons):SolarRadiation+ 
               factor(FunctioningDay):Rainfall + factor(Seasons):Rainfall + 
               factor(Seasons):Windspeed + Rainfall:Windspeed + SolarRadiation:Windspeed + 
               Humidity:Windspeed + factor(Holiday):SolarRadiation + Humidity:factor(Holiday) + 
               Temperature:Rainfall + 
               Hour:factor(Holiday) + Rainfall:SolarRadiation + Hour:factor(Seasons) + 
               Temperature:Windspeed + Hour:Rainfall + Temperature:factor(Holiday), 
             data = newdata)
anova(fit11,fitfinal) #P-value= 1.202e-08 *** Temparature: factor(FuctioningDay) Yes. 
#Its same as summary table. Significant.
