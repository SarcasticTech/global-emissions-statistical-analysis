#-----------------------------------------------------------#

# Importing & Normalizing the Data Set
greenhouse <- read.csv("Greenhouse Gas Full.csv")

#-----------------------------------------------------------#

# Installing & Importing Required Packages & Libraries
# install.packages("caret")
# install.packages("stats")
# install.packages("moments")
# install.packages("PerformanceAnalytics")
# install.packages("corrplot")
# install.packages("psych")
# install.packages("TTR")
# install.packages("forecast")
# install.packages("dplyr")
# install.packages("caTools")
# install.packages("ROCR") 
# install.packages("tidyverse")
# install.packages("fpp2")
# install.packages("datarium")
# install.packages("qqplotr")
# install.packages("ggplot2")
library(caret)
library(stats)
library(moments)
library(PerformanceAnalytics)
library(corrplot)
library(psych)
library(TTR)
library(forecast)
library(dplyr)
library(caTools)
library(ROCR) 
library(car)
library(tidyverse)
library(fpp2)
library(ggplot2) 
library(datarium) 
library(qqplotr)
library(RVAideMemoire)

#-----------------------------------------------------------#

# Data Preparation

# Normalizing the Data Set
# greenhouse <- greenhouse %>% mutate_each_(list(~scale(.) %>% as.vector),
#                                           vars = c("CO2.Value","CH4.Value","N2O.Value",
#                                                    "HFCs.Value", "PFCs.Value", "SF6.Value",
#                                                    "NF3.Value", "GHG.Value", "GDP"))
# Creating Outlier Function (Optional)
# outliers <- function(x) {
# 
#   Q1 <- quantile(x, probs=.25)
#   Q3 <- quantile(x, probs=.75)
#   iqr = Q3-Q1
# 
#   upper_limit = Q3 + (iqr*1.5)
#   lower_limit = Q1 - (iqr*1.5)
# 
#   x > upper_limit | x < lower_limit
# }

# Alternate Version - 1 Outlier (Optional)
# remove_outliers <- function(greenhouse, cols = names(greenhouse)) {
#   for (col in cols) {
#     greenhouse <- greenhouse[!outliers(greenhouse[[col]]),]
#     }
#   greenhouse
#   }
# green_fixed <- remove_outliers(greenhouse, c('CO2.Value', 'CH4.Value',
#                                                 'N2O.Value', 'HFCs.Value',
#                                                 'PFCs.Value', 'SF6.Value',
#                                                 'NF3.Value', 'Total.Emissions',
#                                                 'Main.Emissions',
#                                                 'Industrial.Emissions',
#                                                 'GDP'))
# boxplot(green_fixed[, c(3:13)])

# Identifying Data Outliers by Country via Boxplots
boxplot(CO2.Value ~ Country, data = greenhouse, las = 2)
boxplot(CH4.Value ~ Country, data = greenhouse, las = 2)
boxplot(N2O.Value ~ Country, data = greenhouse, las = 2)
boxplot(HFCs.Value ~ Country, data = greenhouse, las = 2)
boxplot(PFCs.Value ~ Country, data = greenhouse, las = 2)
boxplot(SF6.Value ~ Country, data = greenhouse, las = 2)
boxplot(NF3.Value ~ Country, data = greenhouse, las = 2)
boxplot(GHG.Value ~ Country, data = greenhouse, las = 2)
boxplot(GDP ~ Country, data = greenhouse, las = 2)

# Part 1 - Descriptive Statistical Analysis

# Reading the Data
print(greenhouse)
names(greenhouse)
head(greenhouse)
tail(greenhouse)
summary(greenhouse)
describe(greenhouse)
str(greenhouse)
is.null(greenhouse)

## Version 1 - Summarized Variables Analysis (Not by Country) ##
# Selecting Variables for Analysis
greenhouse_co2 <- (greenhouse$CO2.Value)
greenhouse_ch4 <- (greenhouse$CH4.Value)
greenhouse_n2o <- (greenhouse$N2O.Value)
greenhouse_hfc <- (greenhouse$HFCs.Value)
greenhouse_pfc <- (greenhouse$PFCs.Value)
greenhouse_sf6 <- (greenhouse$SF6.Value)
greenhouse_nf3 <- (greenhouse$NF3.Value)
greenhouse_ghg <- (greenhouse$GHG.Value)
greenhouse_gdp <- (greenhouse$GDP)

# Identifying Min & Max of Variables
range(greenhouse_co2)
range(greenhouse_ch4)
range(greenhouse_n2o)
range(greenhouse_hfc)
range(greenhouse_pfc)
range(greenhouse_sf6)
range(greenhouse_nf3)
range(greenhouse_ghg)
range(greenhouse_gdp)

# Identifying Mean & Median
mean(greenhouse_co2)
mean(greenhouse_ch4)
mean(greenhouse_n2o)
mean(greenhouse_hfc)
mean(greenhouse_pfc)
mean(greenhouse_sf6)
mean(greenhouse_nf3)
mean(greenhouse_ghg)
mean(greenhouse_gdp)

median(greenhouse_co2)
median(greenhouse_ch4)
median(greenhouse_n2o)
median(greenhouse_hfc)
median(greenhouse_pfc)
median(greenhouse_sf6)
median(greenhouse_nf3)
median(greenhouse_ghg)
median(greenhouse_gdp)

# Identifying Standard Deviation
sd(greenhouse_co2)
sd(greenhouse_ch4)
sd(greenhouse_n2o)
sd(greenhouse_hfc)
sd(greenhouse_pfc)
sd(greenhouse_sf6)
sd(greenhouse_nf3)
sd(greenhouse_ghg)
sd(greenhouse_gdp)

# Identifying Skewness & Kurtosis
skewness(greenhouse_co2)
skewness(greenhouse_ch4)
skewness(greenhouse_n2o)
skewness(greenhouse_hfc)
skewness(greenhouse_pfc)
skewness(greenhouse_sf6)
skewness(greenhouse_nf3)
skewness(greenhouse_ghg)
skewness(greenhouse_gdp)

kurtosis(greenhouse_co2)
kurtosis(greenhouse_ch4)
kurtosis(greenhouse_n2o)
kurtosis(greenhouse_hfc)
kurtosis(greenhouse_pfc)
kurtosis(greenhouse_sf6)
kurtosis(greenhouse_nf3)
kurtosis(greenhouse_ghg)
kurtosis(greenhouse_gdp)

## Version 2 of Descriptive Analysis of All Variables by Country ##
aggregate(greenhouse[, c(2:11)], list(greenhouse$Country), FUN=range)
aggregate(greenhouse[, c(3:11)], list(greenhouse$Country), FUN=mean)
aggregate(greenhouse[, c(3:11)], list(greenhouse$Country), FUN=median)
aggregate(greenhouse[, c(3:11)], list(greenhouse$Country), FUN=sd)
aggregate(greenhouse[, c(3:11)], list(greenhouse$Country), FUN=kurtosis)
aggregate(greenhouse[, c(3:11)], list(greenhouse$Country), FUN=skewness)

# Identifying Mode by Creating an Additional Function
# Variables are Numeric - Hence No Re-Occuring Values for Output
# Hashed for Reference
# mode <- function(v){
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# mode(greenhouse_ghg)
# mode(greenhouse_gdp)
# aggregate(greenhouse, list(greenhouse$Country), FUN=mode)

#-----------------------------------------------------------#

# Part 2 - Correlation Analysis

# Sample Pearson Correlation Between Two Variables
cor(greenhouse$GHG.Value, greenhouse$GDP, method = "pearson")

# Sample Spearman Correlation Between Two Variables
cor(greenhouse$GHG.Value, greenhouse$GDP, method = "spearman")

# Sample Kendall Correlation Between Two Variables
cor(greenhouse$GHG.Value, greenhouse$GDP, method = "kendall")

# Selecting & Creating Correlation from Data Sets
greenhouse_full <- cor(greenhouse[, c(3:11)], method = "kendall")

# Displaying Correlation
greenhouse_full

# Identifying P-Values
g.mat <- corr.test(greenhouse_full)$p
g.mat

# Version 1 Correlation Matrix
corrplot(greenhouse_full,
         method = "pie",
         type = "upper")

# Ruling Out Insignificant
corrplot(greenhouse_full,
         method = "pie",
         p.mat = g.mat,
         type = "upper")

# Version 2 Correlation Matrix
corrplot(greenhouse_full,
         method = "number",
         type = "upper")

# Ruling Out Insignificant
corrplot(greenhouse_full,
         method = "number",
         p.mat = g.mat,
         type = "upper")

# Version 3 - Difference in Correlation of Countries
# Comparing Correlation Between Japan & Italy Sample
# Selecting Countries
cor_japan <- greenhouse[is.element(greenhouse$Country, c('Japan')),]
cor_italy <- greenhouse[is.element(greenhouse$Country, c('Italy')),]

# Creating Correlation
cor_japanc <- cor(cor_japan[, c(3:11)], method = "kendall")
cor_italyc <- cor(cor_italy[, c(3:11)], method = "kendall")

# Correlation Matrix
corrplot(cor_japanc,
         method = "number",
         type = "upper")

# Correlation Matrix
corrplot(cor_italyc,
         method = "number",
         type = "upper")

#-----------------------------------------------------------#

# Part 3 - Regression Analysis

# Selecting variables for Regression by Country for Accurate Results
green_usa <- greenhouse[is.element(greenhouse$Country, c('United States of America')),]

green_usa_cor <- cor(green_usa[, c(3:11)], method = "kendall")

corrplot(green_usa_cor,
         method = "number",
         type = "upper")

# Single Linear Regression
# Analysis by Forward Stepwise
model_1 <-lm(GDP ~ SF6.Value, green_usa)
summary.lm(model_1)

# Identified Variables for Regression
# Checking Assumptions by Plots
plot(GDP ~ SF6.Value, green_usa, col = "blue",
     main = "Regression: GDP & SF6.Value",
     xlab = "SF6.Value",
     ylab = "GDP")
abline(model_1, col="red")

plot(model_1, 1)
plot(model_1, 2)
plot(model_1, 3)

# Assumptions Approved - Report Results
# Predict GDP via SF6.Value
# GDP = 28740 + 2.245 x SF6.Value = GDP
# GDP = 28740 + 2.245 x 69 (Sample Size) = 28894.905

# Multiple Linear Regression
# Analysis by Forward Stepwise
model_2 <-lm(GDP ~  CH4.Value + PFCs.Value + SF6.Value, green_usa)
summary.lm(model_2)

# Identified Significant Variables for Regression
# Checking Assumptions by Plots
model_3 <-lm(GDP ~ CH4.Value + SF6.Value, green_usa)
summary.lm(model_3)
pairs(green_usa[,c(11,4,8)], lower.panel = NULL, pch = 19, cex = 0.2)
plot(model_3, 1)
plot(model_3, 2)
plot(model_3, 3)

# No Collinearity Identified
vif(model_3)

# Assumptions Approved - Report Results
# Predict GDP via CH4.Value + SF6.Value
# GDP = (-31330) + 0.09484 x CH4.Value + 1.805 x SF6.Value = GDP

#-----------------------------------------------------------#

# Part 4 - Time Series Analysis

# Grouping Variables by Highest & Lowest Emission Countries
green_high <- greenhouse[is.element(greenhouse$Country, c('United States of America', 'Japan',
                                                          'Germany', 'Canada', 'United Kingdom',
                                                          'Italy')),]

green_low <- greenhouse[is.element(greenhouse$Country, c('Ireland', 'Denmark',
                                                         'Sweden', 'Norway', 'Switzerland',
                                                         'Iceland')),]

# Creating Forecast Error Function
plotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  
  
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

## Version 1 of Time Series with Highest Emission Countries ##

# Plotting Time Series
green_high_mean <- green_high %>%
  group_by(Year) %>%
  summarise(across(c(GHG.Value),list(mean=mean)))

print(green_high_mean)

green_high_mean <- ts(green_high_mean, frequency = 1, start = c(2000))
green_high_mean <- green_high_mean[,-1]

plot.ts(green_high_mean)

# # HoltWinters Model
# # Applying Simple Exponential Smoothing
# green_high_v1 <- ts(green_high_mean, frequency = 1, start = c (2000))
# green_high_v2 <- HoltWinters(green_high_v1, beta = FALSE, gamma = FALSE)
# green_high_v2
# 
# green_high_v2$fitted
# plot(green_high_v2)
# green_high_v2$SSE
# 
# # Creating Forecast
# green_high_v3 <- forecast(green_high_v2, h=8)
# green_high_v3
# plot(green_high_v3)
# 
# # Identifying Forecast Error
# acf(green_high_v3$residuals, lag.max = 20, na.action = na.pass)
# Box.test(green_high_v3$residuals, lag = 20, type="Ljung-Box")
# 
# plot.ts(green_high_v3$residuals)
# green_high_v3$residuals <- green_high_v3$residuals[!is.na(green_high_v3$residuals)]
# plotForecastErrors(green_high_v3$residuals)

# ARIMA Model for Highest Emission Countries (2,0,0)
# Difference Time Series
green_high_skirts <- diff(green_high_mean, differences=1)
plot.ts(green_high_skirts)

green_high_skirts2 <- diff(green_high_skirts, differences=2)
plot.ts(green_high_skirts2)

# Creating Correlogram & Partial Correlogram
acf(green_high_skirts2, lag.max = 20)
acf(green_high_skirts2, lag.max = 20, plot = FALSE)

pacf(green_high_skirts2, lag.max = 20)
pacf(green_high_skirts2, lag.max = 20, plot = FALSE)

# Identifying ARIMA Model
auto.arima(green_high_skirts2)

# Fitting ARIMA Model
green_high_arima <- arima(green_high_skirts2, order = c (2,0,0))
green_high_arima

# Plotting ARIMA Forecast
green_high_arima <- forecast(green_high_arima, h = 8)
plot(green_high_arima)

# Plotting ARIMA Errors
plot.ts(green_high_arima$residuals)
plotForecastErrors(green_high_arima$residuals)
mean(green_high_arima$residuals)

## Version 2 of Time Series with Lowest Emission Countries ##

# Plotting Time Series
green_low_mean <- green_low %>%
  group_by(Year) %>%
  summarise(across(c(GHG.Value),list(mean=mean)))

print(green_low_mean)

green_low_mean <- ts(green_low_mean, frequency = 1, start = c(2000))
green_low_mean <- green_low_mean[,-1]

plot.ts(green_low_mean)

# # HoltWinters Model
# # Applying Simple Exponential Smoothing
# green_low_v1 <- ts(green_low_mean, frequency = 1, start = c (2000))
# green_low_v2 <- HoltWinters(green_low_v1, beta = FALSE, gamma = FALSE)
# green_low_v2
# 
# green_low_v2$fitted
# plot(green_low_v2)
# green_low_v2$SSE
# 
# # Creating Forecast
# green_low_v3 <- forecast(green_low_v2, h=8)
# green_low_v3
# plot(green_low_v3)
# 
# # Identifying Forecast Error
# acf(green_low_v3$residuals, lag.max = 20, na.action = na.pass)
# Box.test(green_low_v3$residuals, lag = 20, type="Ljung-Box")
# 
# plot.ts(green_low_v3$residuals)
# 
# green_low_v3$residuals <- green_low_v3$residuals[!is.na(green_low_v3$residuals)]
# plotForecastErrors(green_low_v3$residuals)

# ARIMA Model for Lowest Emission Countries (4,0,0)
# Difference Time Series
green_low_skirts <- diff(green_low_mean, differences=1)
plot.ts(green_low_skirts)

green_low_skirts2 <- diff(green_low_skirts, differences=2)
plot.ts(green_low_skirts2)

# Creating Correlogram & Partial Correlogram
acf(green_low_skirts2, lag.max = 20)
acf(green_low_skirts2, lag.max = 20, plot = FALSE)

pacf(green_low_skirts2, lag.max = 20)
pacf(green_low_skirts2, lag.max = 20, plot = FALSE)

# Identifying ARIMA Model
auto.arima(green_low_skirts2)

# Fitting ARIMA Model
green_low_arima <- arima(green_low_skirts2, order = c (4,0,0))
green_low_arima

# Plotting ARIMA Forecast
green_low_arima <- forecast(green_low_arima, h = 8)
plot(green_low_arima)

# Plotting ARIMA Errors
plot.ts(green_low_arima$residuals)
plotForecastErrors(green_low_arima$residuals)
mean(green_low_arima$residuals)

#-----------------------------------------------------------#

# Part 5 - Comparative Analysis & Hypothesis Testing

## Version 1.1 - One-Sample T-Test for Highest Emission Countries ##
green_high_hy <- greenhouse[is.element(greenhouse$Country, c('United States of America', 'Japan',
                                                          'Germany', 'Canada', 'United Kingdom',
                                                          'Italy')),]
# Creating Mean for Group
green_high_hy_mean <- green_high_hy %>%
  group_by(Year) %>%
  summarise(across(c(GHG.Value),list(mean=mean)))

green_high_hy_mean1 <- mean(green_high_hy_mean$GHG.Value_mean)

# Checking Normality by Q-Q Plot
ggplot(mapping = aes(sample=green_high_hy_mean$GHG.Value_mean)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

# Histogram for Distribution
hist(green_high_hy_mean$GHG.Value_mean)

# T-Test = Identified P-Value: 0.5 - Null Not Rejected
t.test(green_high_hy_mean$GHG.Value_mean, mu = green_high_hy_mean1, alternative="less")

## Version 1.2 - One Sample T-Test for Highest Emission Countries - GDP ##
# Creating Mean for Group
green_high_hy_meanv1 <- green_high_hy %>%
  group_by(Year) %>%
  summarise(across(c(GDP),list(mean=mean)))

green_high_hy_meanv2 <- mean(green_high_hy_meanv1$GDP_mean)

# Checking Normality by Q-Q Plot
ggplot(mapping = aes(sample=green_high_hy_meanv1$GDP_mean)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

# Histogram for Distribution
hist(green_high_hy_meanv1$GDP_mean)

# T-Test = Identified P-Value: 0.5 - Null Not Rejected
t.test(green_high_hy_meanv1$GDP_mean, mu = green_high_hy_meanv2, alternative="less")

## Version 2.1 - One-Sample T-Test for Lowest Emission Countries ##
green_low_hy <- greenhouse[is.element(greenhouse$Country, c('Ireland', 'Denmark',
                                                            'Sweden', 'Norway', 'Switzerland',
                                                            'Iceland')),]
# Creating Mean for Group
green_low_hy_mean <- green_low_hy %>%
  group_by(Year) %>%
  summarise(across(c(GHG.Value),list(mean=mean)))

green_low_hy_mean1 <- mean(green_low_hy_mean$GHG.Value_mean)

# Checking Normality by Q-Q Plot
ggplot(mapping = aes(sample=green_low_hy_mean$GHG.Value_mean)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

# Histogram for Distribution
hist(green_low_hy_mean$GHG.Value_mean)

# T-Test = Identified P-Value: 0.5 - Null Not Rejected
t.test(green_low_hy_mean$GHG.Value_mean, mu = green_low_hy_mean1, alternative="less")

## Version 2.2 - One Sample T-Test for Highest Emission Countries - GDP ##
# Creating Mean for Group
green_low_hy_meanv1 <- green_low_hy %>%
  group_by(Year) %>%
  summarise(across(c(GDP),list(mean=mean)))

green_low_hy_meanv2 <- mean(green_low_hy_meanv1$GDP_mean)

# Checking Normality by Q-Q Plot
ggplot(mapping = aes(sample=green_low_hy_meanv1$GDP_mean)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

# Histogram for Distribution
hist(green_low_hy_meanv1$GDP_mean)

# T-Test = Identified P-Value: 0.5 - Null Not Rejected
t.test(green_low_hy_meanv1$GDP_mean, mu = green_low_hy_meanv2, alternative="less")

## Version 3 - Welch Two Sample T-Test on Emission Levels ##
# Convert Variable to Factor
greenhouse$Emission.Level <- as.factor(greenhouse$Emission.Level)

# Creating Boxplot for Comparison
boxplot(CO2.Value ~ Emission.Level, data = greenhouse)

# T-Test = Identified P-Value: 1.766e-14 - Null Rejected
t.test(CO2.Value ~ Emission.Level, greenhouse)

#-----------------------------------------------------------#

# Junkyard Code Section

#-----------------------------------------------------------#