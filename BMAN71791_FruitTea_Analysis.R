##################################################
#                BMAN 71791                   ####
#Applied Statistics and Business Forecasting  ####
#Fruit Tea Sales Analysis and Forecasting     ####
#Author: Bansi Kamani                         ####
##################################################
# Fruit Tea data

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

graphics.off() #CLEAR PLOTS 
rm(list = ls()) #CLEAR WORKSPACE (GLOBAL ENVIRONMENT) 
cat("\014") #CLEAR CONSOLE

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

###read the file###
# 1) Load data by providing path to data file
fruittea <- load(file="DataSets/Data_FruitTea.RData")  # Update path to your dataset
fruittea

###exploratory data analysis###
install.packages("MASS")
library(MASS)

attach(fruittea)
dim(fruittea)
summary(fruittea)
table(Region)
head(fruittea)

#check the correlation
cor.test(Sales..units.,Ad1..GRP.)
cor.test(Sales..units.,Ad2..No_of_banners.)
cor.test(Sales..units.,Prom..No_of_stores.)
cor.test(Sales..units.,Wage..Perc..)

#two figures for data exploration in the presentation slides
install.packages("ggplot2")
library(ggplot2)
ggplot(data = fruittea, aes(x = Region, y = Sales..units.)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Sales in different Region", x = "Region", y = "Sales units")

ggplot(data = fruittea, aes(x = Year, y = Sales..units.)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Sales in different Year", x = "Year", y = "Sales units")

###question 1###
transform_Prom <- Prom..No_of_stores.
transform_Ad1 <- (Ad1..GRP.)
transform_sales <- log(Sales..units.)

#create a new dataframe without wage, time, month, year)
trainingset<- data.frame(transform_sales, Ad2..No_of_banners.,transform_Prom, transform_Ad1, Product,Region)
rm(transform_sales,Ad2..No_of_banners., transform_Prom, transform_Ad1, Product, Region)
head(trainingset)

# 2) Initial linear model. 
fitted_mod<- lm(transform_sales ~ Ad2..No_of_banners.+transform_Prom + transform_Ad1  + Region, data = trainingset)
summary(fitted_mod)

# 3) How do the predicted values look like?- doubt as sir used something different.
plot(trainingset$Ad2..No_of_banners.,trainingset$transform_sales)
points(x = trainingset$transform_sales , y = fitted_mod$fitted.values,pch=18,col=rgb(0.5,0,0,.8),type="p")

# 4) Let's compute the residuals of the model.
residuals<- trainingset$transform_sales - predict.lm(object = fitted_mod,newdata = trainingset) #Remember that the predict function give us the predicted values from the model.

# 5) Plots of the residuals
# Index vs. residuals
plot(1:length(residuals), residuals,bty="n",main=NULL,pch=18,col=rgb(0.6,.7,.2,.9),cex.axis=2,cex=1,cex.main=1,cex.lab=3,xlab="Index",ylab="Residuals")
abline(h=0,col="red") #Creates a horizontal red line at y=0

#Fitted values vs. residuals
plot(fitted_mod$fitted.values, residuals,bty="n",main=NULL,pch=18,col=rgb(0.2,.7,.7,.9),cex.axis=2,cex=1,cex.main=3,cex.lab=3,xlab="Fitted values",ylab="residuals")
abline(h=0,col="red")

#Ad2..No_of_banners. vs. residuals (Figure 2)
plot(trainingset$Ad2..No_of_banners., residuals,bty="n",main=NULL,pch=18,col=rgb(0.3,.7,.2,.9),cex.axis=2,cex=1,cex.main=3,cex.lab=3,xlab="Ad2",ylab="residuals")
abline(h=0,col="red")

# homoscetascity is hold true in this graph
#transform_Prom vs. residuals (Figure 3)
plot(trainingset$transform_Prom, residuals,bty="n",main=NULL,pch=18,col=rgb(0.3,.7,.2,.9),cex.axis=2,cex=1,cex.main=3,cex.lab=3,xlab="Promotions",ylab="residuals")
abline(h=0,col="red")

#transform_Ad1  vs. residuals (Figure 1)
plot(trainingset$transform_Ad1 , residuals,bty="n",main=NULL,pch=18,col=rgb(0.3,.7,.2,.9),cex.axis=2,cex=1,cex.main=3,cex.lab=3,xlab="Ad1",ylab="residuals")
abline(h=0,col="red")

#QQplot for the residuals. (Figure 4)
qqnorm(y = fitted_mod$residuals,cex.axis=2,cex=1,cex.main=3,cex.lab=3) #Creates the qqplot for the residuals by comparint the quantiles of the residuals to the quantiles of a standard normal distribution. The expected plot should show the points in a line.
qqline(fitted_mod$residuals, col = "red", lwd = 2) #Adds the line in red where the points are expected to be located.
# u should know this plot


#normality assumption not being held
#Histogram of the residuals (Figure 5)
hist(residuals,bty="n",main=NULL,pch=18,col=rgb(0.2,.7,.7,.9),cex.axis=2,cex=1,cex.main=3,cex.lab=3,freq = FALSE)
points(x = seq(min(residuals),max(residuals),length.out = 100), dnorm(x = seq(min(residuals),max(residuals),length.out = 100),mean = 0, sd = sd(residuals)), col="red",type="l")
#u r expected to do histogram
#Linear Equation: log(Sales(units))= 0.0092097 * (Ad2..No_of_banners.)+ 0.0063052 * (transform_Prom) + 0.0194104 * (transform_Ad1) 

# Extract coefficients from the linear regression model
coefficients <- coef(fitted_mod)

# Coefficient for Ad2..No_of_banners
coefficient_Ad2 <- coefficients["Ad2..No_of_banners."]

# Coefficient for Prom..No_of_stores
coefficient_Prom <- coefficients["transform_Prom"]

# Coefficient for Ad1..GRP
coefficient_Ad1 <- coefficients["transform_Ad1"]

# Exponentiate coefficients if sales are log-transformed
coefficient_Ad2_exp <- exp(coefficient_Ad2)
coefficient_Prom_exp <- exp(coefficient_Prom)
coefficient_Ad1_exp <- exp(coefficient_Ad1)

# Print or use the exponentiated coefficients as needed
cat("Association between units sold and Ad2..No_of_banners (after exponentiation):", coefficient_Ad2_exp, "\n")
cat("Association between units sold and Prom..No_of_stores (after exponentiation):", coefficient_Prom_exp, "\n")
cat("Association between units sold and Ad1..GRP (after exponentiation):", coefficient_Ad1_exp, "\n")


# Aggregate total sales for each level of marketing activities
aggregate(transform_sales ~ Ad2..No_of_banners. + transform_Prom + transform_Ad1, data = trainingset, sum)


# Create a bar plot of total sales for each level of marketing activities
ggplot(data = aggregate(transform_sales ~ Ad2..No_of_banners. + transform_Prom + transform_Ad1, data = trainingset, sum),
       aes(x = transform_Ad1, y = transform_sales, fill = factor(Ad2..No_of_banners.))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Sales for Each Marketing Activity", x = "Marketing Activity", y = "Total Sales") +
  theme_minimal()
#put a range of adds
#add banners for add1 and ad3


###question 2###
#use the coefficient from fitted_mod in question 1
cost_tv = 2000000
cost_banners = 500000
coef_ad1 <- coefficient_Ad1
coef_ad2 <- coefficient_Ad2

#which one is cost-effective
eff_tv = coef_ad1*0.3 / cost_tv
eff_banners = coef_ad2*0.3 / cost_banners
if (eff_tv > eff_banners) {
  print("TV ads is cost-effective.")
} else if (eff_banners > eff_tv) {
  print("Banners is cost-effective.")
} else {
  print("TV ads and Banners have the same effect.")
}
#The answer is "Banners is cost-effective."

###question 3###
str(fruittea) #CLASSES
#CONVERT CHARACTER TO FACTOR
fruittea$Region<-as.factor(fruittea$Region)
fruittea$Month<-as.factor(fruittea$Month)
str(fruittea) #check classes again

summary(fruittea) #SUMMARY OF DATASET

# Assuming 'fruittea' is our original dataset, create dataset without variable product
new_fruittea <- fruittea[, -which(names(fruittea) == "Product")]

regression <- lm(Sales..units. ~ ., data = new_fruittea)#RUN THE REGRESSION
summary(regression) #SUMMARY OF REGRESSION

#~Model Selection~#
install.packages("olsrr")
library(olsrr)

#1st way - STEPWISE FORWARD REGRESSION
a <- 0.05 #SIGNIFICANCE level = 5%
ols_step_forward_p(regression, penter = a, details = FALSE)

# Variables selected based on stepwise forward selection
selected_variables <- c("Ad2..No_of_banners.", "Region", "Wage..Perc..", "Time")

# Create a linear regression model using the selected variables
regression_selected <- lm(Sales..units. ~ ., data = fruittea[, c("Sales..units.", selected_variables)])

# Print the summary of the new regression model
summary(regression_selected)

#~Requirements of Linear Regression~#
#COLLINEARITY CONDITION
library(car)
vif(regression_selected)


#LINEARITY CONDITION
r_standard<- rstandard(regression_selected) 
plot(regression_selected$fitted.values,r_standard,xlab="y_hat", ylab="errors_star", 
     col='black', main = paste("Plot: Y-Hat and Standard Errors"))
abline(h=0, col="red", lw=2, lty=1)

#HETEROSCEDASTICITY OF THE RESIDUALS
install.packages('lmtest')
library(lmtest)

#1st way:
plot(regression_selected$fitted.values,r_standard,xlab="y_hat", ylab="errors_star",
     col='black', main = paste("Plot: Y-Hat and Standard Errors"))
abline(h=0, col="red", lw=2, lty=1)

#2nd way:
bptest(regression_selected) #Breusch-Pagan Test
# => p-value = 5.046e-07

#Model transformation
lny <- log(new_fruittea$Sales..units.)
regression_lny <-lm(lny ~(Ad2..No_of_banners. + Region + Wage..Perc.. +Time), new_fruittea)
summary(regression_lny)
bptest(regression_lny)
#  p-value = 0.5092

sqrt_y <-sqrt(new_fruittea$Sales..units.)
regression_sqrt<-lm(sqrt_y~(Ad2..No_of_banners. + Region + Wage..Perc.. +Time), new_fruittea)
summary(regression_sqrt)
r_standard3 <- rstandard(regression_sqrt) 

bptest(regression_sqrt)
# => p-value = 8.772e-09

#transformation improve our model, specifically log

#NORMALITY OF THE RESIDUALS
#1st way:
hist(r_standard)
#transformed regression
rstandard_trans <- rstandard(regression_lny)
hist(rstandard_trans)
#The symmetry of the histogram suggests that the rstandard_trans values 
#are normally distributed. 

#2nd way: Q-Q plot
qqPlot(regression_lny, main="Q-Q Plot")

qqnorm(rstandard_trans)

#3rd way: Lillie Test
install.packages("nortest")
library("nortest")
lillie.test(rstandard_trans)

#INFLUENTIAL POINTS
library(olsrr)
ols_plot_dffits(regression_lny)
ols_plot_cooksd_bar(regression_lny)


summary(regression_lny)

#Data visualization for Q3

#scatterplot matrix #shows the relationship between region, wage perc and how it affects sales
pairs(~ `Sales..units.` + Region + Wage..Perc.., data = fruittea)

# Install and load necessary packages if not already installed
# install.packages("ggplot2")
library(ggplot2)


# Plot the data
#montly sales by region
ggplot(fruittea, aes(x = Month, y = Sales..units., fill = Region)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Sales of FruitTea by Region",
       x = "Month",
       y = "Sales (units)",
       fill = "Region") +
  theme_minimal()

#plot sales x region: US has most sales, followed by mexico and spain
ggplot(fruittea, aes(x = Region, y = Sales..units.)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_classic()
+ 
  labs(title = "Sales by Region", x = "Region", y = "Sales")


library(ggplot2)
#relationship between sales, wage perc, and region (Figure 6)
ggplot(fruittea, aes(x = Sales..units., y = Wage..Perc.., color = Region)) +
  geom_point() +
  facet_wrap(~ Region) +
  labs(title = "Relationship between Sales, Wage Percentage, and Region",
       x = "Sales (units)",
       y = "Wage Percentage")

# sales x month plot (Figure 7)
ggplot(fruittea, aes(x = Month, y = Sales..units.)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Monthly Sales",
       x = "Month",
       y = "Sales (units)")

#library(ggplot2)
#sales trend over months
ggplot(fruittea, aes(x = Month, y = Sales..units., group = 1)) +
  geom_line() +
  labs(title = "Sales Trend Over Months",
       x = "Month",
       y = "Sales (units)")

ggplot(fruittea, aes(x = Month, y = Sales..units.)) +
  geom_bar(stat = "identity", fill = "blue") +
  facet_wrap(~Year) +
  labs(title = "Monthly Sales",
       x = "Month",
       y = "Sales (units)")

#sales in different regions in different time (Figure 8)
selected_US <- fruittea[fruittea$Region == "US",]
selected_UK <- fruittea[fruittea$Region == "UK",]
selected_Sin <- fruittea[fruittea$Region == "Singapore",]
selected_Mex <- fruittea[fruittea$Region == "Mexico",]
selected_Spa <- fruittea[fruittea$Region == "Spain",]
#combine the graph of sales from five region
combined_data <- rbind(
  transform(selected_US, Region = "US"),
  transform(selected_UK, Region = "UK"),
  transform(selected_Sin, Region = "Singapore"),
  transform(selected_Mex, Region = "Mexico"),
  transform(selected_Spa, Region = "Spain")
)
ggplot(data = combined_data, aes(x = Time, y = Sales..units., fill = Region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Sales in Different Regions in Different Time", x = "Time", y = "Sales units")


###question 4###
#Checking and converting char to factors
str(fruittea)
fruittea$Product <- as.factor(fruittea$Product)
fruittea$Region <- as.factor(fruittea$Region)
fruittea$Month <- as.factor(fruittea$Month)

attach(fruittea)

#Packages installed and loaded
if (!require("ggplot2")){install.packages("ggplot2")};library(ggplot2) 
if (!require("dplyr")){install.packages("aTSA")};library(dplyr) 
if (!require("ggpubr")){install.packages("forecast")};library(ggpubr) 
if (!require("astsa")){install.packages("astsa")};library(astsa) 
if (!require("aTSA")){install.packages("aTSA")};library(aTSA) 
if (!require("forecast")){install.packages("forecast")};library(forecast) 
if (!require("carat")){install.packages("carat")};library(carat)
if (!require("tseries")){install.packages("tseries")};library(tseries)

### Creating Region-wise data sets
#US
ft_usa <- fruittea[Region == 'US',]
summary(ft_usa)

#UK
ft_uk <- fruittea[Region == 'UK',]
summary(ft_uk)

#Mexico
ft_mexico <- fruittea[Region == 'Mexico',]
summary(ft_mexico)

#Spain
ft_spain <- fruittea[Region == 'Spain',]
summary(ft_spain)

#Singapore
ft_singapore <- fruittea[Region == 'Singapore',]
summary(ft_singapore)


#Checking for seasonality

ft <- fruittea
# Create a date column by pasting Month and Year together
ft$time_index <- as.Date(paste(ft$Year, match(ft$Month, month.abb), "01", sep = "-"))

# Convert to time series
ts_data_usa <- ts(ft_usa$Sales..units., start = c(min(ft_usa$Year), 1), end = c(max(ft_usa$Year), 12), frequency = 12)
ts_data_uk <- ts(ft_uk$Sales..units., start = c(min(ft_uk$Year), 1), end = c(max(ft_uk$Year), 12), frequency = 12)
ts_data_mexico <- ts(ft_mexico$Sales..units., start = c(min(ft_mexico$Year), 1), end = c(max(ft_mexico$Year), 12), frequency = 12)
ts_data_singapore <- ts(ft_singapore$Sales..units., start = c(min(ft_singapore$Year), 1), end = c(max(ft_singapore$Year), 12), frequency = 12)
ts_data_spain <- ts(ft_spain$Sales..units., start = c(min(ft_spain$Year), 1), end = c(max(ft_spain$Year), 12), frequency = 12)



#1. Visualization
ggplot(ft_usa, aes(x=Time, y=Sales..units.)) +
  geom_point(color = "red")+
  geom_line()

plot(ts_data_usa) #ts data similar to ft_usa
#from plain graph we can't establish seasonality

#2. Seasonal decomposition
decomposed <- stl(ts_data_usa, s.window = "periodic")
plot(decomposed)
## We see a pattern in seasonality, the trend is not linear and residuals are concerning

#3. Seasonal Autocorrelation (ACF)
acf(ts_data_usa)

#4 ADF test for checking stationarity of remainder
adf_test <- adf.test(decomposed$time.series[,3])
## All p-values < 0.05 ==> remainder is stationary


######################################################################################
#####################                   Prediction                    ################
######################################################################################

#######################____________________________USA_____________________________________##############################

# 1.1. Splitting data into training, validation and test sets
usa_train <- ts(ts_data_usa[1:48],frequency=12,start=c(2010,1)) #ts denotes we are passing time series data
usa_valid <- ts(ts_data_usa[49:54],frequency=12,start=c(2014,1)) 
usa_test <- ts(ts_data_usa[55:60],frequency=12,start=c(2014,7))

# 1.2. Decomposition of the time-series data - to check if the data has cyclic component
vals <- stl((usa_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals,main="Monthly Fruittea Sales in USA, 2010-2014")

# #### Do we need log? #### Is there any pattern in the remainder above? -- Check
# vals <- stl(log(usa_train),s.window = 12) #s.window is the the number of time units were the cycle repeats.
# plot( vals ,main="log - Monthly Fruittea Sales 2010-2012")
# Data transformation via log/exp doesn't have any impact on the seasonality or trend

head(vals$time.series)
vals$time.series[,1]##seasonality ###We see that the values for the same month are similar over the years, suggesting that a seasonality component does exist
vals$time.series[,2]##trend ###Slight upward in the beginning, but an overall downward trend till 2013
vals$time.series[,3]##remainder ###No patterns, hence no seasonality component leftover in remainder


# 1.3. Simple naive forecast
present_time <- length(usa_train)
last_obs <- usa_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,1))

plot.ts(usa_train,xlim=c(2010,2015),ylim=c(0,160))
points(usa_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)

# 1.4. Naive seasonal forecast

present_time <- length(usa_train)
last_obs <- usa_train[present_time-(12:7)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,1))

plot.ts(usa_train,xlim=c(2010,2015),ylim=c(0,160))
points(usa_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)



# 1.5. Forecast by modelling on individual components in the summative decomposition
# 1.5.1. Forecast using auto-arima
# Fit a model using auto.arima (automatic ARIMA)
model <- auto.arima(usa_train)

# Make forecasts
forecast_values <- forecast(model, h = 6)  # Change 'h' based on the number of periods you want to forecast

# Plot the time series and forecast
plot(forecast_values, main = "Time Series Forecast")
### Doesn't work as we get a single straight line - possibly working off off single point(simple) naive forecasting

# 1.5.2. Forecast using naive forecast for seasonal pattern
#This obtained the avg value of each column, it tells about the seasonality component. We add the trend to this to obtain the more accurate values when predicting for more than 1 point in the future
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

#Modelling the trend
trend <- vals$time.series[,"trend"]

tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)

#Adding the seasonal + trend components
present_time <- length(usa_train) 
whathorizons <-  1:6 #6 cuz I want forecast for the next 6 months
nextTime <- present_time + whathorizons #setting time index for the next year i.e., 37  to 48

forecastsvals <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime)

forecastsvals <- ts(forecastsvals,frequency=12,start=c(2014,1)) # Forecast is in true values

plot.ts(usa_train,xlim=c(2010,2015),ylim=c(0,160))
points(usa_valid,type="l",lty=2)
points(forecastsvals,col="green",type = "l",pch=18)


# Modelling for remainder
## Checking for stationarity in remainder
Rt_usa <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt_usa)

## ACF and PACF plots:
acf(x = Rt_usa)  
pacf(x = Rt_usa) #Checking PACF --> there's a potential for order 4 as 4th bar seems big enough


##Auto-arima on remainder
auto.arima(Rt_usa)
# Series: Rt_usa 
# ARIMA(0,0,1)(1,0,0)[12] with zero mean 
# 
# Coefficients:
#   ma1     sar1
# -0.2901  -0.5300
# s.e.   0.1784   0.1528
# 
# sigma^2 = 347.9:  log likelihood = -157.41
# AIC=320.82   AICc=321.57   BIC=325.57


##Modified arima on remainder

### Fit an arima model using the suggested order from the point above.
arimafit_4 <- arima(x = Rt_usa, order = c(4,0,0),include.mean = FALSE) 
arimafit_1 <- arima(x = Rt_usa, order = c(1,0,0),include.mean = FALSE) 

### Obtaining predictions for the remainder
Ycast_4 <- stats::predict(arimafit_4,n.ahead = 6)
Ycast_1 <- stats::predict(arimafit_1,n.ahead = 6)

### Obtaining the full forecast
present_time <- length(usa_train)
whathorizons <-  1:6
nextTime <- present_time + whathorizons
usa_forecastsvals_ar4 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_4$pred ##<-- Adding the forecast of the remainder found using Auto-regressive model to the forecasts obtained using seasonality and trend before
usa_forecastsvals_ar1 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_1$pred

usa_forecastsvals_valid <- usa_forecastsvals_ar1
usa_forecastsvals_valid <- ts(usa_forecastsvals_valid,frequency=12,start=c(2014,1))
#With ar4 - MAPE is 138.34, with ar1 - MAPE is 101.60, clearly ar1 is superior

### Plotting all the model validation 
plot.ts(usa_train,xlim=c(2010,2015),ylim=c(0,160), main = "Validation forecast results")
points(usa_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
points(forecastsvals,col="green",type = "l",pch=18)            # Forecast using seasonality and trend
points(usa_forecastsvals_valid,col="purple",type = "l",pch=18)    # Forecast after adding the remainder forecast
legend("topleft", legend = c("Training", "Validation", "Naive Forecast", "Naive Seasonal Forecast",
                              "Seasonality and Trend Forecast", "Forecast with Remainder"),
       col = c("black", "black", "blue", "red", "green", "purple"),
       lty = c(1, 2, NA, 1, 1, 1), pch = c(NA, NA, 18, 18, 18, 18))

### Computing errors
MAPE_naive <- mean(abs(usa_valid - Naive_forecast)/usa_valid) * 100
MAPE_Naive_seasonal_forecast <- mean(abs(usa_valid - Naive_seasonal_forecast)/usa_valid) * 100
MAPE_notsonaive <- mean(abs(usa_valid - forecastsvals)/usa_valid) * 100
MAPE_AR <- mean(abs(usa_valid - usa_forecastsvals_valid)/usa_valid) * 100

MAPEs_usa <- c(MAPE_naive,MAPE_Naive_seasonal_forecast,MAPE_notsonaive,MAPE_AR)
#610.65137  64.29410  94.34775 101.60166

plt <- barplot(c(Naive=MAPE_naive,NaiveSeas=MAPE_Naive_seasonal_forecast,TS_seas_trend=MAPE_notsonaive,TS_remainder=MAPE_AR),main="MAPE USA", col = "light blue")
text(x = plt, y = MAPEs_usa-1, labels = round(MAPEs_usa,2), col = "black", cex =1, pos = 1)

## Result based on the MAPE values, chosen method is : Naive Seasonal Forecasting
# 1.6. Predicting test data
present_time <- length(usa_train)
last_obs <- usa_train[present_time-(6:1)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,7))

plot.ts(usa_train,xlim=c(2010,2015),ylim=c(0,160), main = "Test forecast results")
lines(usa_valid, col = "black", lty = 3)
lines(usa_test,type="l",lty=2)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
legend("topleft", legend = c("Training", "Validation", "Test","Forecast"),
       col = c("black", "black", "black", "red"),
       lty = c(1, 3, 2, 1), pch = c(NA, NA, NA,18))


MAPE_Naive_seasonal_forecast <- mean(abs(usa_test - Naive_seasonal_forecast)/usa_test) * 100
# 306.3129

# 1.7. Predicting the next month data
last_obs <- usa_train[present_time-(12:1)+1]
usa_forecast <- ts(last_obs,frequency=12,start=c(2015,1))
usa_forecast[1]
#8.08


#######################____________________________SPAIN_____________________________________##############################

# 2.1. Splitting data into training, validation and test sets
spain_train <- ts(ts_data_spain[1:48],frequency=12,start=c(2010,1)) #ts denotes we are passing time series data
spain_valid <- ts(ts_data_spain[49:54],frequency=12,start=c(2014,1)) 
spain_test <- ts(ts_data_spain[55:60],frequency=12,start=c(2014,7))

# 2.2. Decomposition of the time-series data - to check if the data has cyclic component
vals <- stl((spain_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals,main="Monthly Fruittea Sales in Spain, 2010-2013")


# 2.3. Simple naive forecast
present_time <- length(spain_train)
last_obs <- spain_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,1))

plot.ts(spain_train,xlim=c(2010,2015),ylim=c(0,160))
points(spain_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)

# 2.4. Naive seasonal forecast
present_time <- length(spain_train)
last_obs <- spain_train[present_time-(12:7)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,1))

plot.ts(spain_train,xlim=c(2010,2015),ylim=c(0,160))
points(spain_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)


# 2.5. Forecast by modelling on individual components in the summative decomposition
# 2.5.1. Forecast using naive forecast for seasonal pattern
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

#Modelling the trend
plot(vals$time.series[,"trend"])
trend <- vals$time.series[,"trend"]

tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)


#Adding the seasonal + trend components
present_time <- length(spain_train) 
whathorizons <-  1:6 #6 as we want forecast for the next 6 months
nextTime <- present_time + whathorizons #setting time index for the next year i.e., 49 to 54

forecastsvals <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime)

forecastsvals <- ts(forecastsvals,frequency=12,start=c(2014,1)) # Forecast is in true values

plot.ts(spain_train,xlim=c(2010,2015),ylim=c(0,160))
points(spain_valid,type="l",lty=2)
points(forecastsvals,col="green",type = "l",pch=18)


# Modelling for remainder
## Checking for stationarity in remainder
Rt_spain <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt_spain)

## ACF and PACF plots:
acf(x = Rt_spain)  #To apply auto-regressive model checking ACF
pacf(x = Rt_spain) #Checking PACF --> there's a potential for order 3 cuz 3rd bar seems big enogh


#Auto-arima on remainder
auto.arima(Rt_spain)
# Series: Rt_spain 
# ARIMA(1,0,0)(1,0,0)[12] with zero mean 
# 
# Coefficients:
#   ar1     sar1
# -0.2563  -0.3189
# s.e.   0.1531   0.1656
# 
# sigma^2 = 493.4:  log likelihood = -216.6
# AIC=439.19   AICc=439.74   BIC=444.81


## Modified arima on remainder

### Fit an arima model using the suggested order from the point above.
#arimafit_3 <- arima(x = Rt_spain, order = c(3,0,0),include.mean = FALSE) #If the mean of the process is constant and not 0
arimafit_1 <- arima(x = Rt_spain, order = c(1,0,0),include.mean = FALSE) #  we can specify, include.mean = FALSE

### Obtaining predictions for the remainder
#Ycast_3 <- stats::predict(arimafit_3,n.ahead = 6)
Ycast_1 <- stats::predict(arimafit_1,n.ahead = 6)

### Obtaining the full forecast
present_time <- length(spain_train)
whathorizons <-  1:6
nextTime <- present_time + whathorizons
#spain_forecastsvals_ar3 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_3$pred ##<-- Adding the forecast of the remainder found using Auto-regressive model to the forecasts obtained using seasonality and trend before
spain_forecastsvals_ar1 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_1$pred

spain_forecastsvals_valid <- spain_forecastsvals_ar1
spain_forecastsvals_valid <- ts(spain_forecastsvals_valid,frequency=12,start=c(2014,1))

### Plotting all the model validation 
plot.ts(spain_train,xlim=c(2010,2015),ylim=c(0,160), main = "Validation forecast results")
points(spain_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
points(forecastsvals,col="green",type = "l",pch=18)            # Forecast using seasonality and trend
points(spain_forecastsvals_valid,col="purple",type = "l",pch=18)    # Forecast after adding the remainder forecast --> No significant difference after adding remainder
legend("topleft", legend = c("Training", "Validation", "Naive Forecast", "Naive Seasonal Forecast",
                             "Seasonality and Trend Forecast", "Forecast with Remainder"),
       col = c("black", "black", "blue", "red", "green", "purple"),
       lty = c(1, 2, NA, 1, 1, 1), pch = c(NA, NA, 18, 18, 18, 18))


### Computing errors
MAPE_naive <- mean(abs(spain_valid - Naive_forecast)/spain_valid) * 100
MAPE_Naive_seasonal_forecast <- mean(abs(spain_valid - Naive_seasonal_forecast)/spain_valid) * 100
MAPE_notsonaive <- mean(abs(spain_valid - forecastsvals)/spain_valid) * 100
MAPE_AR <- mean(abs(spain_valid - spain_forecastsvals_valid)/spain_valid) * 100

MAPEs_spain <- c(MAPE_naive,MAPE_Naive_seasonal_forecast,MAPE_notsonaive,MAPE_AR)
#92.13431  48.66092 197.58211 204.26977

plt <- barplot(c(Naive=MAPE_naive,NaiveSeas=MAPE_Naive_seasonal_forecast,TS_seas_trend=MAPE_notsonaive,TS_remainder=MAPE_AR),main="MAPE Spain", col = "light blue")
text(x = plt, y = MAPEs_spain-1, labels = round(MAPEs_spain,2), col = "black", cex =1, pos = 1)


## Result based on the MAPE values, chosen method is : Naive Seasonal Forecasting
# 2.6. Predicting test data
present_time <- length(spain_train)
last_obs <- spain_train[present_time-(6:1)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,7))

plot.ts(spain_train,xlim=c(2010,2015),ylim=c(0,160), main = "Test forecast results")
lines(spain_valid, type = "l", lty = 3)
lines(spain_test,type="l",lty=2)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)

MAPE_Naive_seasonal_forecast <- mean(abs(spain_test - Naive_seasonal_forecast)/spain_test) * 100
# 245.451

# 2.7. Predicting the next month data
last_obs <- spain_train[present_time-(12:1)+1]
spain_forecast <- ts(last_obs,frequency=12,start=c(2015,1))
spain_forecast[1]
# 12.18



#######################____________________________UK___________________________________##############################

#####For uk :----- Split 4+0.5+0.5
# 3.1. Splitting data into training, validation and test sets
uk_train <- ts(ts_data_uk[1:48],frequency=12,start=c(2010,1)) #ts denotes we are passing time series data
uk_valid <- ts(ts_data_uk[49:54],frequency=12,start=c(2014,1)) 
uk_test <- ts(ts_data_uk[55:60],frequency=12,start=c(2014,7))

# 3.2. Decomposition of the time-series data - to check if the data has cyclic component
vals <- stl((uk_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals,main="Monthly Fruittea Sales in UK, 2010-2013")

# 3.3. Simple naive forecast
present_time <- length(uk_train)
last_obs <- uk_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,1))

plot.ts(uk_train,xlim=c(2010,2015),ylim=c(0,160))
points(uk_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)

# 3.4. Naive seasonal forecast
present_time <- length(uk_train)
last_obs <- uk_train[present_time-(12:7)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,1))

plot.ts(uk_train,xlim=c(2010,2015),ylim=c(0,160))
points(uk_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)


# 3.5. Forecast by modelling on individual components in the summative decomposition

# 3.5.1. Forecast using naive forecast for seasonal pattern
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

#Modelling the trend
plot(vals$time.series[,"trend"])
trend <- vals$time.series[,"trend"]

tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)


#Adding the seasonal + trend components
present_time <- length(uk_train) 
whathorizons <-  1:6 
nextTime <- present_time + whathorizons 

forecastsvals <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime)

forecastsvals <- ts(forecastsvals,frequency=12,start=c(2014,1)) # Forecast is in true values

plot.ts(uk_train,xlim=c(2010,2015),ylim=c(0,160))
points(uk_valid,type="l",lty=2)
points(forecastsvals,col="green",type = "l",pch=18)


# Modelling for remainder
## Checking for stationarity in remainder
Rt_uk <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt_uk)

## ACF and PACF plots:
acf(x = Rt_uk)  
pacf(x = Rt_uk)


##Auto-arima on remainder
auto.arima(Rt_uk)
# Series: Rt_uk 
# ARIMA(0,0,0)(1,0,0)[12] with zero mean 
# 
# Coefficients:
#   sar1
# -0.4422
# s.e.   0.1199
# 
# sigma^2 = 60.55:  log likelihood = -167.39
# AIC=338.79   AICc=339.05   BIC=342.53


## Modified arima on remainder

### Fit an arima model using the suggested order from the point above.
arimafit_1 <- arima(x = Rt_uk, order = c(1,0,0),include.mean = FALSE)

### Obtaining predictions for the remainder
Ycast_1 <- stats::predict(arimafit_1,n.ahead = 6)

### Obtaining the full forecast
present_time <- length(uk_train)
whathorizons <-  1:6
nextTime <- present_time + whathorizons
uk_forecastsvals_ar1 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_1$pred

uk_forecastsvals_valid <- uk_forecastsvals_ar1
uk_forecastsvals_valid <- ts(uk_forecastsvals_valid,frequency=12,start=c(2014,1))

### Plotting all the model validation 
plot.ts(uk_train,xlim=c(2010,2015),ylim=c(0,60), main = "Validation forecast results")
points(uk_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
points(forecastsvals,col="green",type = "l",pch=18)            # Forecast using seasonality and trend
points(uk_forecastsvals_valid,col="purple",type = "l",pch=18)    # Forecast after adding the remainder forecast --> No significant difference after adding remainder
legend("topleft", legend = c("Training", "Validation", "Naive Forecast", "Naive Seasonal Forecast",
                             "Seasonality and Trend Forecast", "Forecast with Remainder"),
       col = c("black", "black", "blue", "red", "green", "purple"),
       lty = c(1, 2, NA, 1, 1, 1), pch = c(NA, NA, 18, 18, 18, 18))


### Computing errors
MAPE_naive <- mean(abs(uk_valid - Naive_forecast)/uk_valid) * 100
MAPE_Naive_seasonal_forecast <- mean(abs(uk_valid - Naive_seasonal_forecast)/uk_valid) * 100
MAPE_notsonaive <- mean(abs(uk_valid - forecastsvals)/uk_valid) * 100
MAPE_AR <- mean(abs(uk_valid - uk_forecastsvals_valid)/uk_valid) * 100

MAPEs_uk <- c(MAPE_naive,MAPE_Naive_seasonal_forecast,MAPE_notsonaive,MAPE_AR)
#99.01486  78.05507 162.16972 161.95610

plt <- barplot(c(Naive=MAPE_naive,NaiveSeas=MAPE_Naive_seasonal_forecast,TS_seas_trend=MAPE_notsonaive,TS_remainder=MAPE_AR),main="MAPE UK", col = "light blue")
text(x = plt, y = MAPEs_uk-1, labels = round(MAPEs_uk,2), col = "black", cex =1, pos = 1)


## Result based on the MAPE values, chosen method is : Naive Seasonal Forecasting
# 3.6. Predicting test data
present_time <- length(uk_train)
last_obs <- uk_train[present_time-(6:1)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,7))

plot.ts(uk_train,xlim=c(2010,2015),ylim=c(0,60), main = "Test forecast results")
points(uk_valid, type = "l", lty = 3)
points(uk_test,type="l",lty=2)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)

MAPE_Naive_seasonal_forecast <- mean(abs(uk_test - Naive_seasonal_forecast)/uk_test) * 100
#65.951

# 3.7. Predicting the next month data
last_obs <- uk_train[present_time-(12:1)+1]
uk_forecast <- ts(last_obs,frequency=12,start=c(2015,1))
uk_forecast[1]
#4.22

#######################____________________________Mexico___________________________________##############################

#####For mexico :----- Split 4+0.5+0.5
# 4.1. Splitting data into training, validation and test sets
mexico_train <- ts(ts_data_mexico[1:48],frequency=12,start=c(2010,1)) #ts denotes we are passing time series data
mexico_valid <- ts(ts_data_mexico[49:54],frequency=12,start=c(2014,1)) 
mexico_test <- ts(ts_data_mexico[55:60],frequency=12,start=c(2014,7))

# 4.2. Decomposition of the time-series data - to check if the data has cyclic component
vals <- stl((mexico_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals,main="Monthly Fruittea Sales in Mexico, 2010-2013")


# 4.3. Simple naive forecast
present_time <- length(mexico_train)
last_obs <- mexico_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,1))

plot.ts(mexico_train,xlim=c(2010,2015),ylim=c(0,160))
points(mexico_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)

# 4.4. Naive seasonal forecast
present_time <- length(mexico_train)
last_obs <- mexico_train[present_time-(12:7)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,1))

plot.ts(mexico_train,xlim=c(2010,2015),ylim=c(0,160))
points(mexico_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)


# 4.5. Forecast by modelling on individual components in the summative decomposition
# Forecast using naive forecast for seasonal pattern
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

# Modelling the trend
plot(vals$time.series[,"trend"])
trend <- vals$time.series[,"trend"]

tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)


# Adding the seasonal + trend components
present_time <- length(mexico_train) 
whathorizons <-  1:6 
nextTime <- present_time + whathorizons 

forecastsvals <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime)

forecastsvals <- ts(forecastsvals,frequency=12,start=c(2014,1)) # Forecast is in true values

plot.ts(mexico_train,xlim=c(2010,2015),ylim=c(0,160))
points(mexico_valid,type="l",lty=2)
points(forecastsvals,col="green",type = "l",pch=18)


# Modelling for remainder
## Checking for stationarity in remainder
Rt_mexico <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt_mexico)

## CF and PACF plots:
acf(x = Rt_mexico)  #To apply auto-regressive model checking ACF
pacf(x = Rt_mexico) #Checking PACF --> there's a potential for order 3 cuz 3rd bar seems big enogh


## Auto-arima on remainder
auto.arima(Rt_mexico)
# Series: Rt_mexico 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 = 469:  log likelihood = -215.72
# AIC=433.44   AICc=433.53   BIC=435.32


## Modified arima on remainder

### Fit an arima model using the suggested order from the point above.
arimafit_1 <- arima(x = Rt_mexico, order = c(1,0,0),include.mean = FALSE) 

### Obtaining predictions for the remainder
Ycast_1 <- stats::predict(arimafit_1,n.ahead = 6)

### Obtaining the full forecast
present_time <- length(mexico_train)
whathorizons <-  1:6
nextTime <- present_time + whathorizons
mexico_forecastsvals_ar1 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_1$pred

mexico_forecastsvals_valid <- mexico_forecastsvals_ar1
mexico_forecastsvals_valid <- ts(mexico_forecastsvals_valid,frequency=12,start=c(2014,1))

### Plotting all the model validation 
plot.ts(mexico_train,xlim=c(2010,2015),ylim=c(0,160), main = "Validation forecast results")
points(mexico_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
points(forecastsvals,col="green",type = "l",pch=18)            # Forecast using seasonality and trend
points(mexico_forecastsvals_valid,col="purple",type = "l",pch=18)    # Forecast after adding the remainder forecast --> No significant difference after adding remainder
legend("topleft", legend = c("Training", "Validation", "Naive Forecast", "Naive Seasonal Forecast",
                             "Seasonality and Trend Forecast", "Forecast with Remainder"),
       col = c("black", "black", "blue", "red", "green", "purple"),
       lty = c(1, 2, NA, 1, 1, 1), pch = c(NA, NA, 18, 18, 18, 18))


### Computing errors
MAPE_naive <- mean(abs(mexico_valid - Naive_forecast)/mexico_valid) * 100
MAPE_Naive_seasonal_forecast <- mean(abs(mexico_valid - Naive_seasonal_forecast)/mexico_valid) * 100
MAPE_notsonaive <- mean(abs(mexico_valid - forecastsvals)/mexico_valid) * 100
MAPE_AR <- mean(abs(mexico_valid - mexico_forecastsvals_valid)/mexico_valid) * 100

MAPEs_mexico <- c(MAPE_naive,MAPE_Naive_seasonal_forecast,MAPE_notsonaive,MAPE_AR)
#748.5334 146.8065 291.9311 292.7258

plt <- barplot(c(Naive=MAPE_naive,NaiveSeas=MAPE_Naive_seasonal_forecast,TS_seas_trend=MAPE_notsonaive,TS_remainder=MAPE_AR),main="MAPE Mexico", col = "light blue")
text(x = plt, y = MAPEs_mexico-1, labels = round(MAPEs_mexico,2), col = "black", cex =1, pos = 1)


## Result based on the MAPE values, chosen method is : Naive Seasonal Forecasting
# 4.6. Predicting test data
present_time <- length(mexico_train)
last_obs <- mexico_train[present_time-(6:1)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,7))

plot.ts(mexico_train,xlim=c(2010,2015),ylim=c(0,160), main = "Test forecast results")
lines(mexico_valid, type = "l", lty = 3)
lines(mexico_test,type="l",lty=2)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)

MAPE_Naive_seasonal_forecast <- mean(abs(mexico_test - Naive_seasonal_forecast[7:12])/mexico_test) * 100
#297.7126

# 4.7. Predicting the next month data
last_obs <- mexico_train[present_time-(12:1)+1]
mexico_forecast <- ts(last_obs,frequency=12,start=c(2015,1))
mexico_forecast[1]
#3.9

#######################____________________________Singapore___________________________________##############################

#####For singapore :----- Split 4+0.5+0.5
# 5.1. Splitting data into training, validation and test sets
singapore_train <- ts(ts_data_singapore[1:48],frequency=12,start=c(2010,1)) #ts denotes we are passing time series data
singapore_valid <- ts(ts_data_singapore[49:54],frequency=12,start=c(2014,1)) 
singapore_test <- ts(ts_data_singapore[55:60],frequency=12,start=c(2014,7))

# 5.2. Decomposition of the time-series data - to check if the data has cyclic component
vals <- stl((singapore_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals,main="Monthly Fruittea Sales in Singapore, 2010-2013")


# 5.3. Simple naive forecast
present_time <- length(singapore_train)
last_obs <- singapore_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,1))

plot.ts(singapore_train,xlim=c(2010,2015),ylim=c(0,160))
points(singapore_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)

# 5.4. Naive seasonal forecast

present_time <- length(singapore_train)
last_obs <- singapore_train[present_time-(12:7)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,1))

plot.ts(singapore_train,xlim=c(2010,2015),ylim=c(0,160))
points(singapore_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)


# 5.5. Forecast by modelling on individual components in the summative decomposition

#Forecast using naive forecast for seasonal pattern
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

#Modelling the trend
plot(vals$time.series[,"trend"])
trend <- vals$time.series[,"trend"]

tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)


#Adding the seasonal + trend components
present_time <- length(singapore_train) 
whathorizons <-  1:6 
nextTime <- present_time + whathorizons 

forecastsvals <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime)

forecastsvals <- ts(forecastsvals,frequency=12,start=c(2014,1)) # Forecast is in true values

plot.ts(singapore_train,xlim=c(2010,2015),ylim=c(0,160))
points(singapore_valid,type="l",lty=2)
points(forecastsvals,col="green",type = "l",pch=18)


# Modelling for remainder
## Checking for stationarity in remainder
Rt_singapore <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt_singapore)

## ACF and PACF plots:
acf(x = Rt_singapore) 
pacf(x = Rt_singapore)


##Auto-arima on remainder
auto.arima(Rt_singapore)
# Series: Rt_singapore 
# ARIMA(0,0,0) with zero mean  --> implies that the remainder is white noise
# 
# sigma^2 = 61.26:  log likelihood = -166.87
# AIC=335.74   AICc=335.83   BIC=337.61


## Modified arima on remainder

### Fit an arima model using the suggested order from the point above.
arimafit_1 <- arima(x = Rt_singapore, order = c(1,0,0),include.mean = FALSE) 

### Obtaining predictions for the remainder
Ycast_1 <- stats::predict(arimafit_1,n.ahead = 6)

### Obtaining the full forecast
present_time <- length(singapore_train)
whathorizons <-  1:6
nextTime <- present_time + whathorizons
singapore_forecastsvals_ar1 <- cycle[whathorizons] + trend_model$coefficients[1] + trend_model$coefficients[2] * (nextTime) + Ycast_1$pred

singapore_forecastsvals_valid <- singapore_forecastsvals_ar1
singapore_forecastsvals_valid <- ts(singapore_forecastsvals_valid,frequency=12,start=c(2014,1))

### Plotting all the model validation 
plot.ts(singapore_train,xlim=c(2010,2015),ylim=c(0,60), main = "Validation forecast results")
points(singapore_valid,type="l",lty=2)
points(Naive_forecast,col="blue",type = "p",pch=18)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
points(forecastsvals,col="green",type = "l",pch=18)            # Forecast using seasonality and trend
points(singapore_forecastsvals_valid,col="purple",type = "l",pch=18)    # Forecast after adding the remainder forecast --> No significant difference after adding remainder

### Computing errors
MAPE_naive <- mean(abs(singapore_valid - Naive_forecast)/singapore_valid) * 100
MAPE_Naive_seasonal_forecast <- mean(abs(singapore_valid - Naive_seasonal_forecast)/singapore_valid) * 100
MAPE_notsonaive <- mean(abs(singapore_valid - forecastsvals)/singapore_valid) * 100
MAPE_AR <- mean(abs(singapore_valid - singapore_forecastsvals_valid)/singapore_valid) * 100

MAPEs_singapore <- c(MAPE_naive,MAPE_Naive_seasonal_forecast,MAPE_notsonaive,MAPE_AR)
# 69.71127 206.74603  93.83441  93.76843

plt <- barplot(c(Naive=MAPE_naive,NaiveSeas=MAPE_Naive_seasonal_forecast,TS_seas_trend=MAPE_notsonaive,TS_remainder=MAPE_AR),main="MAPE singapore", col = "light blue")
text(x = plt, y = MAPEs_singapore-1, labels = round(MAPEs_singapore,2), col = "black", cex =1, pos = 1)


## Result based on the MAPE values, chosen method is : Naive Seasonal Forecasting
# 5.6. Predicting test data
present_time <- length(singapore_train)
last_obs <- singapore_train[present_time-(6:1)+1]
Naive_seasonal_forecast <- ts(last_obs,frequency=12,start=c(2014,7))

#Simple naive on test
present_time <- length(singapore_train)
last_obs <- singapore_train[present_time]
Naive_forecast <- ts(rep(last_obs,6),frequency=12,start=c(2014,7))


plot.ts(singapore_train,xlim=c(2010,2015),ylim=c(0,60), main = "Test forecast results")
points(singapore_valid, type = "l", lty = 3)
points(singapore_test,type="l",lty=2)
points(Naive_seasonal_forecast,col="red",type = "l",pch=18)
#points(Naive_forecast,col="blue",type = "p",pch=18)

MAPE_Naive_seasonal_forecast <- mean(abs(singapore_test - Naive_seasonal_forecast)/singapore_test) * 100
#178.6764
# MAPE_Naive_forecast <- mean(abs(singapore_test - Naive_forecast)/singapore_test) * 100 ##Not kept since during test this has higher MAPE
# #340.4565 

# 5.7. Predicting the next month data
last_obs <- singapore_train[present_time-(12:1)+1]
singapore_forecast <- ts(last_obs,frequency=12,start=c(2015,1))
singapore_forecast[1]
#16.28