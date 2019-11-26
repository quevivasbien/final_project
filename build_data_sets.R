setwd('C:/Users/silve/Documents/math/time_series/final_project')

# Import trends data
library(readr)
tr <- read_csv("all_trends.csv")
# Import consumption data
cons <- read_csv("PCE.csv")
colnames(cons) <- c('month', 'consumption')

# Format all vars as time series obs
library(zoo)

tr$month <- as.yearmon(tr$month)
tr$business_industrial <- ts(tr$business_industrial, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$finance <- ts(tr$finance, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$health <- ts(tr$health, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$hobbies_leisure <- ts(tr$hobbies_leisure, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$jobs_education <- ts(tr$jobs_education, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$shopping <- ts(tr$shopping, start = c(2004, 1), end = c(2019, 11), frequency = 12)
tr$travel <- ts(tr$travel, start = c(2004, 1), end = c(2019, 11), frequency = 12)

cons$month <- as.yearmon(cons$month)
cons$consumption <- ts(cons$consumption, start = c(1959, 1), end = c(2019, 9), frequency = 12)
# Add log consumption
cons$logcons <- log(cons$consumption)

# DF-GLS tests for stationarity
library(urca)
summary(ur.ers(tr$business_industrial)) # Conclusion: unit root
summary(ur.ers(diff(tr$business_industrial))) # Conclusion: no unit root, 0.01 < p < 0.05

library(dynlm)
timestamp <- time(tr$business_industrial)

# Need to detrend the 2004-2010, 2011-2015, and 2016-2019 parts separately
detr <- data.frame(tr$month)
detr$business_industrial <- c(summary(dynlm(tr$business_industrial ~ timestamp, end = c(2010, 12)))$residuals,
                              summary(dynlm(tr$business_industrial ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                              summary(dynlm(tr$business_industrial ~ timestamp, start = c(2016, 1)))$residuals)
detr$finance <- c(summary(dynlm(tr$finance ~ timestamp, end = c(2010, 12)))$residuals,
                              summary(dynlm(tr$finance ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                              summary(dynlm(tr$finance ~ timestamp, start = c(2016, 1)))$residuals)
detr$health <- c(summary(dynlm(tr$health ~ timestamp, end = c(2010, 12)))$residuals,
                  summary(dynlm(tr$health ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                  summary(dynlm(tr$health ~ timestamp, start = c(2016, 1)))$residuals)
detr$hobbies_leisure <- c(summary(dynlm(tr$hobbies_leisure ~ timestamp, end = c(2010, 12)))$residuals,
                  summary(dynlm(tr$hobbies_leisure ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                  summary(dynlm(tr$hobbies_leisure ~ timestamp, start = c(2016, 1)))$residuals)
detr$jobs_education <- c(summary(dynlm(tr$jobs_education ~ timestamp, end = c(2010, 12)))$residuals,
                         summary(dynlm(tr$jobs_education ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                         summary(dynlm(tr$jobs_education ~ timestamp, start = c(2016, 1)))$residuals)
detr$shopping <- c(summary(dynlm(tr$shopping ~ timestamp, end = c(2010, 12)))$residuals,
                  summary(dynlm(tr$shopping ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                  summary(dynlm(tr$shopping ~ timestamp, start = c(2016, 1)))$residuals)
detr$travel <- c(summary(dynlm(tr$travel ~ timestamp, end = c(2010, 12)))$residuals,
                         summary(dynlm(tr$travel ~ timestamp, start = c(2011, 1), end = c(2015, 12)))$residuals,
                         summary(dynlm(tr$travel ~ timestamp, start = c(2016, 1)))$residuals)
# Revert residual series to time series type
detr$business_industrial <- ts(detr$business_industrial, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$finance <- ts(detr$finance, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$health <- ts(detr$health, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$hobbies_leisure <- ts(detr$hobbies_leisure, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$jobs_education <- ts(detr$jobs_education, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$shopping <- ts(detr$shopping, start = c(2004, 1), end = c(2019, 11), frequency = 12)
detr$travel <- ts(detr$travel, start = c(2004, 1), end = c(2019, 11), frequency = 12)

# Pretty sure these should be stationary now, but just to double-check:
summary(ur.ers(detr$business_industrial)) # Conclusion: definitely no unit root
summary(ur.ers(detr$finance)) # No unit root
summary(ur.ers(detr$health)) # No unit root
summary(ur.ers(detr$hobbies_leisure)) # 0.01 < p < 0.05 # The Christmas spikes make this one weird
summary(ur.ers(detr$jobs_education)) # No unit root
summary(ur.ers(detr$shopping)) # No unit root
summary(ur.ers(detr$travel))

plot(detr$hobbies_leisure)

# Var model!
library(vars)
var_model <- VAR(detr[,-1], lag.max=12)
forecasts <- predict(var_model) # Try messing with number of lags?

# Identify best model with VARselect
VARselect(detr[,-1], lag.max = 12) # More lags seems to be better with no seasonality
VARselect(detr[,-1], lag.max = 12, season = 12) # 1 lag or 5 lags seems best
var_model <- VAR(detr[,-1], p = 1, season = 12)
plot(predict(var_model, n.ahead = 24))

# Can use stargazer package to generate latex tables, but it only works with lm or dynlm objects
# Actually: can gen table calling stargazer(var_model$varresult)


# Examine predictive power of these vars on consumption

# determine stationarity of logcons
summary(ur.ers(cons$logcons)) # Not stationary # Also not trend-stationary as far as I can tell
summary(ur.ers(cons$consumption)) # Not stationary

# Is logcons I(1)?
summary(ur.ers(diff(cons$logcons))) # Yes

# Maybe I can ask whether the Google trends Granger-cause changes in consumption?

# Set up variables
trcons <- data.frame(
  window(detr$business_industrial, start = c(2004, 1), end = c(2019, 9)),
  window(detr$finance, start = c(2004, 1), end = c(2019, 9)),
  window(detr$health, start = c(2004, 1), end = c(2019, 9)),
  window(detr$hobbies_leisure, start = c(2004, 1), end = c(2019, 9)),
  window(detr$jobs_education, start = c(2004, 1), end = c(2019, 9)),
  window(detr$shopping, start = c(2004, 1), end = c(2019, 9)),
  window(detr$travel, start = c(2004, 1), end = c(2019, 9)),
  window(diff(cons$logcons), start = c(2004, 1), end = c(2019, 9))
)
colnames(trcons) <- c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel', 'change_consumption')

# Create simple VAR
cons_var <- VAR(trcons, lag.max=4)
# Granger causality test, robust
causality(cons_var, cause = c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel'), vcov. = vcovHC(cons_var))
# Conclusion: reject H0 of no Granger causality with p=0.0548. (p is much lower without using robust standard errors)
# Note: This is highly dependent on the number of lags used. I can't get significant results with any other lag length

# Wait, including seasonality is important here: the trend series are highly seasonal
# Can also use VARselect function to determine right number of lags
VARselect(trcons, lag.max = 12, season = 12) # Best is just one lag; seasonality captures most everything else
cons_var <- VAR(trcons, p = 1, season = 12)
causality(cons_var, cause = c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel'), vcov. = vcovHC(cons_var))
# p = 0.037 # Still sensitive to changes in lag length, but not nearly as much

# Try removing seasonality first?
desscons <- data.frame(
  trcons$business_industrial - decompose(trcons$business_industrial)$seasonal,
  trcons$finance - decompose(trcons$finance)$seasonal,
  trcons$health - decompose(trcons$health)$seasonal,
  trcons$hobbies_leisure - decompose(trcons$hobbies_leisure)$seasonal,
  trcons$jobs_education - decompose(trcons$jobs_education)$seasonal,
  trcons$shopping - decompose(trcons$shopping)$seasonal,
  trcons$travel - decompose(trcons$travel)$seasonal,
  trcons$change_consumption
)
colnames(desscons) <- colnames(trcons)
dess_var <- VAR(desscons, p=1) # p=1 still optimal by VARselect
# VAR coeffs are basically identical to before
causality(dess_var, cause = c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel'), vcov. = vcovHC(dess_var))
# p = 0.033, basically same as before

# Try Granger causality with shopping only.

# Compare forecast power on consumption with forcast of arma model

# Generate tables

libary(stargazer)
stargazer(var_model$varresult$business_industrial, var_model$varresult$finance, var_model$varresult$health, var_model$varresult$hobbies_leisure,
          title = 'VAR coefficients', keep.stat = 'n')
# ~
