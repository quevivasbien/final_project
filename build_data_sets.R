setwd('Documentos/math/time_series/final_project')

# Import trends data
library(readr)
tr <- read_csv("all_trends.csv")
# Import consumption data
cons <- read_csv("DPCE.csv")
colnames(cons) <- c('month', 'pct_change_consumption') # pct_change_consumption is percent change in consumption

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
cons$pct_change_consumption <- ts(cons$pct_change_consumption, start = c(1959, 2), end = c(2019, 10), frequency = 12)

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

# Is pct_change_consumption stationary?
summary(ur.ers(cons$pct_change_consumption)) # p < 0.01, no unit root

# Maybe I can ask whether the Google trends Granger-cause changes in consumption?

# Set up variables
trcons <- data.frame(
  window(detr$business_industrial, start = c(2004, 1), end = c(2019, 10)),
  window(detr$finance, start = c(2004, 1), end = c(2019, 10)),
  window(detr$health, start = c(2004, 1), end = c(2019, 10)),
  window(detr$hobbies_leisure, start = c(2004, 1), end = c(2019, 10)),
  window(detr$jobs_education, start = c(2004, 1), end = c(2019, 10)),
  window(detr$shopping, start = c(2004, 1), end = c(2019, 10)),
  window(detr$travel, start = c(2004, 1), end = c(2019, 10)),
  window(cons$pct_change_consumption, start = c(2004, 1), end = c(2019, 10))
)
colnames(trcons) <- c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel', 'pct_change_consumption')

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
# p \approx 0 # Still sensitive to changes in lag length, but not nearly as much

# Try removing seasonality first?
desscons <- data.frame(
  trcons$business_industrial - decompose(trcons$business_industrial)$seasonal,
  trcons$finance - decompose(trcons$finance)$seasonal,
  trcons$health - decompose(trcons$health)$seasonal,
  trcons$hobbies_leisure - decompose(trcons$hobbies_leisure)$seasonal,
  trcons$jobs_education - decompose(trcons$jobs_education)$seasonal,
  trcons$shopping - decompose(trcons$shopping)$seasonal,
  trcons$travel - decompose(trcons$travel)$seasonal,
  trcons$pct_change_consumption
)
colnames(desscons) <- colnames(trcons)
dess_var <- VAR(desscons, p=1) # p=1 still optimal by VARselect
# VAR coeffs are basically identical to before
causality(dess_var, cause = c('business_industrial', 'finance', 'health', 'hobbies_leisure', 'jobs_education', 'shopping', 'travel'), vcov. = vcovHC(dess_var))
# p \approx 0, basically same as before

# Try Granger causality with finance only (finance has the strongest relationship based on the VAR models).
# Based on VARselect, 3 lags is optimal for finance, consumption model
finance_var <- VAR(desscons[c('finance', 'pct_change_consumption')], p = 3) # Eyeballing it, it looks like consumption follows finance, but finance doesn't follow consumption
causality(finance_var, cause = 'finance', vcov. = vcovHC(finance_var)) # p = 0.0050

# Compare forecast power on consumption with forcast of arma model

# Estimate ARMA model for logcons
library(aTSA)
# identify(trcons$change_consumption, p = 6, q = 6) # This takes a bit to run. The minimum AICC is an ARMA(2,0). ARMA(5,5) is also good. I don't know about more lags since it takes too long to run the grid search other wise.
estimate(trcons$change_consumption, p = 2, q = 0) # All coeffs significant. AIC = -1599, SB = -1590
estimate(trcons$change_consumption, p = 5, q = 5) # MA 3 and 4 not really significant, but good otherwise. AIC = -1601, SBC = -1565
# Messing with lags more doesn't seem to help. I'll take the ARMA(5,5) since we'll be using this for forecasts.
# Fit on all but last 12 months
arma <- estimate(window(trcons$change_consumption, end = c(2018, 9)), p = 5, q = 5)
# Create forecasts
arma_fcast <- forecast(arma, lead = 12, id = seq.Date(from = as.Date('2004-01-01'), to = as.Date('2018-09-01'), by='month'), output = FALSE)
arma_fcast_series <- zooreg(arma_fcast[,'Forecast'], as.yearmon('2018-10'), freq = 12)
var_fcast <- predict(cons_var, n.ahead = 12)
var_fcast_series <- zooreg(var_fcast$fcst$change_consumption[,'fcst'], as.yearmon('2018-10'), freq = 12)
true_series <- window(trcons$change_consumption, start = c(2018, 10))
# Doesn't seem like there's a significant difference

# Impulse response on pct_change_consumption
# Using dess_var and cons_var gives basically identical results, though they're a bit more pronounced with dess_var
cons_irf <- irf(dess_var, response = 'pct_change_consumption', n.ahead = 11)
# TODO: create single plot with all impulse responses
# Impulse response from only finance VAR
finance_irf <- irf(finance_var, response = 'pct_change_consumption', n.ahead = 11)




# Generate tables

library(stargazer)
stargazer(var_model$varresult$business_industrial, var_model$varresult$finance, var_model$varresult$health, var_model$varresult$hobbies_leisure,
          title = 'Trends VAR coefficients, 1/2', keep.stat = 'n')
stargazer(var_model$varresult$jobs_education, var_model$varresult$shopping, var_model$varresult$travel,
          title = 'Trends VAR coefficients, 2/2', keep.stat = 'n')

stargazer(dess_var$varresult$business_industrial, dess_var$varresult$finance, dess_var$varresult$health, dess_var$varresult$hobbies_leisure,
          title = 'Consumption VAR coefficients, 1/2', keep.stat = 'n')
stargazer(dess_var$varresult$jobs_education, dess_var$varresult$shopping, dess_var$varresult$travel, dess_var$varresult$pct_change_consumption,
          title = 'Consumption VAR coefficients, 2/2', keep.stat = 'n')

stargazer(finance_var$varresult,
          title = 'Finance/Consumption VAR coefficients', keep.stat = 'n')

# Include plot for dess_cons consumption fit?