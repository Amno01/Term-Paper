# Term Paper

###############################################################################

# Set working directory (change this)
setwd("C:/Users/Amanda/Downloads/Time series 3b/Assignments/Term paper/Script")

# Load required libraries
library(readxl)
library(dplyr)
library(zoo)
library(lubridate)
library(openxlsx)
library(writexl)
library(urca)
library(dynlm)
library(vars)  
library(forecast)
library(purrr)
library(ggplot2)
library(tidyr)

###############################################################################

# Convert monthly CPIF-data to quarterly

# Read in the Excel file
cpif <- read_excel("../Data/CPIF_index_monthly_2001_2024.xlsx")

# Rename columns
colnames(cpif) <- c("date", "cpif_index")

# Convert the date to a working Date format 
# (assumes YYYY-MM format, adds "-01" for the first of the month)
cpif <- cpif %>%
  mutate(date = as.Date(paste0(gsub("M", "-", date), "-01")))

# Create a quarterly variable using zoo::as.yearqtr
cpif <- cpif %>%
  mutate(quarter = as.yearqtr(date))

# Aggregate to quarterly data (average of index per quarter)
cpif_quarterly <- cpif %>%
  group_by(quarter) %>%
  summarise(cpif_q = mean(cpif_index, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert "2001 Q1" to "2001K1"
  mutate(quarter = gsub(" Q", "K", as.character(quarter)))

# Save the new excel
# write.xlsx(cpif_quarterly, "../Data/cpif_quarterly_2001_2024.xlsx", overwrite = TRUE)


###############################################################################

# Change the names on the Dates for intrest rates

# Read in the Excel file
interest_rate <- read_excel("../Data/interest_rates_quarter_2001_2024.xlsx")

# Rename columns
colnames(interest_rate) <- c("date", "interest_rate")

# Convert the date to a working Date format 
interest_rate <- interest_rate %>%
  mutate(date = gsub("Kvartal ", "K", date),
         date = gsub(" ", "", date)) 

# Save the new excel
write.xlsx(interest_rate, "../Data/interest_rates_quarterly_2001_2024.xlsx", overwrite = TRUE)

###############################################################################

# Combine the data files 

# File names in order
file_list <- c("../Data/unemp_25_64.xlsx", "../Data/15-24_unemployment_2001_2024.xlsx",
 "../Data/GDP_fixed_2001_2024.xlsx", "../Data/cpif_quarterly_2001_2024.xlsx", "../Data/interest_rates_quarterly_2001_2024.xlsx",
 "../Data/15-24_LFP_2001_2024.xlsx")

# Read all data frames
data_list <- lapply(file_list, read_excel)

# Merge all data frames by "Date" column using reduce + full_join
merged_data <- Reduce(function(x, y) full_join(x, y, by = "date"), data_list)

# Sort by Date
merged_data <- merged_data %>% arrange(date)

# Rename columns
colnames(merged_data) <- c("date", "unemployment_25_64", "unemployment_15_24", "gdp", "cpif", "interest_rate", "youth_lfp")

# Write to Excel
#write_xlsx(merged_data, "../Data/unemployment_gdp_cpif_rate_lfp_data.xlsx")

###############################################################################

# Unemployment 25-64 years

# File names in order
unemployment_25_64_list <- c("../Data/unemployment_25_34.xlsx", "../Data/unemployment_35_44.xlsx", "../Data/unemployment_45_54.xlsx", "../Data/unemployment_55_64.xlsx")

# Read all data frames
unemployment_list <- lapply(unemployment_25_64_list, read_excel)

# Rename columns to indicate their age group (assumes each has one data column + 'date')
age_groups <- c("25_34", "35_44", "45_54", "55_64")
unemployment_list <- Map(function(df, age) {
  colnames(df)[colnames(df) != "date"] <- paste0("unemployment_", age)
  df
}, unemployment_list, age_groups)

# Merge all data frames by date
unemployment_merged <- Reduce(function(x, y) full_join(x, y, by = "date"), unemployment_list) %>%
  arrange(date)

# Double-check that we are only averaging the intended 4 columns (and not the new one itself)
unemployment_rate_columns <- c("unemployment_25_34", "unemployment_35_44", "unemployment_45_54", "unemployment_55_64")

# Create new column: average unemployment for 25-64
unemployment_merged <- unemployment_merged %>%
  mutate(unemployment_25_64 = rowMeans(across(all_of(unemployment_rate_columns)), na.rm = TRUE))

# Drop total_unemployment 
unemployment_merged$unemployment_25_34<- NULL
unemployment_merged$unemployment_35_44<- NULL
unemployment_merged$unemployment_45_54<- NULL
unemployment_merged$unemployment_55_64<- NULL

# Write to a new Excel file
# write_xlsx(unemployment_merged, "../Data/unemployment_25_65.xlsx")

###############################################################################

# Unemployment 25-64 years (not season adjusted)

# File names in order
unemp_25_64_list <- c("../Data/unemp_25_34.xlsx", "../Data/unemp_35_44.xlsx", "../Data/unemp_45_54.xlsx", "../Data/unemp_55_64.xlsx")

# Read all data frames
unemp_list <- lapply(unemp_25_64_list, read_excel)

# Rename columns to indicate their age group (assumes each has one data column + 'date')
age_groups <- c("25_34", "35_44", "45_54", "55_64")
unemp_list <- Map(function(df, age) {
  colnames(df)[colnames(df) != "date"] <- paste0("unemployment_", age)
  df
}, unemp_list, age_groups)

# Merge all data frames by date
unemp_merged <- Reduce(function(x, y) full_join(x, y, by = "date"), unemp_list) %>%
  arrange(date)

# Double-check that we are only averaging the intended 4 columns (and not the new one itself)
unemp_rate_columns <- c("unemployment_25_34", "unemployment_35_44", "unemployment_45_54", "unemployment_55_64")

# Create new column: average unemployment for 25-64
unemp_merged <- unemp_merged %>%
  mutate(unemp_25_64 = rowMeans(across(all_of(unemp_rate_columns)), na.rm = TRUE))

# Drop total_unemployment 
unemp_merged$unemployment_25_34<- NULL
unemp_merged$unemployment_35_44<- NULL
unemp_merged$unemployment_45_54<- NULL
unemp_merged$unemployment_55_64<- NULL

# Write to a new Excel file
write_xlsx(unemp_merged, "../Data/unemp_25_64.xlsx")

###############################################################################


# Add the NEET to another excel 

# Read the new file (2007–2024 data)
neet_data <- read_excel("../Data/NEET_15-24_2007_2024.xlsx")

# Merge the new data into the existing dataset by "date"
combined_data <- full_join(merged_data, neet_data, by = "date") %>%
  arrange(date)  # Optional: keep it in chronological order

# Rename columns
colnames(combined_data) <- c("date", "total_unemployment", "youth_unemployment", "gdp", "cpif", "neet")

# Write to a new Excel file
#write_xlsx(combined_data, "../Data/neet_unemployment_gdp_cpif_data.xlsx")

###############################################################################

# Reserch question
# How is youth unemployment affected by macroeconomic variables such as GDP and inflation? 
# A VAR analysis on Swedish quarterly data 2001–2024. 

# Read in the data for 2001-2024
unemployment_data <- read_excel( "../Data/unemployment_gdp_cpif_rate_lfp_data.xlsx")

# Check the structure of dataset
str(unemployment_data)

# Take the logs to get the relative changes or growth rates

# CPIF use the first difference of the log to get quarterly inflation.  
# Log-transforming GDP helps you interperet the differences as growth rates. 

# Calculate inflation and GDP growth variables
unemployment_data$inflation_q <- c(NA, 100 * diff(log(unemployment_data$cpif)))  # First difference, so prepend NA
unemployment_data$lgdp <- log(unemployment_data$gdp)

# Remove first row with NA due to differencing
unemployment_data_clean <- unemployment_data[!is.na(unemployment_data$inflation_q), ]

# Create time series object from the cleaned data frame
data_ts <- ts(unemployment_data_clean[, c("unemployment_25_64", "unemployment_15_24", "lgdp", "inflation_q", "interest_rate", "youth_lfp")],
              start = c(2001, 2), frequency = 4)

# Plot the data
ts.plot(data_ts,
        main = "Swedish Macro Variables: 2001–2024",
        col = c("red", "blue", "darkgreen", "darkorange", "purple", "yellow"),
        ylab = "Levels / % Changes",
        lty = 1:3,
        lwd = 2)
legend("topright", legend = c("25_64_unemployment", "15_24_unemployment", "log(GDP)", "quarterly inflation", "intrest_rate", "youth_lfp"),
       col = c("red", "blue", "darkgreen", "darkorange", "purple", "yellow"), lty = 1:3, lwd = 2)

# Interpretation of the figure: (Change this)

# Youth unemployment: Visibly non-stationary: the series has strong seasonal fluctations 
# and no clear mean-reversion. (there are long cycles and changing variance - a sign of unit root behaviour.
# -> likely need to difference the series)

# Total Unemployment: Appears less volatile than youth unemployment, but still shows persistent trends over time. 
# (no clear mean. reversion either -> also a candidate for differencing.)

# log(GDP): Shows a smooth upward trend: this is a classic non-stationary behvarior (typical 
# for GDP in levels). (Definitely needs to be differenced to analyze growth/staitonarity.)

# Quarterly inflation: appears relatively sationary around a constant mean. 
# This is expected, as it is already transformed to a percentage change. 
# (Likely staitonary as it is, but should be tested)

###############################################################################

# ADF test 
# H0: variable has a unit root (non-stationary)
# We want to reject H0 to conclude stationarity. 

# 25-64 Unemployment
summary(ur.df(data_ts[, "unemployment_25_64"], type = "none", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "unemployment_25_64"], type = "drift", selectlags = "AIC")) # Can reject at 5% 
summary(ur.df(data_ts[, "unemployment_25_64"], type = "trend", selectlags = "AIC")) # Can reject at 10%  

# 15-24 Unemployment
summary(ur.df(data_ts[, "unemployment_15_24"], type = "none", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "unemployment_15_24"], type = "drift", selectlags = "AIC")) # Can reject at 1% 
summary(ur.df(data_ts[, "unemployment_15_24"], type = "trend", selectlags = "AIC")) # Can reject at 1%

# Logged GDP
summary(ur.df(data_ts[, "lgdp"], type = "none", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "lgdp"], type = "drift", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "lgdp"], type = "trend", selectlags = "AIC")) # Can not reject

# Inflation (already a first difference of the log CPIF)
summary(ur.df(data_ts[, "inflation_q"], type = "none", selectlags = "AIC")) # Can reject at 5%
summary(ur.df(data_ts[, "inflation_q"], type = "drift", selectlags = "AIC")) # Can reject at 5%
summary(ur.df(data_ts[, "inflation_q"], type = "trend", selectlags = "AIC")) # Can reject at 5%

# Interest rate 
summary(ur.df(data_ts[, "interest_rate"], type = "none", selectlags = "AIC")) # Can reject at 5%
summary(ur.df(data_ts[, "interest_rate"], type = "drift", selectlags = "AIC")) # Can reject at 5%
summary(ur.df(data_ts[, "interest_rate"], type = "trend", selectlags = "AIC")) # Can not reject 

# Youth LFP 
summary(ur.df(data_ts[, "youth_lfp"], type = "none", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "youth_lfp"], type = "drift", selectlags = "AIC")) # Can not reject
summary(ur.df(data_ts[, "youth_lfp"], type = "trend", selectlags = "AIC")) # Can not reject 


# To not reject the null hypothesis of trend stationarity, we need a small number.
# The alternative hypothesis of the KPSS test is existence of a unit root.

# KPSS test 
summary(ur.kpss(data_ts[, "unemployment_25_64"])) # Can reject at 10% 
summary(ur.kpss(data_ts[, "unemployment_15_24"])) # Can reject at 10% , Weak evidence for I(0)
summary(ur.kpss(data_ts[, "lgdp"])) # Can reject at 1% 
summary(ur.kpss(data_ts[, "inflation_q"])) # Can not reject
summary(ur.kpss(data_ts[, "interest_rate"])) # Can reject at 1% 
summary(ur.kpss(data_ts[, "youth_lfp"])) # Can reject at 1% 


# Based on the results log(GDP), interest rate and youth LFP should be differenced

# 15-25 Unemployment 
unemployment_data_clean$unemployment_25_64_diff <- c(NA, diff(unemployment_data_clean$unemployment_25_64))

# Difference log(GDP)
unemployment_data_clean$dlgdp <- c(NA, diff(unemployment_data_clean$lgdp))

# Difference interest rate
unemployment_data_clean$interest_rate_diff <- c(NA, diff(unemployment_data_clean$interest_rate))

# Difference youth LFP 
unemployment_data_clean$youth_lfp_diff <- c(NA, diff(unemployment_data_clean$youth_lfp))

# Remove first row with NA 
unemployment_data_clean <- unemployment_data_clean[!is.na(unemployment_data_clean$dlgdp), ]

# Recreate data_ts with the differenced variables
data_ts_diff <- ts(unemployment_data_clean[, c(  
                                                "unemployment_15_24", 
                                                "dlgdp", 
                                                "inflation_q",
                                                "interest_rate_diff",
                                                "youth_lfp_diff")],
                   start = c(2001, 3), frequency = 4)

# Run the ADF again to verify that the differenced variables is stationary

# unemployment age 25-64 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "trend", selectlags = "AIC")) # Can reject 5% 

# log GDP 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "trend", selectlags = "AIC")) # Can reject 1% 

# intrest rate 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "trend", selectlags = "AIC")) # Can reject 1% 

# youth LFP 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "trend", selectlags = "AIC")) # Can reject 1% 

###############################################################################

# Engle-Granger cointegration test
# Cointegration is about testing whether non-stationary series have a stationary linear combination.

# When working eith variables that are l(1) (i.e non-stationary in levels, but
# stationary after first difference), a test for cointegration in levels have to be done, not in differences. 

# In this case we can already conclude that inflation_q is stationary (l(0))

regression <- dynlm(unemployment_25_64 ~ lgdp + interest_rate + youth_lfp, data = data_ts)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC")) # test statistic -3.1063, can not reject the null


regression <- dynlm(lgdp ~ interest_rate + youth_lfp, data = data_ts)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC")) # -2.1049, can not reject the null


# So now we have to test cointegration between these variables that are l(1) pairwise: 
# lgdp ~ interest_rate 
# lgdp ~ youth_lfp 
# interest_rate ~ youth_lfp

# lgdp ~ interest_rate 
regression_1 <- dynlm(lgdp ~ interest_rate, data = data_ts)
summary(ur.df(regression_1$residuals, type = "drift", selectlags = "AIC")) # Can not reject -0.66

# lgdp ~ youth_lfp 
regression_2 <- dynlm(lgdp ~ youth_lfp, data = data_ts)
summary(ur.df(regression_2$residuals, type = "drift", selectlags = "AIC")) # Can not reject -2.75

# interest_rate ~ youth_lfp
regression_3 <- dynlm(interest_rate ~ youth_lfp, data = data_ts)
summary(ur.df(regression_3$residuals, type = "drift", selectlags = "AIC")) # Can not reject -2.85

# H0 -> the series are not cointegrated
# If we reject the null the two series are cointegrated 

# Compare test statistic to S&W Table: 
# Significance:  10%      5%      1%
# 1 regressors: -3.12 # -3.41 # -3.96
# 2 regressors: -3.52 # -3.80 # -4.36
# 3 regressors: -3.84 # -4.16 # -4.73
# 4 regressors: -4.20 # -4.49 # -5.07

# Interpretation 
# These variables, although individually l(1), do not move together in the long run
# in a stable equilibrium relationship. 

###############################################################################

# Variable order matters in Cholesky decomposition
# The ordering of the variables determines which variables are allowed to react 
# contemporaneously to others. 

# Justification of the order: 

# log-differenced GDP GDP is somewhat more reactive than unemployment but still reflects aggregate demand/supply shocks 
# that take time to materialize.

# Inflation reflects macroeconomic trends (e.g., supply chain pressures, wage growth), which respond to both 
# monetary policy and real activity.
# You can argue that inflation adjusts more slowly than interest rates, 
# but for this model i decide that this is most appropriate. 

# Interest rate are set relatively quickly in response to inflaiton, 
# unemployment, or GDP data. So this is ordered after inflation and GDP. 

# Youth LFP is more flexible and responsive to current labor market conditions 
# (e.g., they may choose to work or study). 

# Youth unemployment is highly sensitive to shocks and fluctuates more than total unemployment. 

data_ts_diff <- data_ts_diff [, c( "dlgdp", "inflation_q", "interest_rate_diff", "unemployment_15_24", "youth_lfp_diff")]

# All variables are now stationary (I(0))

# Determine apporpriate lag order using VARselect
VARselect(data_ts_diff, type = "const")

# AIC(n)  HQ(n)  SC(n) FPE(n)
   10     10      3     10  #the results when I included unemployment_25_64 

   10     5       3     5 

# Usually AIC is prefered 
# Because the suggested lag is > 6 it may signal issues (overfitting, autocorrelation in residuals)

# Use some of the other 
var_model <- VAR(data_ts_diff, p = 3, type = "const")
summary(var_model)

###############################################################################

# Serial correlation test: Ljung-Box (multivariate)

# VAR model (assumed already estimated as var_model with p = 3)
# Multivariate Ljung-Box test using Portmanteau test statistic

serial.test(var_model, lags.pt = 6, type = "PT.asymptotic") 

# Test for autocorrelation up to lag 9
serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")

# Test for autocorrelation up to lag 12 because i dont have that much data. 
serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")

# Normality test: Multivariate Jarque-Bera

# Perform the multivariate Jarque-Bera test
normality.test(var_model)$jb.mul$JB # Can reject at 1% 
# The residuals are not normal, this is not an assumption but to be noted.

###############################################################################

# Granger causality tests using the VAR model

# Helps test predictive relationships between variables in the VAR model. 
# In this case test if GDP and inflation help predict youth unemployment or the other way around. 

# Null hypothesis: the variable specified in `cause` does NOT Granger cause the others

# Does GDP Granger cause at least one of the other variables in the VAR
granger_gdp_to_youth <- causality(var_model, cause = "dlgdp", vcov. = sandwich::vcovHC(var_model))
granger_gdp_to_youth$Granger # p-value = 0.0004015 

# Does inflation Granger cause at least one of the other variables in the VAR
granger_inflation_to_youth <- causality(var_model, cause = "inflation_q", vcov. = sandwich::vcovHC(var_model))
granger_inflation_to_youth$Granger # p-value = 2.2e-16

# Does interest rate Granger cause at least one of the other variables in the VAR
granger_interest_rate_to_youth <- causality(var_model, cause = "interest_rate_diff", vcov. = sandwich::vcovHC(var_model))
granger_interest_rate_to_youth$Granger # p-value = 0.634

# Does youth LFP Granger cause at least one of the other variables in the VAR
granger_lfp_to_youth <- causality(var_model, cause = "youth_lfp_diff", vcov. = sandwich::vcovHC(var_model))
granger_lfp_to_youth$Granger # p-value = 0.1922

# In the full multivariate VAR system, we find strong evidence that GDP growth (p = 0.0004) and inflation (p < 0.0001) Granger-cause 
# at least one endogenous variable, indicating their broader influence within the macroeconomic system. 
# However, interest rates and youth labor force participation do not show predictive power in this context.

###############################################################################

# Impulse Response Functions (IRFs)
# Understad how one variable shocks another over time 
# Using the Cholesky decomposition 
# Periods in this is quarterly so period 4 is one year after the initial shock

# Compute IRF: effect of GDP growth on youth unemployment
IRF_gdp_youth <- irf(var_model, 
                     impulse = "dlgdp",          # Shock from GDP growth
                     response = "unemployment_15_24",  # Response in youth unemployment
                     ortho = TRUE,               # Cholesky decomposition (ordering matters!)
                     boot = TRUE,                # Include confidence intervals
                     ci = 0.95)                  # 95% confidence interval

# Compute IRF: effect of inflation on youth unemployment
IRF_infl_youth <- irf(var_model, 
                      impulse = "inflation_q", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)

# Compute IRF: effect of interest rate on youth unemployment
IRF_interest_rate_youth <- irf(var_model, 
                      impulse = "interest_rate_diff", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)

# Compute IRF: effect of youth LFP on youth unemployment
IRF_lfp_youth <- irf(var_model, 
                      impulse = "youth_lfp_diff", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)


# Plot the IRFs
plot(IRF_gdp_youth, main = "IRF: GDP growth → Youth Unemployment")
# A positive shock to GDP growth reduces youth unemployment significantly in the short run, 
# but the effect diminishes and becomes uncertain over time

plot(IRF_infl_youth, main = "IRF: Inflation → Youth Unemployment")
# The effect is mostly below zero in the early quarters, but the confidence band cross zero 
# almost the entire time, meaning this is not statistically significant. 
# By quarter 4–5, the response reverses, suggesting some long-term positive relationship, 
# but again not significant. 

plot(IRF_interest_rate_youth, main = "IRF: Interest rate → Youth Unemployment")

plot(IRF_lfp_youth, main = "IRF: Youth LFP → Youth Unemployment")



# Interpretations of the plots 
# There is a shock ex. Log GDP growth
# The black line is the estimated response of youth unemployment to a one-unit shock in ex. GDP growth. 
# The red line is the: 95 % bootstrap confidence interval (based on 100 replications)

# GDP growth -> Youth Unemployment 
# Immediate effect period 1: 
# The black line drops sharply right after the GDP shock. 
# This means that a positive GDP shock immediately reduces youth unemployment. 

# Short-run dynamics period 1-4
# The response remains significantly negative (i.e. withing the confidence bands), 
# suggesting a strong short-run effect. 

# Medium-run period 5-8
# The effect is still negative but dampens- youth unemployment starts to rebound slightly. 
# The confidence bands widen, implying increasing uncertainty. 

# Long-run periods 9-10 + 
# The black line moves closer to zero and crosses it. 
# The effect mey be less persistent or potentially reversed, but the confidence 
# bands cover zero, so not statistically significant anymore. 

###############################################################################

# Forecasting with the VAR model

# Set horizon (e.g. 40 quarters = 10 years)
hmax <- 40

# Forecast all variables in the VAR model
var_forecast <- forecast(var_model, h = hmax, fan = TRUE)

# Plot forecasts for each variable

# Interpretation 
# Black Line: Historical data.
# Blue Line: Point forecasts from the VAR model.
# Dark & Light Shaded Areas: Confidence intervals (fan chart) representing forecast uncertainty 
# — darker areas indicate more confidence.

# Forecast youth unemployment
plot(var_forecast$forecast$unemployment_15_24,
     main = "Forecast: Youth Unemployment",
     ylab = "Percentage",
     xlab = "Time")

# Interpretation 
# Youth unemployment shows strong seasonality and cyclical variation. The model captures this by projecting a wavy forecast line. 
# However, the fan widening indicates higher long-run forecast uncertainty. Youth unemployment is less predictable than inflation or GDP growth.

# Forecast GDP growth
plot(var_forecast$forecast$dlgdp,
     main = "Forecast: GDP Growth (log difference)",
     ylab = "Growth rate",
     xlab = "Time")

# Interpretation 
# The model predicts relatively stable GDP growth, reverting to a steady average post-2020. 
# The sharp dip around 2020 is the COVID-19 shock. The forecast is centered around zero, with wide uncertainty bands, 
# reflecting uncertainty in economic activity recovery.

# Forecast inflation
plot(var_forecast$forecast$inflation_q,
     main = "Forecast: Quarterly Inflation",
     ylab = "Rate (%)",
     xlab = "Time")

# Interpretation
# The model expects inflation to stabilize around a new mean level after a post-pandemic shock (large volatility spike around 2020). 
# However, there's substantial uncertainty, especially in the longer horizon (note the widening fan).

# Forecast interest rate
plot(var_forecast$forecast$interest_rate_diff,
     main = "Forecast: Interest Rate",
     ylab = "Rate (%)",
     xlab = "Time")

# Forecast youth LFP 
plot(var_forecast$forecast$youth_lfp_diff,
     main = "Forecast: Youth LFP",
     ylab = "Rate (%)",
     xlab = "Time")

###############################################################################

# Forecast error variance decompositions (FEVD)

FEVD <- fevd(var_model)

round(FEVD$dlgdp*100,1)
round(FEVD$inflation_q*100,1)
round(FEVD$interest_rate_diff*100,1)
round(FEVD$unemployment_15_24*100,1)
round(FEVD$youth_lfp_diff*100,1)

# How much of the forecast error variance of the variable unemployment_15_24 at 
# diffrent horizons (step 1 to 10) is explained by shocks to each variable in the VAR. 

FEVD

# Convert each component of FEVD to a tidy data frame
fevd_list <- lapply(names(FEVD), function(var_name) {
  df <- as.data.frame(FEVD[[var_name]])
  df <- round(df * 100, 1)
  df$Horizon <- 1:nrow(df)
  df_long <- df %>%
    pivot_longer(-Horizon, names_to = "ContributingVariable", values_to = "Percent_Variance")
  df_long$ResponseVariable <- var_name
  return(df_long)
})

# Combine all response variables into one data frame
fevd_all <- bind_rows(fevd_list)

# Plot
ggplot(fevd_all, aes(x = factor(Horizon), y = Percent_Variance, fill = ContributingVariable)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ ResponseVariable, ncol = 1, scales = "free_y") +
  labs(
    title = "FEVD for Multiple Response Variables",
    x = "Horizon",
    y = "Percentage",
    fill = "Contributing Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", color = "darkblue"),
    legend.position = "right"
  )
