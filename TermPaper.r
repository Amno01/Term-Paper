# Term Paper
# EC7413
# Spring 2025 

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
library(paletteer)
library(xtable)

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
# write.xlsx(interest_rate, "../Data/interest_rates_quarterly_2001_2024.xlsx", overwrite = TRUE)

###############################################################################

# Unemployment 25-64 years, take average of the 4 age groups 

# File names in order
unemployment_25_64_list <- c("../Data/unemployment_25_34.xlsx", "../Data/unemployment_35_44.xlsx", "../Data/unemployment_45_54.xlsx", "../Data/unemployment_55_64.xlsx")

# Read all data frames
unemployment_list <- lapply(unemployment_25_64_list, read_excel)

# Rename columns to indicate their age group
age_groups <- c("25_34", "35_44", "45_54", "55_64")
unemployment_list <- Map(function(df, age) {
  colnames(df)[colnames(df) != "date"] <- paste0("unemployment_", age)
  df
}, unemployment_list, age_groups)

# Merge all data frames by date
unemployment_merged <- Reduce(function(x, y) full_join(x, y, by = "date"), unemployment_list) %>%
  arrange(date)

# Double-check that we are only averaging the intended 4 columns
unemployment_rate_columns <- c("unemployment_25_34", "unemployment_35_44", "unemployment_45_54", "unemployment_55_64")

# Create new column: average unemployment for 25-64
unemployment_merged <- unemployment_merged %>%
  mutate(unemployment_25_64 = rowMeans(across(all_of(unemployment_rate_columns)), na.rm = TRUE))

# Drop all the other age groups other than the total
unemployment_merged$unemployment_25_34<- NULL
unemployment_merged$unemployment_35_44<- NULL
unemployment_merged$unemployment_45_54<- NULL
unemployment_merged$unemployment_55_64<- NULL

# Write to a new Excel file
# write_xlsx(unemployment_merged, "../Data/unemployment_25_64.xlsx")

###############################################################################

# Combine the data files 

# File names in order
file_list <- c("../Data/unemployment_25_64.xlsx", "../Data/15-24_unemployment_2001_2024.xlsx",
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
# write_xlsx(merged_data, "../Data/unemployment_gdp_cpif_rate_lfp_data.xlsx")

###############################################################################

# Reserch question: 
# How is youth unemployment affected by macroeconomic variables such as GDP and inflation? 
# A VAR analysis on Swedish quarterly data 2001–2024. 

# Read in the data for 2001-2024
unemployment_data <- read_excel( "../Data/unemployment_gdp_cpif_rate_lfp_data.xlsx")

# Take the logs to get the relative changes or growth rates: 
# CPIF use the first difference of the log to get quarterly inflation.  
# Log-transforming GDP helps you interperet the differences as growth rates. 

# Calculate inflation and GDP growth variables
unemployment_data$inflation_q <- c(NA, 100 * diff(log(unemployment_data$cpif))) 
unemployment_data$lgdp <- log(unemployment_data$gdp)

# Remove first row with NA due to differencing
unemployment_data_clean <- unemployment_data[!is.na(unemployment_data$inflation_q), ]

# Create time series object from the cleaned data frame
data_ts <- ts(unemployment_data_clean[, c("unemployment_25_64", "unemployment_15_24", "lgdp", "inflation_q", "interest_rate", "youth_lfp")],
              start = c(2001, 2), frequency = 4)

# Create a data frame for the plot (later on unemployment 25-64 is not used)
data_ts_plot <- data_ts[, colnames(data_ts) != "unemployment_25_64"]

# Extract the color palette
colorblind_palette <- as.character(paletteer_d("wesanderson::Darjeeling2", n = 5))

# Save plot to PNG
png("../Output/swedish_macro_plot.png", width = 1000, height = 600)

# Plot the time series with colorblind-safe palette
data_plot <- ts.plot(data_ts_plot,
            col = colorblind_palette,
            ylab = "Levels / % Changes",
            lty = 1,
            lwd = 2)

# Add a matching legend
legend(x = 2020, y = 45,
       legend = c("Unemployment ages 15-24", "log(GDP)",
                  "Quarterly Inflation", "Interest Rate", "Youth LFP"),
       col = colorblind_palette,
       lty = 1,
       lwd = 2)

# Close the PNG device
dev.off()

# Interpretation of the time series plot:

# Unemployment 25-64 
# Seasonal pattern around a relatively stable mean, likely stationary. 

# Unemployment 15-24 
# Apparent fluctuations with apparent cyclical behavior. The mean level seems to change over time, 
# Especially increasing after 2008 and showing high volatility. 
# The series does not appear stationary - it likely has a unit root. 

# Log GDP 
# Follows a upward trend with minor dips. This is probably indicates a non-stationary variable. 

# Quarterly inflation 
# Seasonal pattern with spikes and dips, but no obvious long-term trend. 
# This series may be stationary around a mean with seasonal components. 

# Interest rate 
# Bounded, lower-level variation, especially in later years. 
# The series looks stationary or weakly non-stationary. 

# Youth LFP 
# Fluctuations around a relatively stable mean, likely stationary. 

###############################################################################

# ADF test 
# H0: variable has a unit root (non-stationary)
# Want to reject H0 to conclude stationarity. 

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

# KPSS test
# H0: trend stationarity, need a small number not to reject. 
# The alternative hypothesis of the KPSS test is existence of a unit root.

summary(ur.kpss(data_ts[, "unemployment_25_64"])) # Can reject at 10% 
summary(ur.kpss(data_ts[, "unemployment_15_24"])) # Can reject at 10%
summary(ur.kpss(data_ts[, "lgdp"])) # Can reject at 1% 
summary(ur.kpss(data_ts[, "inflation_q"])) # Can not reject
summary(ur.kpss(data_ts[, "interest_rate"])) # Can reject at 1% 
summary(ur.kpss(data_ts[, "youth_lfp"])) # Can reject at 1% 


# Based on the results from ADF and KPSS: 
# Unemployment 25-64, log(GDP), interest rate and youth LFP should be differenced

# I decide not to difference 15-24 because ADF test with drift and trend reject the null of a unit root at 1% level. 
# Given that KPSS is a reject heavy test, rejecting at 10% level is weak evidence against stationariy. 
# Inflation has already been diffed previously from the tests seems stationary l(0). 

# Taking diff of the I(1) variables
# 25-64 Unemployment 
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
data_ts_diff <- ts(unemployment_data_clean[, c( "unemployment_25_64_diff", 
                                                "unemployment_15_24", 
                                                "dlgdp", 
                                                "inflation_q",
                                                "interest_rate_diff",
                                                "youth_lfp_diff")],
                   start = c(2001, 3), frequency = 4)

# Run the ADF again to verify that the differenced variables is stationary

# Unemployment ages 25-64 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "unemployment_25_64_diff"], type = "trend", selectlags = "AIC")) # Can reject 15% 

# Log GDP 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "dlgdp"], type = "trend", selectlags = "AIC")) # Can reject 1% 

# Interest rate 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "interest_rate_diff"], type = "trend", selectlags = "AIC")) # Can reject 1% 

# Youth LFP 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "none", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "drift", selectlags = "AIC")) # Can reject 1% 
summary(ur.df(data_ts_diff[, "youth_lfp_diff"], type = "trend", selectlags = "AIC")) # Can reject 1% 

###############################################################################

# Engle-Granger cointegration test
# Cointegration is about testing whether non-stationary series have a stationary linear combination.

# When working with variables that are l(1) (i.e non-stationary in levels, but
# stationary after first difference), a test for cointegration in levels have to be done, not in differences. 

# H0: the series are not cointegrated
# If we reject the null the two series are cointegrated 

# Compare test statistic to S&W Table p.665
# Significance:  10%      5%      1%
# 1 regressors: -3.12 # -3.41 # -3.96
# 2 regressors: -3.52 # -3.80 # -4.36
# 3 regressors: -3.84 # -4.16 # -4.73
# 4 regressors: -4.20 # -4.49 # -5.07

# A test for cointegration between: unemployment ages 25-64, log GDP, interest rate and youth LFP 
regression <- dynlm(unemployment_25_64 ~ lgdp + interest_rate + youth_lfp, data = data_ts)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC")) # test statistic -3.1063, 
# can not reject the null with 3 regressors from the S&W table

# A test for cointegration between: log GDP, interest rate and youth LFP 
regression <- dynlm(lgdp ~ interest_rate + youth_lfp, data = data_ts)
summary(ur.df(regression$residuals, type = "drift", selectlags = "AIC")) # test statistic -2.1049, 
# can not reject the null with 2 regressors from the S&W table

# These variables, although individually l(1), the test indicates that they do not move together in 
# in a stable equilibrium relationship in the long run. 

###############################################################################

# Variable order matters in Cholesky decomposition
# The ordering of the variables determines which variables are allowed to react 
# contemporaneously to others. 

# Justification of the order: 

# Log-differenced GDP is somewhat more reactive than unemployment but still reflects aggregate demand/supply shocks 
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


data_ts_diff <- data_ts_diff [, c( "unemployment_25_64_diff", "dlgdp", "inflation_q", "unemployment_15_24", "youth_lfp_diff", "interest_rate_diff")]

data_ts_new <- data_ts_diff [, c( "dlgdp", "inflation_q", "unemployment_15_24", "youth_lfp_diff", "interest_rate_diff")]

# Determine apporpriate lag order 
VARselect(data_ts_diff, type = "const")

# Determine appropriate lag order without unemployment 25-64 
VARselect(data_ts_new, type = "const")

# AIC(n)  HQ(n)  SC(n) FPE(n)
   10     10      3     10  # The results when I included unemployment ages 25-64 

   10     5       3      5  # The results when not including unemployment ages 25-64 

# Based on the results of the appropriate lag I choose not to include unemployment ages 25-64. 
# It likely introduces serial correlation and complex dynamic relationships that require more lags to capture. 
# Likely overlaps with youth unemployment that is the variable of interest. 

# Usually AIC is prefered but it favors more more complex models suggesting higher lag lengths. 
# SC imposes stronger penalty on model complexity and is more suitable because it captures 
# the essensial dynamics without overfitting or reducing the model´s robustness. 
# But it should be notet that it is a warning that the AIC suggested lag is > 6 
# because it may signal issues (overfitting, autocorrelation in residuals). 

# Estimate the reduced form VAR with p=3 
var_model_3 <- VAR(data_ts_new, p = 3, type = "const")
summary(var_model_3)

# Estimate the reduced form VAR with p=5 
var_model_5 <- VAR(data_ts_new, p = 5, type = "const")
summary(var_model_5)

###############################################################################

# Serial correlation test: Ljung-Box (multivariate)
# H0: no serial correlation up to whatever chosen lag length.

# Multivariate Ljung-Box test using Portmanteau test statistic for p=3 

# Test for autocorrelation up to lag 6
serial.test(var_model_3, lags.pt = 6, type = "PT.asymptotic") # Can reject 1% 

# Test for autocorrelation up to lag 9
serial.test(var_model_3, lags.pt = 9, type = "PT.asymptotic") # Can reject 1% 

# Test for autocorrelation up to lag 12
serial.test(var_model_3, lags.pt = 12, type = "PT.asymptotic") # Can reject 1% 

# Test for autocorrelation up to lag 16, no more than that for small amount of data. 
serial.test(var_model_3, lags.pt = 16, type = "PT.asymptotic") # Can reject 5% 

# Test when p=5, for robustness check

# Test for autocorrelation up to lag 10
serial.test(var_model_5, lags.pt = 10, type = "PT.asymptotic") # Can reject 1% 

# Test for autocorrelation up to lag 15
serial.test(var_model_5, lags.pt = 15, type = "PT.asymptotic") # Can not reject 

# Test for autocorrelation up to lag 20, over 16 have to have a lot of observations. 
serial.test(var_model_5, lags.pt = 20, type = "PT.asymptotic") # Can not reject

# SC selected a lag length of p = 3, residual autocorrelation tests revealed significant 
# serial correlation at multiple lags (up to lag 16). This indicates potential misspecification.
# There is signs of residual autocorrelation but to avoid overfitting I choose p=3. 

# Increasing the lag length to p=5 improved residual diagnostics with 
# no significant autocorrelation up to lag 15. But the cost of this is more parameters, 
# which have to be noted when I have 94 observations. 

# Normality test: Multivariate Jarque-Bera
# H0: residuals are normally distributed 

# Perform the multivariate Jarque-Bera test
normality.test(var_model_3)$jb.mul$JB # Can reject at 1% 
# The residuals are not normal, this is not an assumption but should be noted. 

###############################################################################

# Granger causality tests using the VAR model

# Helps test predictive relationships between variables in the VAR model. 
# H0: the variable specified in `cause` does NOT Granger cause at least one of the other variables. 

# Does GDP Granger cause at least one of the other variables in the VAR
granger_youth <- causality(var_model_3, cause = "unemployment_15_24", vcov. = sandwich::vcovHC(var_model_3))
granger_youth$Granger # p-value = 0.005077, can reject 1% 

# Does GDP Granger cause at least one of the other variables in the VAR
granger_gdp <- causality(var_model_3, cause = "dlgdp", vcov. = sandwich::vcovHC(var_model_3))
granger_gdp$Granger # p-value = 0.0004015, can reject 1% 

# Does inflation Granger cause at least one of the other variables in the VAR
granger_inflation <- causality(var_model_3, cause = "inflation_q", vcov. = sandwich::vcovHC(var_model_3))
granger_inflation$Granger # p-value = 2.2e-16, can reject 1% 

# Does interest rate Granger cause at least one of the other variables in the VAR
granger_interest_rate <- causality(var_model_3, cause = "interest_rate_diff", vcov. = sandwich::vcovHC(var_model_3))
granger_interest_rate$Granger # p-value = 0.634, can not reject

# Does youth LFP Granger cause at least one of the other variables in the VAR
granger_lfp <- causality(var_model_3, cause = "youth_lfp_diff", vcov. = sandwich::vcovHC(var_model_3))
granger_lfp$Granger # p-value = 0.1922, can not reject

# In the full multivariate VAR system, there is evidence that Youth unemployment, GDP growth and inflation Granger-cause 
# at least one endogenous variable, indicating their broader influence within the macroeconomic system. 
# However, interest rates and youth labor force participation do not show predictive power in this context.


# Create a table with p-values from results
granger_pvals <- tibble::tibble(
  Cause = c("Unemployment 15-24", "GDP Growth", "Inflation", "Interest Rate", "Youth LFP"),
  `p-value` = c(
    signif(granger_youth$Granger$p.value, 4),
    signif(granger_gdp$Granger$p.value, 4),
    signif(granger_inflation$Granger$p.value, 4),
    signif(granger_interest_rate$Granger$p.value, 4),
    signif(granger_lfp$Granger$p.value, 4)
  )
)

# Create xtable and save to file
print(
  xtable(granger_pvals, digits = c(0, 0, 4), caption = "Granger causality test"),
  file = "../Output/granger_pvals_latex.txt",
  include.rownames = FALSE
)

###############################################################################

# Impulse Response Functions (IRFs)
# Understad how one variable shocks another over time 
# Using the Cholesky decomposition 
# Periods in this data is quarterly so period 4 is one year after the initial shock

# Function to convert VAR::irf output to a tidy data frame with shaded CI
irf_to_df <- function(irf_obj, impulse_name) {
  tibble(
    Horizon = 0:(length(irf_obj$irf[[1]]) - 1),
    Response = irf_obj$irf[[1]],
    Lower = irf_obj$Lower[[1]],
    Upper = irf_obj$Upper[[1]],
    Impulse = impulse_name
  )
}

# Compute IRF: effect of GDP growth on youth unemployment
IRF_gdp_youth <- irf(var_model_3, 
                     impulse = "dlgdp",          
                     response = "unemployment_15_24",
                     ortho = TRUE,               
                     boot = TRUE,                
                     ci = 0.95)                  

# Compute IRF: effect of inflation on youth unemployment
IRF_infl_youth <- irf(var_model_3, 
                      impulse = "inflation_q", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)

# Compute IRF: effect of interest rate on youth unemployment
IRF_interest_rate_youth <- irf(var_model_3, 
                      impulse = "interest_rate_diff", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)

# Compute IRF: effect of youth LFP on youth unemployment
IRF_lfp_youth <- irf(var_model_3, 
                      impulse = "youth_lfp_diff", 
                      response = "unemployment_15_24", 
                      ortho = TRUE, 
                      boot = TRUE, 
                      ci = 0.95)

# Combine all into one tidy data frame
irf_df_all <- bind_rows(
  irf_to_df(IRF_gdp_youth, "GDP Growth"),
  irf_to_df(IRF_infl_youth, "Inflation"),
  irf_to_df(IRF_interest_rate_youth, "Interest Rate"),
  irf_to_df(IRF_lfp_youth, "Youth LFP")
)

# Plot the IRFs
plot(IRF_gdp_youth, main = "IRF: GDP growth → Unemployment 15-24")
# A positive shock to GDP growth reduces youth unemployment significantly in the short run, 
# but the effect diminishes and becomes uncertain over time

plot(IRF_infl_youth, main = "IRF: Inflation → Unemployment 15-24")
# The effect is mostly below zero in the early quarters, but the confidence band cross zero 
# almost the entire time, meaning this is not statistically significant. 
# By quarter 4–5, the response reverses, suggesting some long-term positive relationship, 
# but again not significant. 

plot(IRF_interest_rate_youth, main = "IRF: Interest rate → Unemployment 15-24")

plot(IRF_lfp_youth, main = "IRF: Youth LFP → Unemployment 15-24")

# Plot using shaded CI area
irf_plot <- ggplot(irf_df_all, aes(x = Horizon, y = Response)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  facet_wrap(~ Impulse, scales = "free_y", ncol = 2) +
  labs(
    y = "Response",
    x = "Horizon (Quarters)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

# Save to PNG
# ggsave("../Output/irf_youth_unemployment.png", plot = irf_plot, width = 10, height = 6.5, dpi = 300)

###############################################################################

# Forecasting with the VAR model

# Set horizon (e.g. 40 quarters = 10 years)
hmax <- 40

# Forecast all variables in the VAR model
var_forecast <- forecast(var_model_3, h = hmax, fan = TRUE)

# Set up the plotting area 
# png("../Output/var_forecasts.png", width = 1200, height = 900) 
# par(mfrow = c(3, 2), mar = c(4, 4, 3, 2)) 

# Plot forecasts for each variable 

# Forecast youth unemployment
plot(var_forecast$forecast$unemployment_15_24,
     main = "Forecast: Unemployment 15-24",
     ylab = "Percentage",
     xlab = "Time")

# Interpretation 
# Youth unemployment shows strong seasonality and cyclical variation. The model captures this by projecting a wavy forecast line. 
# However, the fan widening indicates higher long-run forecast uncertainty. Youth unemployment is less predictable than inflation or GDP growth.

# Forecast GDP growth
plot(var_forecast$forecast$dlgdp,
     main = "Forecast: GDP Growth",
     ylab = "Growth rate",
     xlab = "Time")

# Interpretation 
# The model predicts relatively stable GDP growth, reverting to a steady average post-2020. 
# The sharp dip around 2020 is the COVID-19 shock. The forecast is centered around zero, with wide uncertainty bands, 
# reflecting uncertainty in economic activity recovery.

# Forecast inflation
plot(var_forecast$forecast$inflation_q,
     main = "Forecast: Inflation",
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

# Finish saving the file
dev.off()  

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
    pivot_longer(-Horizon, names_to = "contributing_variable", values_to = "percent_variance")
  df_long$response_variable <- var_name
  return(df_long)
})

# Combine all response variables into one data frame
fevd_all <- bind_rows(fevd_list)

# Rename Contributing Variables (legend)
fevd_all$contributing_variable <- recode(fevd_all$contributing_variable,
  "dlgdp" = "GDP Growth",
  "inflation_q" = "Inflation",
  "interest_rate_diff" = "Interest Rate",
  "unemployment_15_24" = "Unemployment 15-24",
  "youth_lfp_diff" = "Youth LFP"
)

# Rename Response Variables (facet titles)
fevd_all$response_variable <- recode(fevd_all$response_variable,
  "dlgdp" = "GDP Growth",
  "inflation_q" = "Inflation",
  "interest_rate_diff" = "Interest Rate",
  "unemployment_15_24" = "Unemployment 15-24",
  "youth_lfp_diff" = "Youth LFP"
)

# Plot
fevd_plot <- ggplot(fevd_all, aes(x = factor(Horizon), y = percent_variance, fill = contributing_variable)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ response_variable, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = colorblind_palette) +
  labs(
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


# Save as PNG
#ggsave("../Output/fevd_plot.png", plot = fevd_plot, width = 8, height = 10, dpi = 300)
