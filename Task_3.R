#INSTALLING NECESSARY LIBRARIES
install.packages('readxl')
install.packages('ggplot2')
install.packages('forecast')
install.packages('TTR')
install.packages("dplyr")  # Only needed if dplyr is not already installed
if (!require(tseries)) install.packages("tseries")

#LOADING NECESSARY LIBRARIES
library(readxl)      # For reading CSV files
library(ggplot2)    # For data visualization
library(forecast)
library(TTR)
library(tseries)
library(dplyr)

#LOADING DATA SET
birth_data<-read_excel('birth_rate.xlsx')


#EXPLORATION OF DATA

# Extract the columns for Year, Total Births (UK), and 
#Total Births (England & Wales)
birth_data_clean <- birth_data %>% select(`Year`, 
          `Number of live births: United Kingdom`, 
         `Number of live births: England and Wales`) %>%
  filter(!is.na(Year) & !is.na(`Number of live births: United Kingdom`) 
         & !is.na(`Number of live births: England and Wales`))
# Reverse the order of rows if necessary
birth_data_clean <- birth_data_clean[nrow(birth_data_clean):1, ]

# Check the first few rows to ensure correct ordering
head(birth_data_clean)


# Rename columns for simplicity
colnames(birth_data_clean) <- 
  c("Year", "UK_Births", "England_Wales_Births")

#PREPROCESSING

# Convert to a time series object
uk_births_ts <- ts(birth_data_clean$UK_Births,
                   start = min(birth_data_clean$Year), frequency = 1)

uk_births_ts
eng_wales_births_ts <- ts(birth_data_clean$England_Wales_Births, 
                   start = min(birth_data_clean$Year), frequency = 1)
eng_wales_births_ts

uk_births_ts <- as.numeric(gsub(":", NA, uk_births_ts))
# Check if NAs were replaced
sum(is.na(uk_births_ts))
# Remove missing values from the time series
uk_births_ts <- na.omit(uk_births_ts)
# Check the result
sum(is.na(uk_births_ts))  # Should be 0

#VISUALIZATION

#PLOTTING TIME SERIES
plot.ts(uk_births_ts)
plot.ts(eng_wales_births_ts)

#ACF

acf(uk_births_ts, main = "ACF of UK Births")
acf(eng_wales_births_ts, main = "ACF of England & Wales Births")

adf_result <- adf.test(uk_births_ts, alternative = "stationary")
print(adf_result)

adf_result1 <- adf.test(eng_wales_births_ts, alternative = "stationary")
print(adf_result1)

#MAKING DATA STATIONARY
differenced_series_uk <- diff(uk_births_ts)
plot(differenced_series_uk, main = "Differenced Time Series UK")
acf(differenced_series_uk, main = "ACF of Differenced SeriesUK")
adf.test(differenced_series_uk)

differenced_series_eng <- diff(eng_wales_births_ts)
plot(differenced_series_eng, main = "Differenced Time Series UK")
acf(differenced_series_eng, main = "ACF of Differenced SeriesUK")
adf.test(differenced_series_eng)

# Decompose the UK births time series
#uk_decomp_SMA3<- SMA(uk_births_ts,n=3)
#plot.ts(uk_decomp_SMA3)

#uk_decomp_SMA8<- SMA(uk_births_ts,n=8)
#plot.ts(uk_decomp_SMA8)


#eng_wales_decomp_SMA3 <- SMA(eng_wales_births_ts,n=3)
#plot.ts(eng_wales_decomp_SMA3)

#eng_wales_decomp_SMA8 <- SMA(eng_wales_births_ts,n=8)
#plot.ts(eng_wales_decomp_SMA8)

#FORECASTING

#ARIMA
uk_arima <- auto.arima(uk_births_ts)  # For UK births
eng_wales_arima <- auto.arima(eng_wales_births_ts)  # For England & Wales

# Forecast next 10 years
uk_forecast <- forecast(uk_arima, h = 10)
eng_wales_forecast <- forecast(eng_wales_arima, h = 10)

# Plot forecasts
plot(uk_forecast, main = "UK Births Forecast")
plot(eng_wales_forecast, main = "England & Wales Births Forecast")

#HOLT-Winters
uk_holt <- HoltWinters(uk_births_ts, beta = FALSE, gamma = FALSE)
eng_wales_holt <- HoltWinters(eng_wales_births_ts, beta = FALSE, 
                              gamma = FALSE)

# Forecast next 10 years
uk_holt_forecast <- forecast(uk_holt, h = 10)
eng_wales_holt_forecast <- forecast(eng_wales_holt, h = 10)

# Plot forecasts
plot(uk_holt_forecast, main = "Holt's Linear Model - UK Births Forecast")
plot(eng_wales_holt_forecast, main = "Holt's Linear Model - 
     England & Wales Births Forecast")


#ACCURACY
accuracy(uk_forecast)  # ARIMA accuracy for UK
accuracy(uk_holt_forecast)  # Holt accuracy for UK

accuracy(eng_wales_forecast)  # ARIMA accuracy for England_Wales
accuracy(eng_wales_holt_forecast)  # Holt accuracy for England_Wales

checkresiduals(uk_arima)  # For ARIMA
checkresiduals(uk_holt)   # For Holt Model

checkresiduals(eng_wales_arima)  # For ARIMA
checkresiduals(eng_wales_holt)   # For Holt Model
