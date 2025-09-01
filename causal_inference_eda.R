library(dplyr)
library(readr)
library(CausalImpact)
library(lubridate)
library(prophet)

setwd("/Users/nicholasknauer/Downloads")

sales <- read_csv('sales.csv')
marketing_spend <- read_csv('marketing_spend.csv')

data_combined <- sales %>%
  select(date, city_a_sales = city_a) %>%
  left_join(marketing_spend %>% select(date, city_a_spend = city_a, online, tv), by = "date")

# Ensure data is sorted by date and no missing dates for the analysis period
data_combined <- data_combined %>%
  arrange(date) %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2023-12-31"))

# Convert to a time series matrix as required by CausalImpact
model_ts <- data_combined %>%
  select(city_a_sales, city_a_spend, online, tv) %>%  # Keep only necessary columns
  as.matrix()

# Instead of using dates, find the indices for pre and post periods
pre_start_index <- which(data_combined$date == as.Date("2020-01-01"))
pre_end_index <- which(data_combined$date == as.Date("2023-07-30"))
post_start_index <- which(data_combined$date == as.Date("2023-07-31"))
post_end_index <- which(data_combined$date == as.Date("2023-12-31"))

# Defining pre.period and post.period using indices
pre_period <- c(pre_start_index, pre_end_index)
post_period <- c(post_start_index, post_end_index)

# Run the CausalImpact analysis
impact_result <- CausalImpact(model_ts, pre_period, post_period)

# Display the results
summary(impact_result)

summary(impact_result, "report")

#Posterior inference {CausalImpact}

#Average          Cumulative        
#Actual                   16194            2493828           
#Prediction (s.d.)        15874 (260)      2444640 (40039)   
#95% CI                   [15338, 16357]   [2362027, 2519049]

#Absolute effect (s.d.)   319 (260)        49188 (40039)     
#95% CI                   [-164, 856]      [-25221, 131801]  

#Relative effect (s.d.)   2% (1.7%)        2% (1.7%)         
#95% CI                   [-1%, 5.6%]      [-1%, 5.6%]       

#Posterior tail-area probability p:   0.10429
#Posterior prob. of a causal effect:  90%

plot(impact_result)

daily_results <- data.frame(impact_result$series)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-12-31")

# Create a sequence of dates from start to end date
date_range <- seq.Date(from = start_date, to = end_date, by = "day")

# Display the first few dates in the sequence
head(date_range)

daily_results_with_date <- cbind(daily_results, date_range)

pre_period <- filter(daily_results_with_date, date_range < as.Date('2023-07-31'))

# Assuming df is your dataframe with actual, predicted columns
actual <- pre_period$response
predicted <- pre_period$point.pred

# Calculate MSE
mse <- mean((actual - predicted)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate R-squared
sst <- sum((actual - mean(actual))^2) # Total sum of squares
ssr <- sum((actual - predicted)^2) # Sum of squares of residuals
r_squared <- 1 - (ssr / sst)

# Print the results
cat("MSE:", mse, "\nRMSE:", rmse, "\nR-squared:", r_squared, "\n")

library(ggplot2)
daily_results_post <- filter(daily_results_with_date, date_range >= as.Date('2023-07-31'))
ggplot(daily_results_post, aes(x=date_range, y=cum.effect)) +
  geom_line() +
theme_minimal() + 
  labs(x = "Date", y = "Point Difference", title = "Point Difference over Time") 

marketing_spend_vs_effect <- left_join(daily_results_post, marketing_spend, by=c("date_range"="date"))

# Load the necessary library
library(ggplot2)
library(dplyr)

# Assuming your dataframe is named df
df_long <- marketing_spend_vs_effect %>%
  pivot_longer(cols = c("city_a", "tv", "online"), names_to = "category", values_to = "value") 

# Basic line plot
p <- ggplot(marketing_spend_vs_effect, aes(x = date_range)) +
  geom_line(aes(y = cum.effect), colour = "blue") + # Plotting cum.effect with primary y-axis
  geom_line(data = df_long, aes(x = date_range, y = value, colour = category)) + # Plotting the rest on the same axis, intending to use as secondary
  scale_y_continuous(name = "cum.effect",
                     sec.axis = sec_axis(~., name = "Other Metrics")) + # Secondary axis mirroring the primary
  labs(x = "Date", colour = "Metric") +
  theme_minimal()

print(p)

write_csv(marketing_spend_vs_effect, 'marketing_spend_vs_effect.csv')

##EDA

# Basic summary statistics
summary(data_combined)

##City A Time Series Decomposition

library(tidyverse)
library(lubridate)
library(forecast)

ts_city_a <- ts(data_combined$city_a_sales, start = c(2020, 1), frequency = 365)

# Decompose the time series
decomposed <- stl(ts_city_a, s.window = "periodic")

# Plot the decomposed components
plot(decomposed)

# Plotting time series for City A sales
ggplot(data_combined, aes(x = date)) +
  geom_line(aes(y = city_a_sales, colour = "City_A_Sales")) +
  geom_line(aes(y = online, colour = "Online Spend")) +
  geom_line(aes(y = city_a_spend, colour = "City_A_Spend")) + 
  geom_line(aes(y = tv, colour = "TV")) +
  labs(title = "City A Sales and Spend Over Time",
       x = "Date", y = "Amount") +
  scale_color_manual("", values = c("City_A_Sales" = "blue", "Online Spend" = "red", "City_A_Spend" = "green", "TV" = "purple"))


# Fit linear model to City A sales
lm_sales_a <- lm(city_a_sales ~ as.numeric(date), data = data_combined)
summary(lm_sales_a)

# Plotting the trend line
ggplot(data_combined, aes(x = date, y = city_a_sales)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Trend Line for City A Sales", x = "Date", y = "Sales")

# Assuming 'decomposed' is the result of the STL decomposition in R.
seasonal_component <- decomposed$time.series[, "seasonal"]
plot(seasonal_component, type = "l")

# Using the `anomalize` package for anomaly detection
library(anomalize)

data_combined %>%
  select(date, city_a_sales) %>%
  anomalize(city_a_sales) %>%
  time_decompose(city_a_sales) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

##City B

data_combined_b <- sales %>%
  select(date, city_b_sales = city_b) %>%
  left_join(marketing_spend %>% select(date, city_b_spend = city_b, online, tv), by = "date")

ts_city_b <- ts(data_combined_b$city_b_sales, start = c(2020, 1), frequency = 365)

# Decompose the time series
decomposed_b <- stl(ts_city_b, s.window = "periodic")

# Plot the decomposed components
plot(decomposed_b)

# Plotting time series for City B sales
ggplot(data_combined_b, aes(x = date)) +
  geom_line(aes(y = city_b_sales, colour = "City_B_Sales")) +
  geom_line(aes(y = online, colour = "Online Spend")) +
  geom_line(aes(y = city_b_spend, colour = "City_B_Spend")) + 
  geom_line(aes(y = tv, colour = "TV")) +
  labs(title = "City B Sales and Spend Over Time",
       x = "Date", y = "Amount") +
  scale_color_manual("", values = c("City_B_Sales" = "blue", "Online Spend" = "red", "City_B_Spend" = "green", "TV" = "purple"))

# Fit linear model to City B sales
lm_sales_b <- lm(city_b_sales ~ as.numeric(date), data = data_combined_b)
summary(lm_sales_b)

# Plotting the trend line
ggplot(data_combined_b, aes(x = date, y = city_b_sales)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Trend Line for City B Sales", x = "Date", y = "Sales")

# Assuming 'decomposed' is the result of the STL decomposition in R.
seasonal_component <- decomposed_b$time.series[, "seasonal"]
plot(seasonal_component, type = "l")

# Using the `anomalize` package for anomaly detection
library(anomalize)

data_combined_b %>%
  select(date, city_b_sales) %>%
  anomalize(city_b_sales) %>%
  time_decompose(city_b_sales) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

##City C

data_combined_c <- sales %>%
  select(date, city_c_sales = city_c) %>%
  left_join(marketing_spend %>% select(date, city_c_spend = city_c, online, tv), by = "date")

ts_city_c <- ts(data_combined_c$city_c_sales, start = c(2020, 1), frequency = 365)

# Decompose the time series
decomposed_c <- stl(ts_city_c, s.window = "periodic")

# Plot the decomposed components
plot(decomposed_c)

# Plotting time series for City C sales
ggplot(data_combined_c, aes(x = date)) +
  geom_line(aes(y = city_c_sales, colour = "City_C_Sales")) +
  geom_line(aes(y = online, colour = "Online Spend")) +
  geom_line(aes(y = city_c_spend, colour = "City_C_Spend")) + 
  geom_line(aes(y = tv, colour = "TV")) +
  labs(title = "City C Sales and Spend Over Time",
       x = "Date", y = "Amount") +
  scale_color_manual("", values = c("City_C_Sales" = "blue", "Online Spend" = "red", "City_C_Spend" = "green", "TV" = "purple"))


# Fit linear model to City C sales
lm_sales_c <- lm(city_c_sales ~ as.numeric(date), data = data_combined_c)
summary(lm_sales_c)

# Plotting the trend line
ggplot(data_combined_c, aes(x = date, y = city_c_sales)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Trend Line for City C Sales", x = "Date", y = "Sales")

# Assuming 'decomposed' is the result of the STL decomposition in R.
seasonal_component <- decomposed_c$time.series[, "seasonal"]
plot(seasonal_component, type = "l")

# Using the `anomalize` package for anomaly detection
library(anomalize)

data_combined_c %>%
  select(date, city_c_sales) %>%
  anomalize(city_c_sales) %>%
  time_decompose(city_c_sales) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

##City D

data_combined_d <- sales %>%
  select(date, city_d_sales = city_d) %>%
  left_join(marketing_spend %>% select(date, city_d_spend = city_d, online, tv), by = "date")

ts_city_d <- ts(data_combined_d$city_d_sales, start = c(2020, 1), frequency = 365)

# Decompose the time series
decomposed_d <- stl(ts_city_d, s.window = "periodic")

# Plot the decomposed components
plot(decomposed_d)

# Plotting time series for City D sales
ggplot(data_combined_d, aes(x = date)) +
  geom_line(aes(y = city_d_sales, colour = "City_d_Sales")) +
  geom_line(aes(y = online, colour = "Online Spend")) +
  geom_line(aes(y = city_d_spend, colour = "City_D_Spend")) + 
  geom_line(aes(y = tv, colour = "TV")) +
  labs(title = "City D Sales and Spend Over Time",
       x = "Date", y = "Amount") +
  scale_color_manual("", values = c("City_D_Sales" = "blue", "Online Spend" = "red", "City_D_Spend" = "green", "TV" = "purple"))


# Fit linear model to City D sales
lm_sales_d <- lm(city_d_sales ~ as.numeric(date), data = data_combined_d)
summary(lm_sales_d)

# Plotting the trend line
ggplot(data_combined_d, aes(x = date, y = city_d_sales)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  labs(title = "Trend Line for City D Sales", x = "Date", y = "Sales")

# Assuming 'decomposed' is the result of the STL decomposition in R.
seasonal_component <- decomposed_d$time.series[, "seasonal"]
plot(seasonal_component, type = "l")

# Using the `anomalize` package for anomaly detection
library(anomalize)

data_combined_d %>%
  select(date, city_d_sales) %>%
  anomalize(city_d_sales) %>%
  time_decompose(city_d_sales) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


