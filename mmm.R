sales <- read_csv('sales.csv')
marketing_spend <- read_csv('marketing_spend.csv')

library(dplyr)
marketing_spend_revised <- marketing_spend %>% 
  rename(
    city_a_spend = city_a,
    city_b_spend = city_b,
    city_c_spend = city_c,
    city_d_spend = city_d
  )

new_ds <- left_join(sales, marketing_spend_revised, by=c("date"))
new_ds$days <- 1:nrow(new_ds)
new_ds <- new_ds %>%
  mutate(
    month = month(date), # Extract the month from the date
    month_1 = as.integer(month == 1),
    month_2 = as.integer(month == 2),
    month_3 = as.integer(month == 3),
    month_4 = as.integer(month == 4),
    month_5 = as.integer(month == 5),
    month_6 = as.integer(month == 6),
    month_7 = as.integer(month == 7),
    month_8 = as.integer(month == 8),
    month_9 = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11)
    # December is the omitted baseline month
  )

df <- new_ds %>% select(days, date, city_a_spend, tv, online, city_a, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

library(stats) # For the filter function

adstock <- function(spend, decay) {
  # Convert the numeric vector to a time series object
  spend_ts <- ts(spend)
  # Applying the decay filter
  # The stats::filter function can directly work with ts objects
  adstocked_spend <- stats::filter(spend_ts, filter = decay, method = "recursive")
  
  # Convert back to a numeric vector while handling potential NA values
  adstocked_spend <- as.numeric(adstocked_spend)
  if (!is.null(na.action(adstocked_spend))) {
    adstocked_spend[is.na(adstocked_spend)] <- spend[is.na(adstocked_spend)]
  }
  
  return(adstocked_spend)
}

calculate_rss <- function(df, decay1, decay2, decay3) {
  df$adstocked_city_a_spend <- adstock(df$city_a_spend, decay1)
  df$adstocked_online <- adstock(df$tv, decay2)
  df$adstocked_tv <- adstock(df$online, decay3)
  
  
  model <- lm(city_a ~ adstocked_city_a_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)
  rss <- sum(residuals(model)^2)
  
  return(rss)
}

decay_rates <- seq(0.1, 0.9, by = 0.1)
optimal_rss <- Inf
optimal_decays <- c(0, 0)

for (decay1 in decay_rates) {
  for (decay2 in decay_rates) {
    for (decay3 in decay_rates) {
      rss <- calculate_rss(df, decay1, decay2, decay3)
      
      if (rss < optimal_rss) {
        optimal_rss <- rss
        optimal_decays <- c(decay1, decay2, decay3)
      }
    }
  }
}

cat("Optimal decay rates are:", optimal_decays, "with RSS:", optimal_rss)

##Optimal decay rates are: 0.3 0.6 0.7 with RSS: 492628319

df$adstocked_city_a_spend <- adstock(df$city_a_spend, 0.3)
df$adstocked_online <- adstock(df$online, 0.6)
df$adstocked_tv <- adstock(df$tv, 0.7)

final_model <- lm(city_a ~ adstocked_city_a_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)

summary(final_model)

##predict

future_dataset <- filter(marketing_spend, date > as.Date('2023-12-31'))

future_dataset$adstocked_city_a_spend <- adstock(future_dataset$city_a, 0.3)
future_dataset$adstocked_online <- adstock(future_dataset$online, 0.6)
future_dataset$adstocked_tv <- adstock(future_dataset$tv, 0.7)
future_dataset <- future_dataset %>%
  mutate(
    month = month(date), # Extract the month from the date
    month_1 = as.integer(month == 1),
    month_2 = as.integer(month == 2),
    month_3 = as.integer(month == 3),
    month_4 = as.integer(month == 4),
    month_5 = as.integer(month == 5),
    month_6 = as.integer(month == 6),
    month_7 = as.integer(month == 7),
    month_8 = as.integer(month == 8),
    month_9 = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11)
    # December is the omitted baseline month
  )

future_dataset <- future_dataset %>% 
  select(date, adstocked_city_a_spend, adstocked_online, adstocked_tv, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

predictions_city_a <- as.data.frame(predict(final_model, newdata = future_dataset, interval = 'confidence'))
predictions_city_a <- cbind(future_dataset, predictions_city_a)
predictions_city_a

##CITY B

df <- new_ds %>% select(days, date, city_b_spend, tv, online, city_b, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

library(stats) # For the filter function

adstock <- function(spend, decay) {
  # Convert the numeric vector to a time series object
  spend_ts <- ts(spend)
  # Applying the decay filter
  # The stats::filter function can directly work with ts objects
  adstocked_spend <- stats::filter(spend_ts, filter = decay, method = "recursive")
  
  # Convert back to a numeric vector while handling potential NA values
  adstocked_spend <- as.numeric(adstocked_spend)
  if (!is.null(na.action(adstocked_spend))) {
    adstocked_spend[is.na(adstocked_spend)] <- spend[is.na(adstocked_spend)]
  }
  
  return(adstocked_spend)
}

calculate_rss <- function(df, decay1, decay2, decay3) {
  df$adstocked_city_b_spend <- adstock(df$city_b_spend, decay1)
  df$adstocked_online <- adstock(df$tv, decay2)
  df$adstocked_tv <- adstock(df$online, decay3)
  
  
  model <- lm(city_b ~ adstocked_city_b_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)
  rss <- sum(residuals(model)^2)
  
  return(rss)
}

decay_rates <- seq(0.1, 0.9, by = 0.1)
optimal_rss <- Inf
optimal_decays <- c(0, 0)

for (decay1 in decay_rates) {
  for (decay2 in decay_rates) {
    for (decay3 in decay_rates) {
      rss <- calculate_rss(df, decay1, decay2, decay3)
      
      if (rss < optimal_rss) {
        optimal_rss <- rss
        optimal_decays <- c(decay1, decay2, decay3)
      }
    }
  }
}

cat("Optimal decay rates are:", optimal_decays, "with RSS:", optimal_rss)

df$adstocked_city_b_spend <- adstock(df$city_b_spend, 0.4)
df$adstocked_online <- adstock(df$online, 0.8)
df$adstocked_tv <- adstock(df$tv, 0.2)

final_model <- lm(city_b ~ adstocked_city_b_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)

summary(final_model)

##predict

future_dataset <- filter(marketing_spend, date > as.Date('2023-12-31'))

future_dataset$adstocked_city_b_spend <- adstock(future_dataset$city_b, 0.4)
future_dataset$adstocked_online <- adstock(future_dataset$online, 0.8)
future_dataset$adstocked_tv <- adstock(future_dataset$tv, 0.2)
future_dataset <- future_dataset %>%
  mutate(
    month = month(date), # Extract the month from the date
    month_1 = as.integer(month == 1),
    month_2 = as.integer(month == 2),
    month_3 = as.integer(month == 3),
    month_4 = as.integer(month == 4),
    month_5 = as.integer(month == 5),
    month_6 = as.integer(month == 6),
    month_7 = as.integer(month == 7),
    month_8 = as.integer(month == 8),
    month_9 = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11)
    # December is the omitted baseline month
  )

future_dataset <- future_dataset %>% 
  select(date, adstocked_city_b_spend, adstocked_online, adstocked_tv, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

predictions_city_b <- as.data.frame(predict(final_model, newdata = future_dataset, interval = 'confidence'))
predictions_city_b <- cbind(future_dataset, predictions_city_b)
predictions_city_b

##City C

df <- new_ds %>% select(days, date, city_c_spend, tv, online, city_c, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

library(stats) # For the filter function

adstock <- function(spend, decay) {
  # Convert the numeric vector to a time series object
  spend_ts <- ts(spend)
  # Applying the decay filter
  # The stats::filter function can directly work with ts objects
  adstocked_spend <- stats::filter(spend_ts, filter = decay, method = "recursive")
  
  # Convert back to a numeric vector while handling potential NA values
  adstocked_spend <- as.numeric(adstocked_spend)
  if (!is.null(na.action(adstocked_spend))) {
    adstocked_spend[is.na(adstocked_spend)] <- spend[is.na(adstocked_spend)]
  }
  
  return(adstocked_spend)
}

calculate_rss <- function(df, decay1, decay2, decay3) {
  df$adstocked_city_c_spend <- adstock(df$city_c_spend, decay1)
  df$adstocked_online <- adstock(df$tv, decay2)
  df$adstocked_tv <- adstock(df$online, decay3)
  
  
  model <- lm(city_c ~ adstocked_city_c_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)
  rss <- sum(residuals(model)^2)
  
  return(rss)
}

decay_rates <- seq(0.1, 0.9, by = 0.1)
optimal_rss <- Inf
optimal_decays <- c(0, 0)

for (decay1 in decay_rates) {
  for (decay2 in decay_rates) {
    for (decay3 in decay_rates) {
      rss <- calculate_rss(df, decay1, decay2, decay3)
      
      if (rss < optimal_rss) {
        optimal_rss <- rss
        optimal_decays <- c(decay1, decay2, decay3)
      }
    }
  }
}

cat("Optimal decay rates are:", optimal_decays, "with RSS:", optimal_rss)

df$adstocked_city_c_spend <- adstock(df$city_c_spend, 0.4)
df$adstocked_online <- adstock(df$online, 0.2)
df$adstocked_tv <- adstock(df$tv, 0.7)

final_model <- lm(city_c ~ adstocked_city_c_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)

summary(final_model)

##predict

future_dataset <- filter(marketing_spend, date > as.Date('2023-12-31'))

future_dataset$adstocked_city_c_spend <- adstock(future_dataset$city_c, 0.4)
future_dataset$adstocked_online <- adstock(future_dataset$online, 0.2)
future_dataset$adstocked_tv <- adstock(future_dataset$tv, 0.7)
future_dataset <- future_dataset %>%
  mutate(
    month = month(date), # Extract the month from the date
    month_1 = as.integer(month == 1),
    month_2 = as.integer(month == 2),
    month_3 = as.integer(month == 3),
    month_4 = as.integer(month == 4),
    month_5 = as.integer(month == 5),
    month_6 = as.integer(month == 6),
    month_7 = as.integer(month == 7),
    month_8 = as.integer(month == 8),
    month_9 = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11)
    # December is the omitted baseline month
  )



future_dataset <- future_dataset %>% 
  select(date, adstocked_city_c_spend, adstocked_online, adstocked_tv, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

predictions_city_c <- as.data.frame(predict(final_model, newdata = future_dataset, interval = 'confidence'))
predictions_city_c <- cbind(future_dataset, predictions_city_c)
predictions_city_c

##City D

df <- new_ds %>% select(days, date, city_d_spend, tv, online, city_d, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

library(stats) # For the filter function

adstock <- function(spend, decay) {
  # Convert the numeric vector to a time series object
  spend_ts <- ts(spend)
  # Applying the decay filter
  # The stats::filter function can directly work with ts objects
  adstocked_spend <- stats::filter(spend_ts, filter = decay, method = "recursive")
  
  # Convert back to a numeric vector while handling potential NA values
  adstocked_spend <- as.numeric(adstocked_spend)
  if (!is.null(na.action(adstocked_spend))) {
    adstocked_spend[is.na(adstocked_spend)] <- spend[is.na(adstocked_spend)]
  }
  
  return(adstocked_spend)
}

calculate_rss <- function(df, decay1, decay2, decay3) {
  df$adstocked_city_d_spend <- adstock(df$city_d_spend, decay1)
  df$adstocked_online <- adstock(df$tv, decay2)
  df$adstocked_tv <- adstock(df$online, decay3)
  
  
  model <- lm(city_d ~ adstocked_city_d_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)
  rss <- sum(residuals(model)^2)
  
  return(rss)
}

decay_rates <- seq(0.1, 0.9, by = 0.1)
optimal_rss <- Inf
optimal_decays <- c(0, 0)

for (decay1 in decay_rates) {
  for (decay2 in decay_rates) {
    for (decay3 in decay_rates) {
      rss <- calculate_rss(df, decay1, decay2, decay3)
      
      if (rss < optimal_rss) {
        optimal_rss <- rss
        optimal_decays <- c(decay1, decay2, decay3)
      }
    }
  }
}

cat("Optimal decay rates are:", optimal_decays, "with RSS:", optimal_rss)

df$adstocked_city_d_spend <- adstock(df$city_d_spend, 0.4)
df$adstocked_online <- adstock(df$online, 0.1)
df$adstocked_tv <- adstock(df$tv, 0.8)

final_model <- lm(city_d ~ adstocked_city_d_spend + adstocked_online + adstocked_tv + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_7 + month_8 + month_9 + month_10 + month_11, data = df)

summary(final_model)

##predict

future_dataset <- filter(marketing_spend, date > as.Date('2023-12-31'))

future_dataset$adstocked_city_d_spend <- adstock(future_dataset$city_d, 0.4)
future_dataset$adstocked_online <- adstock(future_dataset$online, 0.1)
future_dataset$adstocked_tv <- adstock(future_dataset$tv, 0.8)
future_dataset <- future_dataset %>%
  mutate(
    month = month(date), # Extract the month from the date
    month_1 = as.integer(month == 1),
    month_2 = as.integer(month == 2),
    month_3 = as.integer(month == 3),
    month_4 = as.integer(month == 4),
    month_5 = as.integer(month == 5),
    month_6 = as.integer(month == 6),
    month_7 = as.integer(month == 7),
    month_8 = as.integer(month == 8),
    month_9 = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11)
    # December is the omitted baseline month
  )

future_dataset <- future_dataset %>% 
  select(date, adstocked_city_d_spend, adstocked_online, adstocked_tv, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11)

predictions_city_d <- as.data.frame(predict(final_model, newdata = future_dataset, interval = 'confidence'))
predictions_city_d <- cbind(future_dataset, predictions_city_d)
predictions_city_d

##Consolidate to one dataset for plotting purposes of predictions

predictions_city_a$name <- 'City_A'
predictions_city_b$name <- 'City_B'
predictions_city_c$name <- 'City_C'
predictions_city_d$name <- 'City_D'

combined_df <- rbind( select(predictions_city_a, date, name, fit, lwr, upr), select(predictions_city_b, date, name, fit, lwr, upr), select(predictions_city_c, date, name, fit, lwr, upr), select(predictions_city_d, date, name, fit, lwr, upr))

ggplot(combined_df, aes(x = date, y = fit, group = name, color = name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Predicted Sales over Time by City",
       x = "Date",
       y = "Sales",
       color = "City") +
  theme(legend.position = "bottom")

write_csv(combined_df, 'all_sales_predictions.csv')
