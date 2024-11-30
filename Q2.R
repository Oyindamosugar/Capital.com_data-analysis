# Filter data for the public channel in H2 2023
public_channel_data <- user_data %>%
  filter(first_touch_channel == "public" & 
           account_created_date >= "2023-07-01" & 
           account_created_date <= "2023-12-31")

# Summarize performance metrics for H2 2023
public_summary <- public_channel_data %>%
  summarise(
    total_users = n(),
    deposit_attempts = sum(!is.na(first_deposit_attempt)),
    completed_deposits = sum(!is.na(first_deposit_date)),
    conversion_rate_attempt = deposit_attempts / total_users,
    conversion_rate_deposit = completed_deposits / total_users,
    avg_observed_revenue = mean(m3_observed_revenue, na.rm = TRUE),
    avg_predicted_revenue = mean(m3_predicted_revenue, na.rm = TRUE)
  )

print(public_summary)


# Filter spend data for the public channel in H2 2023
public_spend <- spend_data %>%
  filter(legendary_channel == "public" & 
           date >= "2023-07-01" & 
           date <= "2023-12-31")

# Calculate total cost for the public channel in H2 2023
total_public_cost <- sum(public_spend$measure_values, na.rm = TRUE)

# Calculate incremental cost per first deposit
incremental_cost_per_deposit <- total_public_cost / public_summary$completed_deposits
print(paste("Incremental Cost per First Deposit:", incremental_cost_per_deposit))



# Regional analysis for public channel in H2 2023
regional_performance <- public_channel_data %>%
  group_by(legendary_location) %>%
  summarise(
    total_users = n(),
    total_cost = sum(spend_data$measure_values[
      spend_data$legendary_channel == "public" & 
        spend_data$legendary_location == legendary_location & 
        spend_data$date >= "2023-07-01" & 
        spend_data$date <= "2023-12-31"], na.rm = TRUE),
    avg_revenue = mean(m3_observed_revenue, na.rm = TRUE),
    conversion_rate = sum(!is.na(first_deposit_date)) / n()
  )

print(regional_performance)



# Pie chart for total users by region
ggplot(regional_performance, aes(x = "", y = total_users, fill = legendary_location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Total Users by Region (Public Channel, H2 2023)",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")

# Pie chart for conversion rates by region
regional_performance <- regional_performance %>%
  mutate(percentage = conversion_rate * 100)

ggplot(regional_performance, aes(x = "", y = percentage, fill = legendary_location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Conversion Rate by Region (Public Channel, H2 2023)",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")




# Calculate time to first deposit for public channel users in H2 2023
time_to_conversion <- public_channel_data %>%
  mutate(
    time_to_attempt = as.numeric(difftime(first_deposit_attempt, account_created_date, units = "days")),
    time_to_deposit = as.numeric(difftime(first_deposit_date, account_created_date, units = "days"))
  ) %>%
  summarise(
    avg_time_to_attempt = mean(time_to_attempt, na.rm = TRUE),
    avg_time_to_deposit = mean(time_to_deposit, na.rm = TRUE)
  )

print(time_to_conversion)





# Filter public channel data for H2 2023
public_channel_h2 <- public_channel_data %>%
  filter(account_created_date >= "2023-07-01" & account_created_date <= "2023-12-31")

# Calculate time to first deposit attempt and first deposit
time_to_conversion_h2 <- public_channel_h2 %>%
  mutate(
    time_to_attempt = as.numeric(difftime(first_deposit_attempt, account_created_date, units = "days")),
    time_to_deposit = as.numeric(difftime(first_deposit_date, account_created_date, units = "days"))
  ) %>%
  summarise(
    avg_time_to_attempt = mean(time_to_attempt, na.rm = TRUE),
    median_time_to_attempt = median(time_to_attempt, na.rm = TRUE),
    avg_time_to_deposit = mean(time_to_deposit, na.rm = TRUE),
    median_time_to_deposit = median(time_to_deposit, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary table
print(time_to_conversion_h2)



# Verify the month column
time_trend_h2 <- public_channel_h2 %>%
  mutate(
    time_to_attempt = as.numeric(difftime(first_deposit_attempt, account_created_date, units = "days")),
    time_to_deposit = as.numeric(difftime(first_deposit_date, account_created_date, units = "days")),
    month = floor_date(account_created_date, "month") # Ensure month is correctly created
  ) %>%
  group_by(month) %>%
  summarise(
    avg_time_to_attempt = mean(time_to_attempt, na.rm = TRUE),
    avg_time_to_deposit = mean(time_to_deposit, na.rm = TRUE),
    .groups = "drop"
  )

# Print the structure of the resulting data frame
str(time_trend_h2)


# Reshape data into long format
time_trend_long_h2 <- time_trend_h2 %>%
  pivot_longer(
    cols = c(avg_time_to_attempt, avg_time_to_deposit),
    names_to = "Metric",
    values_to = "Days"
  )

# Check the structure of the reshaped data
str(time_trend_long_h2)


# Create area chart
ggplot(time_trend_long_h2, aes(x = month, y = Days, fill = Metric)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Time to Conversion: Area Chart (H2 2023)",
    x = "Month",
    y = "Average Time (Days)"
  ) +
  scale_fill_manual(
    values = c("avg_time_to_attempt" = "skyblue", "avg_time_to_deposit" = "lightgreen"),
    labels = c("Avg Time to Attempt", "Avg Time to Deposit")
  ) +
  theme_minimal()

head(user_data)


# Filter public channel data for H2 2023
public_channel_h2 <- public_channel_data %>%
  filter(account_created_date >= "2023-07-01" & account_created_date <= "2023-12-31")

# LTV analysis for the public channel in H2 2023
ltv_public_h2 <- public_channel_h2 %>%
  summarise(
    avg_observed_revenue = mean(m3_observed_revenue, na.rm = TRUE),
    avg_predicted_revenue = mean(m3_predicted_revenue, na.rm = TRUE),
    total_revenue = sum(m3_observed_revenue, na.rm = TRUE) + sum(m3_predicted_revenue, na.rm = TRUE)
  )

# Print the results
print(ltv_public_h2)


