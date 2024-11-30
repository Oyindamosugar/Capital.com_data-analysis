# Filter H2 data
h2_data <- user_data %>%
  filter(account_created_date >= "2023-07-01" & account_created_date <= "2023-12-31")

print (h2_data)

# Summary for slide 1
h2_summary <- h2_data %>%
  summarise(
    total_users = n(),
    attempted_deposits = sum(!is.na(first_deposit_attempt)),
    completed_deposits = sum(!is.na(first_deposit_date)),
    conversion_rate = completed_deposits / total_users
  )

# Bar chart visualization
conversion_data <- tibble(
  Category = c("Attempted Deposits", "Completed Deposits"),
  Count = c(h2_summary$attempted_deposits, h2_summary$completed_deposits)
)

ggplot(conversion_data, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Conversion Rates in H2", x = "Category", y = "Count") +
  theme_minimal()



# Monthly trends
monthly_trends <- h2_data %>%
  mutate(month = floor_date(account_created_date, "month")) %>%
  group_by(month) %>%
  summarise(
    total_users = n(),
    deposit_attempts = sum(!is.na(first_deposit_attempt))
  )

# Line chart visualization
ggplot(monthly_trends, aes(x = month)) +
  geom_line(aes(y = total_users, color = "Total Users")) +
  geom_line(aes(y = deposit_attempts, color = "Deposit Attempts")) +
  labs(title = "Monthly Trends in User Acquisition (H2)", x = "Month", y = "Count") +
  theme_minimal() +
  scale_color_manual(values = c("Total Users" = "blue", "Deposit Attempts" = "green"))


# Performance by location
location_summary <- h2_data %>%
  group_by(legendary_location) %>%
  summarise(
    total_users = n(),
    attempted_deposits = sum(!is.na(first_deposit_attempt)),
    completed_deposits = sum(!is.na(first_deposit_date)),
    conversion_rate = completed_deposits / total_users
  )

# Bar chart visualization
ggplot(location_summary, aes(x = reorder(legendary_location, -total_users), y = total_users, fill = legendary_location)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(conversion_rate, 2)), vjust = -0.5, color = "black") +
  labs(title = "Performance by Legendary Location (H2)", x = "Legendary Location", y = "Total Users") +
  theme_minimal() +
  theme(legend.position = "none")


# Channel effectiveness
channel_summary <- h2_data %>%
  group_by(first_touch_channel) %>%
  summarise(
    total_users = n(),
    attempted_deposits = sum(!is.na(first_deposit_attempt)),
    completed_deposits = sum(!is.na(first_deposit_date)),
    conversion_rate = completed_deposits / total_users
  )

# Bar chart visualization
ggplot(channel_summary, aes(x = reorder(first_touch_channel, -total_users), y = total_users, fill = first_touch_channel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(conversion_rate, 2)), vjust = -0.5, color = "black") +
  labs(title = "Performance by Acquisition Channel (H2)", x = "First Touch Channel", y = "Total Users") +
  theme_minimal() +
  theme(legend.position = "none")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Ensure dates are in the proper format
user_data <- user_data %>%
  mutate(
    account_created_date = ymd(account_created_date),
    first_deposit_date = ymd(first_deposit_date)
  )

# Filter H2 data for account creation and first deposit
h2_user_data <- user_data %>%
  filter(account_created_date >= "2023-07-01" & account_created_date <= "2023-12-31")

h2_deposit_data <- user_data %>%
  filter(first_deposit_date >= "2023-07-01" & first_deposit_date <= "2023-12-31")

# Create a summary dataframe for plotting
acquisition_summary <- tibble(
  Category = c("Account Creation", "First Deposit"),
  Total = c(nrow(h2_user_data), nrow(h2_deposit_data))
)

# Plot the data
ggplot(acquisition_summary, aes(x = Category, y = Total, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Total), vjust = -0.5) +
  labs(
    title = "User Acquisition in H2 (2023)",
    x = "Acquisition Type",
    y = "Total Users"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Ensure dates are in the proper format
user_data <- user_data %>%
  mutate(account_created_date = ymd(account_created_date))

# Filter data for the second half of 2023
h2_user_data <- user_data %>%
  filter(account_created_date >= "2023-07-01" & account_created_date <= "2023-12-31")

# Calculate monthly user acquisition
monthly_acquisition <- h2_user_data %>%
  mutate(month = floor_date(account_created_date, "month")) %>%
  group_by(month) %>%
  summarise(total_users = n(), .groups = "drop")

print(monthly_acquisition)

# Visualize monthly user acquisition
ggplot(monthly_acquisition, aes(x = month, y = total_users)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_users), vjust = -0.5, color = "black") +
  labs(
    title = "Monthly User Acquisition in H2 (2023)",
    x = "Month",
    y = "Total Users Acquired"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)



# Filter data for H2 using first_deposit_date and exclude NA
h2_deposit_data <- user_data %>%
  filter(!is.na(first_deposit_date) & first_deposit_date >= "2023-07-01" & first_deposit_date <= "2023-12-31")
print(h2_deposit_data)

# Calculate monthly user acquisition based on first_deposit_date
monthly_deposit_acquisition <- h2_deposit_data %>%
  mutate(month = floor_date(first_deposit_date, "month")) %>%
  group_by(month) %>%
  summarise(total_users = n(), .groups = "drop")

print (monthly_deposit_acquisition) 

# Visualize monthly user acquisition based on first deposits
ggplot(monthly_deposit_acquisition, aes(x = month, y = total_users)) +
  geom_bar(stat = "identity", fill = "steelblue
  
