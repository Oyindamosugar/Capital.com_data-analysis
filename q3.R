# Ensure the required libraries are loaded
library(dplyr)

# Filter relevant data for observed and predicted revenues
model_evaluation_data <- user_data %>%
  filter(!is.na(m3_observed_revenue) & !is.na(m3_predicted_revenue))

# Calculate metrics
model_metrics <- model_evaluation_data %>%
  summarise(
    r_squared = cor(m3_observed_revenue, m3_predicted_revenue)^2,
    mae = mean(abs(m3_observed_revenue - m3_predicted_revenue)),
    rmse = sqrt(mean((m3_observed_revenue - m3_predicted_revenue)^2)),
    correlation = cor(m3_observed_revenue, m3_predicted_revenue)
  )

# Print metrics
print(model_metrics)


# Check RStudio version
if ("RStudio.Version" %in% ls()) {
  RStudio.Version()$version
} else {
  "You are not running this in RStudio."
}



# Scatter plot of observed vs predicted revenue
ggplot(model_evaluation_data, aes(x = m3_predicted_revenue, y = m3_observed_revenue)) +
  geom_point(color = "#66B2FF", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Observed vs Predicted Revenue",
    x = "Predicted Revenue ($)",
    y = "Observed Revenue ($)"
  ) +
  theme_minimal()



# Check RStudio version
if ("RStudio.Version" %in% ls()) {
  cat("RStudio Version: ", RStudio.Version()$version, "\n")
} else {
  cat("Not running in RStudio.\n")
}

# Check R version
cat("R Version: ", R.version.string, "\n")
