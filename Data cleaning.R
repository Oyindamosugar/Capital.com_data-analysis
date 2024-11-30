install.packages(c("readxl", "tidyverse", "lubridate", "janitor"))
install.packages("ggplot2")
install.packages("dplyr")


# Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)

# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Specify the correct file path in quotes
file_path <- "/Users/oyindamolaaileru/Downloads/Capital.com -Market Data Analyst.xlsx"

#Read the Excel file and specify the sheet name
user_data <- read_excel(file_path, sheet = "User Level Data")
spend_data <- read_excel(file_path, sheet = "Spend Data")

# View the first few rows
# View the first few rows
head(user_data)


str(user_data$first_deposit_attempt)
str(user_data$first_deposit_date)


library(dplyr)

# Convert character to numeric
user_data <- user_data %>%
  mutate(
    first_deposit_attempt = as.numeric(first_deposit_attempt),
    first_deposit_date = as.numeric(first_deposit_date)
  )

# Check the result after converting to numeric
head(user_data)

# Convert numeric to Date
user_data <- user_data %>%
  mutate(
    first_deposit_attempt = as.Date(first_deposit_attempt, origin = "1899-12-30"),
    first_deposit_date = as.Date(first_deposit_date, origin = "1899-12-30")
  )

# Check the result after converting to Date
head(user_data)


# Check column names
colnames(user_data)
colnames(spend_data)

# Check for missing values
sum(is.na(user_data))
sum(is.na(spend_data))

# Identify rows with missing values in user_data
missing_user_data <- user_data[!complete.cases(user_data), ]
head(missing_user_data)  # View the first few rows with missing values

# Count missing values in each column of user_data
col_missing_user_data <- colSums(is.na(user_data))
col_missing_user_data  # Display the count of missing values for each column


# Count total non-missing (non-NA) values across the entire dataset
sum(!is.na(user_data))

# Get the total number of entries in the dataset
total_entries <- prod(dim(user_data))  # Total rows * total columns
total_entries

missing_count <- sum(is.na(user_data))
missing_count

non_missing_count <- sum(!is.na(user_data))
non_missing_count



library(janitor)

# Clean column names
user_data <- clean_names(user_data)
spend_data <- clean_names(spend_data)

# View missing value summary
summary(user_data)
summary(spend_data)


# Check for any non-numeric values in the m3_observed_revenue column
non_numeric_values <- user_data %>%
  filter(!grepl("^[-+]?[0-9]*\\.?[0-9]+$", m3_observed_revenue))

# View non-numeric values
head(non_numeric_values)

# Replace non-numeric values with 0 (or another value)
user_data <- user_data %>%
  mutate(
    m3_observed_revenue = ifelse(grepl("^[-+]?[0-9]*\\.?[0-9]+$", m3_observed_revenue), 
                                 as.numeric(m3_observed_revenue), 
                                 0),  # Replace with 0
    m3_predicted_revenue = ifelse(grepl("^[-+]?[0-9]*\\.?[0-9]+$", m3_predicted_revenue), 
                                  as.numeric(m3_predicted_revenue), 
                                  0)   # Replace with 0
  )

# Check the result
head(user_data)


# Convert revenue columns to numeric
user_data <- user_data %>%
  mutate(
    m3_observed_revenue = as.numeric(m3_observed_revenue),
    m3_predicted_revenue = as.numeric(m3_predicted_revenue)
  )

# Check for duplicates and remove them
# Check for duplicates and remove them in User Level Data
user_data <- user_data %>%
  distinct()

# Check for duplicates and remove them in Spend Data
spend_data <- spend_data %>%
  distinct()


# Load necessary libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(DiagrammeR)