# Install and load required packages
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(tidyr)) install.packages("tidyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidymodels)) install.packages("tidymodels")
if (!require(embed)) install.packages("embed")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(lubridate)) install.packages("lubridate")
if (!require(car)) install.packages("car")
if (!require(rstatix)) install.packages("rstatix")
if (!require(DescTools)) install.packages("DescTools")
if (!require(nnet)) install.packages("nnet")
if (!require(randomForest)) install.packages("randomForest")
if (!require(rpart)) install.packages("rpart")
if (!require(vip)) install.packages("vip")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(caret)) install.packages("caret")
if (!require(rpart.rules)) install.packages("rpart.rules")

# Load the necessary libraries
library(readr)
library(tidyr)
library(tidyverse)
library(tidymodels)
library(embed)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(lubridate)
library(car)
library(rstatix)
library(DescTools)
library(nnet)
library(randomForest)
library(rpart)
library(vip)
library(rpart.plot)
library(caret)
library(rpart.rules)
library(tidymodels)

# Load the data
filePath <- "C:/Users/User/OneDrive/Documents/Degree Sem 1/Programming for Data Analysis/Sample Coursework Question Papers, Coursework Model Answers, and Marking Rubrics-20240329/5. credit score classification data.csv"
df <- read.csv(filePath)


# lowercase all column names
names(df) <- tolower(names(df))  # Convert column names to lowercase
colnames(df)

# check NAs in data set by column
colSums(is.na(df))

# Display the first few rows of the dataset
cat("Original Dataset:\n")
print(head(df))

# Ensure no missing values in ID and customer_id
if (any(is.na(df$id)) | any(is.na(df$customer_id))) {  # Adjust column names to lowercase
  stop("ID or customer_id columns contain missing values!")
}

# Function to find the most common value in a vector
most_common <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA)
  as.character(names(sort(table(x), decreasing = TRUE)[1]))
}

# Clean "Name"
# Replace missing or "Unknown" Name with highest probability Name within the same customer_id
df <- df %>%
  group_by(customer_id) %>%
  mutate(name = ifelse(is.na(name) | name == "Unknown", most_common(name), name))

df$name = replace(df$name, df$name == "", NA)

# Define a function to calculate the mode
name_mode <- function(x) {
  unique_name <- unique(x)
  frequencies <- tabulate(match(x, unique_name))
  mode_name <- unique_name[which.max(frequencies)]
  return(mode_name)
}

# Replace NA with the mode name within each customer_id group
df <- df %>%
  group_by(customer_id) %>%
  mutate(name = ifelse(is.na(name) | name == "Unknown", name_mode(name), name))

View(df)

# Clean "Age"
# Convert Age to numeric and remove non-numeric characters
df$age <- as.numeric(gsub("[^0-9]", "", df$age))  # Adjust column name to lowercase

# Replace ages below 21 or above 80 with NA
df$age[df$age < 21 | df$age > 80] <- NA  # Adjust column name to lowercase

# Function to replace NA values with logical ages based on preceding or succeeding month's age
replace_na_age <- function(df) {
  for (i in 1:nrow(df)) {
    if (is.na(df$age[i])) {
      # Check for succeeding month's age
      if (i < nrow(df) && df$customer_id[i] == df$customer_id[i + 1] && !is.na(df$age[i + 1])) {
        df$age[i] <- df$age[i + 1]  # Adjust column name to lowercase
      }
      # Check for preceding month's age
      else if (i > 1 && df$customer_id[i] == df$customer_id[i - 1] && !is.na(df$age[i - 1])) {
        df$age[i] <- df$age[i - 1]  # Adjust column name to lowercase
      }
      # If no succeeding or preceding month's age available, set to first non-NA age within same customer ID
      else {
        first_non_na_age <- min(df$age[!is.na(df$age) & df$customer_id == df$customer_id[i]], na.rm = TRUE)
        df$age[i] <- first_non_na_age  # Adjust column name to lowercase
      }
    }
  }
  return(df)
}

# Apply the function to replace NA values in the Age column
df <- replace_na_age(df)

# Replace Inf values with the median age of the data
median_age <- median(df$age, na.rm = TRUE)
df$age[is.infinite(df$age)] <- median_age  # Adjust column name to lowercase

# Clean "occupation"
# Function to replace blank or "_______" Occupation with valid data within the same customer_id
replace_na_occupation <- function(df) {
  for (i in 1:nrow(df)) {
    if (is.na(df$occupation[i]) | df$occupation[i] == "" | df$occupation[i] == "_______") {
      # Check for succeeding month's occupation
      if (i < nrow(df) && df$customer_id[i] == df$customer_id[i + 1] && df$occupation[i + 1] != "" && df$occupation[i + 1] != "_______") {
        df$occupation[i] <- df$occupation[i + 1]
      }
      # Check for preceding month's occupation
      else if (i > 1 && df$customer_id[i] == df$customer_id[i - 1] && df$occupation[i - 1] != "" && df$occupation[i - 1] != "_______") {
        df$occupation[i] <- df$occupation[i - 1]
      }
    }
  }
  # Replace any remaining blanks or "_______" with the most common occupation within the same customer_id
  df <- df %>%
    group_by(customer_id) %>%
    mutate(occupation = ifelse(is.na(occupation) | trimws(occupation) == "" | occupation == "_______",
                               names(sort(table(occupation), decreasing = TRUE)[1]), occupation)) %>%
    ungroup()
  return(df)
}

# Apply the function to replace NA values in the Occupation column
df <- replace_na_occupation(df)

# Clean "SSN"
# Replace missing or incorrect SSN with the highest probability SSN within the same customer_id
df <- df %>%
  group_by(customer_id) %>%
  mutate(ssn = ifelse(is.na(ssn) | ssn == "#F%$D@*&8", most_common(ssn), ssn))  # Adjust column name to lowercase

# Ungroup the data
df <- ungroup(df)


# Clean the Annual_Income column
df$annual_income <- gsub("_", "", df$annual_income)  # Remove underscores
df$annual_income <- as.numeric(df$annual_income)     # Convert to numeric
df$annual_income <- round(df$annual_income, 2)       # Round to 2 decimal places

# Function to replace NA with the mode (most common value) within each group
replace_na_with_mode <- function(x) {
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x[is.na(x)] <- mode_val
  return(x)
}

# Function to ensure consistency within each group
ensure_consistency <- function(x) {
  if (all(is.na(x))) {
    return(x)  # If all values are NA, return as is
  }
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  x <- ifelse(is.na(x), mode_val, x)
  return(rep(mode_val, length(x)))
}

# Handle NA values in Monthly_Inhand_Salary by replacing them with the mode for each customer_id
df <- df %>%
  group_by(customer_id) %>%
  mutate(monthly_inhand_salary = replace_na_with_mode(monthly_inhand_salary)) %>%
  ungroup()

# Round Monthly_Inhand_Salary to 2 decimal places
df$monthly_inhand_salary <- round(df$monthly_inhand_salary, 2)

# Calculate Monthly_Inhand_Salary based on Annual_Income (if needed)
df <- df %>%
  mutate(monthly_inhand_salary = ifelse(is.na(monthly_inhand_salary), round(annual_income / 12, 2), monthly_inhand_salary))

# Clean the Num_Credit_Card column
df <- df %>%
  mutate(num_credit_card = ifelse(num_credit_card > 10 | num_credit_card < 0, NA, num_credit_card))

# Clean the Num_of_Loan column
df$num_of_loan <- gsub("[^0-9]", "", df$num_of_loan)  # Remove non-numeric characters
df$num_of_loan <- as.numeric(df$num_of_loan)         # Convert to numeric
df$num_of_loan <- ifelse(df$num_of_loan < 0, NA, df$num_of_loan)  # Set negative values to NA

# Clean the Num_Bank_Accounts column to remove negative numbers
df$num_bank_accounts <- ifelse(df$num_bank_accounts < 0, NA, df$num_bank_accounts)

# Ensure Num_Bank_Accounts, Num_Credit_Card, Interest_Rate, and Num_of_Loan are consistent for each customer_id
df <- df %>%
  group_by(customer_id) %>%
  mutate(
    num_bank_accounts = ensure_consistency(num_bank_accounts),
    num_credit_card = ensure_consistency(num_credit_card),
    interest_rate = ensure_consistency(interest_rate),
    num_of_loan = ensure_consistency(num_of_loan)
  ) %>%
  ungroup()

# Ensure Type_of_Loan is consistent for each customer_id
# Function to replace NA and "not Specified" with a specified value and ensure consistency
ensure_loan_consistency <- function(x, num_loans) {
  x <- ifelse(x == "not Specified" | is.na(x), "N/A", x)  # Treat "not Specified" and NA as "N/A"
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  return(rep(mode_val, length(x)))
}

df <- df %>%
  group_by(customer_id) %>%
  mutate(type_of_loan = ensure_loan_consistency(type_of_loan, num_of_loan)) %>%
  ungroup()

# Check for any remaining issues in Monthly_Inhand_Salary, Num_Bank_Accounts, Num_Credit_Card, Interest_Rate, Num_of_Loan, and Type_of_Loan
summary(df$monthly_inhand_salary)
summary(df$num_bank_accounts)
summary(df$num_credit_card)
summary(df$interest_rate)
summary(df$num_of_loan)
summary(df$type_of_loan)

# Clean----- Delay_from_due_date

# Convert negative values to positive in the 'Delay_from_due_date' column
df$delay_from_due_date <- abs(df$delay_from_due_date)

# Clean Delay_from_due_date
# Remove non-numeric characters and convert to numeric
df$delay_from_due_date <- as.numeric(gsub("[^0-9]", "", df$delay_from_due_date))

# Replace NA for empty or non-numeric entries
df$delay_from_due_date[is.na(df$delay_from_due_date) | !is.finite(df$delay_from_due_date)] <- NA

# Take absolute values
df$delay_from_due_date <- abs(df$delay_from_due_date)


# Clean Num_of_Delayed_Payment
# 1. Remove underscores
df$num_of_delayed_payment <- gsub("_", "", df$num_of_delayed_payment)

# 2. Convert the column to numeric, introducing NAs for non-numeric entries
df$num_of_delayed_payment <- as.numeric(df$num_of_delayed_payment)

# 3. Convert negative numbers to positive
df$num_of_delayed_payment <- abs(df$num_of_delayed_payment)

# 4. Handle Num_of_Delayed_Payment more than 100 by changing it to the median value based on the Customer ID
df <- df %>%
  group_by(customer_id) %>%
  mutate(num_of_delayed_payment = ifelse(num_of_delayed_payment > 45, median(num_of_delayed_payment[num_of_delayed_payment <= 100], na.rm = TRUE), num_of_delayed_payment)) %>%
  ungroup()

# Function to replace NA with rounded median
replace_na_with_rounded_median <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  rounded_median <- round(median_value)
  x[is.na(x)] <- rounded_median
  return(x)
}

# 5. Apply the function to replace NA values with rounded median based on Customer ID
df <- df %>%
  group_by(customer_id) %>%
  mutate(num_of_delayed_payment = replace_na_with_rounded_median(num_of_delayed_payment)) %>%
  ungroup()


#Clean----- Changed_Credit_Limit

# Replace underscores with NA
df$changed_credit_limit <- ifelse(df$changed_credit_limit == "_", NA, df$changed_credit_limit)

# Convert to numeric
df$changed_credit_limit <- as.numeric(df$changed_credit_limit)

# Handle missing values (e.g., impute with median)
df$changed_credit_limit[is.na(df$changed_credit_limit)] <- median(df$changed_credit_limit, na.rm = TRUE)

# Change number from negative to Positive
df$changed_credit_limit <- abs(df$changed_credit_limit)

# Clean----- Num_Credit_Inquiries
df <- df %>%
  group_by(customer_id) %>%
  mutate(
    is_outlier = num_credit_inquiries > 20,
    most_common_value = as.numeric(names(which.max(table(num_credit_inquiries[!is_outlier & !is.na(num_credit_inquiries)])))),
    num_credit_inquiries = ifelse(is_outlier | is.na(num_credit_inquiries), most_common_value, num_credit_inquiries)
  ) %>%
  ungroup() %>%
  select(-is_outlier, -most_common_value)


#Clean-----Credit_Mix

# Replace underscores in Credit_Mix with the most common value for each customer_id
df$credit_mix <- ifelse(df$credit_mix == "_", NA, df$credit_mix)

df <- df %>%
  group_by(customer_id) %>%
  mutate(credit_mix = ifelse(is.na(credit_mix), most_common(credit_mix), credit_mix)) %>%
  ungroup()


#Clean----- Outstanding_Debt

# Remove underscores from the Outstanding_Debt column
df$outstanding_debt <- gsub("_", "", df$outstanding_debt)


# Function to check if a value has fewer than 10 characters (indicating it might be incomplete)
check_incomplete_value <- function(value) {
  num_digits <- nchar(as.character(value))
  return(num_digits < 10)
}

# Round Credit_Utilization_Ratio to 2 decimal places
df <- df %>%
  mutate(credit_utilization_ratio = round(credit_utilization_ratio, 2))

# Define the Month_Converter function
month_converter <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else {
    num1 <- as.numeric(strsplit(x, ' ')[[1]][1])
    num2 <- as.numeric(strsplit(x, ' ')[[1]][4])
    return (num1 * 12) + num2
  }
}

# Convert credit_history_age column to character format if necessary
if (!is.character(df$credit_history_age)) {
  df$credit_history_age <- as.character(df$credit_history_age)
}

# Apply the month_converter function to the credit_history_age column
df$credit_history_age <- as.numeric(sapply(df$credit_history_age, month_converter))


# Function to impute NA values with the median
impute_na_with_median <- function(data) {
  median_value <- median(data, na.rm = TRUE)
  data[is.na(data)] <- median_value
  return(data)
}

# Function to impute NA values with the median
impute_na_with_median <- function(data) {
  median_value <- median(data, na.rm = TRUE)
  data[is.na(data)] <- median_value
  return(data)
}

# Convert credit_history_age column to numeric if necessary
if (!is.numeric(df$credit_history_age)) {
  df$credit_history_age <- as.numeric(df$credit_history_age)
}

# Replace NA values in the credit_history_age column with median value
df$credit_history_age <- impute_na_with_median(df$credit_history_age)

# Clean "Total_EMI_per_month"
# Function to reassign incorrect numeric values based on group-wise minimum and maximum
Numeric_Wrong_Values_Reassign_Group_Min_Max <- function(df, group_col, numeric_col) {
  # Group-wise minimum and maximum calculation
  min_max_values <- df %>%
    group_by({{group_col}}) %>%
    summarize(min_value = min({{numeric_col}}, na.rm = TRUE),
              max_value = max({{numeric_col}}, na.rm = TRUE)) %>%
    ungroup()
  
  # Correcting incorrect numeric values
  df <- df %>%
    left_join(min_max_values, by = group_col) %>%
    mutate({{numeric_col}} := ifelse({{numeric_col}} < min_value, min_value,
                                     ifelse({{numeric_col}} > max_value, max_value, {{numeric_col}}))) %>%
    select(-min_value, -max_value)
  
  return(df)
}

# Clean "amount_invested_monthly"
# Count the occurrences of each unique value in the amount_invested_monthly column
table(df$amount_invested_monthly, useNA = "ifany")

View(df)

# Clean "amount_invested_monthly"
# Clean empty values in amount_invested_monthly column
df$amount_invested_monthly[df$amount_invested_monthly == ""] <- NA

# Clean "__10000__" format in amount_invested_monthly column
df$amount_invested_monthly <- as.numeric(gsub("_", "", df$amount_invested_monthly))

# Check if there are any values equal to 10000 and replace them with NA
df$amount_invested_monthly[df$amount_invested_monthly == 10000] <- NA 

# Check if there are any NA values or empty strings and replace them with 0
df$amount_invested_monthly[is.na(df$amount_invested_monthly) | df$amount_invested_monthly == ""] <- 0

# Impute median values for NA values
df$amount_invested_monthly[is.na(df$amount_invested_monthly)] <- median(df$amount_invested_monthly, na.rm = TRUE)
View(df)

# Clean "payment_behaviour"

# Define the function to calculate mode
mode_function <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Clean "payment_behaviour" column by replacing special characters with spaces
df$payment_behaviour <- gsub("[!@9#%8]", " ", df$payment_behaviour)

# Replace multiple spaces with a single space
df$payment_behaviour <- gsub("\\s+", " ", df$payment_behaviour)

# Trim leading and trailing spaces
df$payment_behaviour <- trimws(df$payment_behaviour)

# Replace empty strings with NA
df$payment_behaviour[df$payment_behaviour == ""] <- NA

# Calculate the mode of the payment_behaviour column, ignoring NAs
mode_value <- mode_function(df$payment_behaviour)

# Impute NA values with the mode
df$payment_behaviour[is.na(df$payment_behaviour)] <- mode_value


View(df)

object_nan_values_reassign_group_mode <- function(data, group_col, object_col) {
  # Define function to calculate mode
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Function to replace NaN values with mode of each group
  make_group_nan_and_fill_mode <- function(df, groupby, column) {
    mini_maxi <- df %>%
      drop_na({{ column }}) %>%
      group_by({{ groupby }}) %>%
      summarize(min_value = min({{ column }}, na.rm = TRUE), max_value = max({{ column }}, na.rm = TRUE))
    
    df <- df %>%
      left_join(mini_maxi, by = groupby) %>%
      mutate({{ column }} := ifelse({{ column }} < min_value | {{ column }} > max_value, NA, {{ column }})) %>%
      group_by({{ groupby }}) %>%
      mutate({{ column }} := ifelse(is.na({{ column }}), get_mode({{ column }}), {{ column }})) %>%
      ungroup() %>%
      select(-min_value, -max_value)
    
    return(df)
  }
  
  # Impute NA values with median before proceeding
  data[[object_col]][is.na(data[[object_col]])] <- median(data[[object_col]], na.rm = TRUE)
  
  # Before Assigning NaN values
  print(paste("Before Assigning:", object_col, ": have", sum(is.na(data[[object_col]])), "NaN Values"))
  
  # Existing Min, Max Values
  print("Existing Min, Max Values:")
  print(summary(data[[object_col]]))
  
  # Groupby by 'customer_id's Actual min, max Values
  mini_maxi <- data %>%
    drop_na({{ object_col }}) %>%
    group_by({{ group_col }}) %>%
    summarize(min_value = min({{ object_col }}, na.rm = TRUE), max_value = max({{ object_col }}, na.rm = TRUE))
  print("Groupby by customer_id's Actual min, max Values:")
  print(mini_maxi)
  
  # Before Assigning Example object_col
  print("Before Assigning Example Payment_Behaviour:")
  print(head(data[[object_col]]))
  
  # Assigning
  data <- make_group_nan_and_fill_mode(data, group_col, object_col)
  
  # After Assigning NaN values
  print(paste("After Assigning:", object_col, ": have", sum(is.na(data[[object_col]])), "NaN Values"))
  
  # After Assigning Example object_col
  print("After Assigning Example Payment_Behaviour:")
  print(head(data[[object_col]]))
  
  return(data)
}

View(df)

# Clean "monthly_balance"

df$monthly_balance[df$monthly_balance == ""] <- NA

df$monthly_balance[df$monthly_balance == "__-333333333333333333333333333__"] <- NA

# Calculate the median of monthly_balance column
median_balance <- median(df$monthly_balance, na.rm = TRUE)

# Replace NA values with the median
df$monthly_balance <- ifelse(is.na(df$monthly_balance), median_balance, df$monthly_balance)

numeric_wrong_values_reassign_group_min_max <- function(data, group_col, numeric_col) {
  
  
  
  # Function to calculate group min and max
  get_group_min_max <- function(df, groupby, column) {
    cur <- df %>%
      filter(!is.na({{ column }})) %>%
      group_by({{ groupby }}) %>%
      summarize(min_value = min({{ column }}), max_value = max({{ column }}))
    return(cur)
  }
  
  # Function to assign wrong values
  make_group_nan_and_fill_min_max <- function(df, groupby, column) {
    mini_maxi <- get_group_min_max(df, groupby, column)
    
    df <- df %>%
      left_join(mini_maxi, by = groupby) %>%
      mutate({{ column }} := ifelse({{ column }} < min_value | {{ column }} > max_value, NA, {{ column }})) %>%
      group_by({{ groupby }}) %>%
      mutate({{ column }} := ifelse(is.na({{ column }}), median({{ column }}, na.rm = TRUE), {{ column }})) %>%
      ungroup() %>%
      select(-min_value, -max_value)
    
    return(df)
  }
  
  # Before Assigning NaN values
  print(paste("Before Assigning:", numeric_col, ": have", sum(is.na(data[[numeric_col]])), "NaN Values"))
  
  # Assigning
  data <- make_group_nan_and_fill_min_max(data, group_col, numeric_col)
  
  # After Assigning NaN values
  print(paste("After Assigning:", numeric_col, ": have", sum(is.na(data[[numeric_col]])), "NaN Values"))
  
  return(data)
}

# Check Unique Credit scores
unique_credit_score <- unique(df$credit_score)
print(unique_credit_score)

# Display the cleaned dataset
cat("Cleaned Dataset:\n")
print(head(df))

# Save the cleaned dataset back to a CSV file
output_file_path <- "cleaned_credit_data.csv"
write_csv(df, output_file_path)

cat(paste("Cleaned dataset saved to", output_file_path, "\n"))
View(df)


# Data Analysis

# Checking the data type
str(df)

# Analysis 1
# Changing Data Type to Numeric
df$annual_income <- as.numeric(gsub(",", "", df$annual_income))

# Inspect the distribution of annual income
summary(df$annual_income)
hist(df$annual_income, breaks = 30, main = "Histogram of Annual Income", xlab = "Annual Income (RM)", ylab = "Frequency")

# Define lower and upper bounds for filtering outliers
lower_bound <- quantile(df$annual_income, 0.01)  
upper_bound <- quantile(df$annual_income, 0.99)  

# Filter out extreme values
df_filtered <- df %>% filter(annual_income >= lower_bound & annual_income <= upper_bound)

# Set a color palette that works well for categorical variables
color_palette <- c("#66c2a5", "#fc8d62", "#8da0cb")  

# Create the density plot with aesthetics
ggplot(df_filtered, aes(x = annual_income, fill = credit_score)) +
  geom_density(alpha = 0.5, color = "white", adjust = 1.5) +  
  scale_fill_manual(values = color_palette) +  
  labs(
    x = "Annual Income (RM)", 
    y = "Density", 
    fill = "Credit Score",
    title = "Density Distribution of Annual Income by Credit Score (Excluding Outliers)"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    axis.text = element_text(size = 10),  
    legend.position = "bottom"  
  )

# Analysis 2 
# Ensure credit_score is a factor with ordered levels
df$credit_score <- factor(df$credit_score, levels = c('Poor', 'Standard', 'Good'))

# Define Lower and Upper Bounds for Filtering Outliers
lower_bound <- quantile(df$annual_income, 0.01)  # Adjust as needed
upper_bound <- quantile(df$annual_income, 0.99)  # Adjust as needed

# Create the Box Plot and Violin Plot Without Outliers
df_filtered <- df %>% filter(annual_income >= lower_bound & annual_income <= upper_bound)

# Create the box plot and violin plot without outliers
ggplot(df_filtered, aes(x = credit_score, y = annual_income, fill = credit_score)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8), alpha = 0.7) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.8), color = "black", alpha = 0.7) +
  stat_summary(
    fun = median, geom = "text", aes(label = round(..y.., 2)), 
    position = position_dodge(width = 0.8), vjust = -1.5, color = "black"
  ) +
  labs(
    title = "Credit Scores Based on Annual Income (Violin Plot with Summary Stats)",
    x = "Credit Score",
    y = "Annual Income"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Poor" = "#FF6B6B", "Standard" = "#87CEEB", "Good" = "#98FB98")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Group Summary Statistics
group_summary <- df_filtered %>%
  group_by(credit_score) %>%
  summarise(
    count = n(),
    mean = mean(annual_income, na.rm = TRUE),
    sd = sd(annual_income, na.rm = TRUE),
    median = median(annual_income, na.rm = TRUE),
    IQR = IQR(annual_income, na.rm = TRUE)
  )
print(group_summary)

# Perform ANOVA
anova_result <- aov(annual_income ~ credit_score, data = df_filtered)
summary(anova_result)
tuk = TukeyHSD(anova_result)
print(tuk, digits = 20)




# Analysis 3

# Convert 'credit_score' to a factor if it is not already
df$credit_score <- as.factor(df$credit_score)

# Fit the multinomial logistic regression model
multinom_model <- multinom(credit_score ~ annual_income, data = df)

# Summary of the multinomial logistic regression model
print(summary(multinom_model))

# Predict the credit score categories using the multinomial model
predicted_scores <- predict(multinom_model, newdata = df)

# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_scores, Actual = df$credit_score)

# Print the confusion matrix
print(confusion_matrix)

# Calculate model accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Model Accuracy:", accuracy, "\n")

# Decision tree specification

# Changing Data Type to Numeric 
df$annual_income <- as.numeric(gsub(",", "", df$annual_income))
df$credit_utilization_ratio <- as.numeric(df$credit_utilization_ratio)
df$num_of_delayed_payment <- as.numeric(df$num_of_delayed_payment)

# Decision Tree for Classification
set.seed(123)
data_split <- initial_split(df_filtered, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

tree_spec <- decision_tree() %>%
  set_engine("rpart", model = TRUE) %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(credit_score ~ annual_income + credit_utilization_ratio + num_of_delayed_payment, data = train_data)

# Variable Importance Plot
vip(tree_fit$fit)

# Make predictions on the test data using the decision tree model
tree_predictions <- predict(tree_fit, new_data = test_data, type = "class") %>%
  pull(.pred_class)

# Create a confusion matrix for the decision tree model
conf_matrix <- confusionMatrix(tree_predictions, test_data$credit_score)
print(conf_matrix)

# Extract and print the rules of the decision tree
rules <- rpart.rules(tree_fit$fit)
print(rules)

# Plot the decision tree
rpart.plot(
  tree_fit$fit,
  type = 4,
  extra = 106,  # Show the number of observations that fall in each node and the class probability
  under = TRUE,
  cex = 0.7,  # Adjust text size
  fallen.leaves = TRUE,  # Make the leaves fall to the bottom of the plot
  shadow.col = "gray",  # Add shadows for better readability
  box.palette = "auto",  # Automatically choose color for the boxes
  main = "Decision Tree for Credit Score Classification",  # Add a title
  tweak = 1.2  # Adjust the spacing of the plot for better readability
)

