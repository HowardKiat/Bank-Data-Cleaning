# Load libraries/packages
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(plyr)
library(ggplot2)
library(crayon)
options(scipen = 999)

# Things to explore
# require(stats)
# require(DataExplorer)


# Clean data set (nrow - N/A) # STILL HAVENT FIGURE OUT
# Load Data set (replace with your own file path)
filePath <- "C:/Users/User/OneDrive/Documents/Degree Sem 1/Programming for Data Analysis/Sample Coursework Question Papers, Coursework Model Answers, and Marking Rubrics-20240329/5. credit score classification data.csv"
data <- read.csv(filePath)
View(data)

# Check data set structure
head(data)
tail(data)
str(data)
summary(data)

# Clean column names
data <- janitor::clean_names(data)
View(data)

# Lowercase all column names
names(data) <- tolower(names(data))
colnames(data)

# Check NA (missing value) in data set by column
colSums(is.na(data))
View(data)


# Check and loop each column of the data set
for (col in names(data)) {
  # Check if column contains any white spaces
  if (any(grepl("^\\s*$", data[[col]]))) {
    # Replace white spaces with NA values
    data[data[[col]] == "", col] <- NA
  }
}

# View the data set to check changes
View(data)

# Cleaning Payment Behavior 
# Get unique values of payment_behaviour column
unique_payment_behaviour <- unique(data$payment_behaviour)
print(unique_payment_behaviour) # not fully cleaned

# Clean Age
clean_age <- function(data) {
  # Convert 'Age' column to numeric
  data$age <- as.numeric(data$age)
  
  # Calculate the number of rows before filtering
  rows_before <- nrow(data)
  
  # Remove negative values and ages below 21 and over 100
  data <- data %>%
    filter(age >= 21 & age <= 100)
  
  # Remove any weird symbols from 'Age' column
  data$age <- as.character(data$age)  # Convert back to character for pattern matching
  data <- data %>%
    filter(!grepl("[_-]|^-", age))
  
  # Convert 'Age' column back to numeric
  data$age <- as.numeric(data$age)
  
  # Calculate the number of rows after filtering
  rows_after <- nrow(data)
  
  # Calculate the number of rows removed
  rows_removed <- rows_before - rows_after
  cat("Number of rows removed:", rows_removed, "\n")
  
  # Summary of the cleaned Age column
  summary(data$age)
  
  return(data)
}

# Apply the cleaning function to the data
data <- clean_age(data)

# View data to ensure changes
View(data)

# Get unique ages after cleaning
unique_age <- unique(data$age)
print(unique_age)



# No need to check removed rows


# Clean monthly_inhand_salary
summary(data$monthly_inhand_salary)

# Clean Annual- Income
unique_annual_income <- unique(data$annual_income)
print(unique_annual_income)

# Assuming 'Annual_Income' is the column containing income values
data$annual_income <- gsub("_$", "", data$annual_income)

# Convert 'Annual_Income' to numeric
data$annual_income <- as.numeric(data$annual_income)

View(data)

unique_annual_income <- unique(data$annual_income)
print(unique_annual_income)

summary(data$annual_income)

# Assuming the lowest annual income is 140000 and highest annual income is 
# Assuming we divide each annual income by 12 to get what each person earns per month 




-------------------------------------------------------------------------------



# Summary of the data set with its data types displayed

glimpse(data)



# View data structure



# Convert data set data types

data$ID <- as.character(data$ID)
data$Customer_ID <- as.character(data$Customer_ID)
data$Month <- as.factor(data$Month)
data$Name <- as.character(data$Name)

data$SSN <- as.character(data$SSN)
data$Occupation <- as.factor(data$Occupation)
data$Annual_Income <- as.numeric(data$Annual_Income)
data$Monthly_Inhand_Salary <- as.numeric(data$Monthly_Inhand_Salary)
data$Num_Bank_Accounts <- as.numeric(data$Num_Bank_Accounts)
data$Num_Credit_Card <- as.numeric(data$Num_Credit_Card)
data$Interest_Rate <- as.numeric(data$Interest_Rate)
data$Num_of_Loan <- as.numeric(data$Num_of_Loan)
data$Type_of_Loan <- as.factor(data$Type_of_Loan)
data$Delay_from_due_date <- as.numeric(data$Delay_from_due_date)
data$Num_of_Delayed_Payment <- as.numeric(data$Num_of_Delayed_Payment)
data$Changed_Credit_Limit <- as.numeric(data$Changed_Credit_Limit)
data$Num_Credit_Inquiries <- as.numeric(data$Num_Credit_Inquiries)
data$Credit_Mix <- as.character(data$Credit_Mix)
data$Outstanding_Debt <- as.numeric(data$Outstanding_Debt)
data$Credit_Utilization_Ratio <- as.numeric(data$Credit_Utilization_Ratio)
data$Credit_History_Age <- as.character(data$Credit_History_Age)
data$Payment_of_Min_Amount <- as.character(data$Payment_of_Min_Amount)
data$Total_EMI_per_month <- as.numeric(data$Total_EMI_per_month)
data$Amount_invested_monthly <- as.numeric(data$Amount_invested_monthly)
data$Payment_Behaviour <- as.character(data$Payment_Behaviour)
data$Monthly_Balance <- as.numeric(data$Monthly_Balance)
data$Credit_Score <- as.character(data$Credit_Score)


# Filter findings

# unique(data$x)
# data %>% 
#   select(x1, x2, ...("y")) %>% 
#   filter(x1, %in% c("y1", y2") & x2 < N/A) %>% 

# ID customerid Name  ssn data that can be removes







