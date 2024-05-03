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
  # Check for white spaces in each column
  if (any(grepl("^\\s*$", data[[col]]))) {
    # If any white spaces are found, replace them with NA value
    data[[col]] <- ifelse(grepl("^\\s*$", data[[col]]), NA, data[[col]])
  }
}

# View the data set to check changes
View(data)


# Cleaning Payment Behavior 
# Get unique values of payment_behaviour column
unique_payment_behaviour <- unique(data$payment_behaviour)
print(unique_payment_behaviour) # not fully cleaned

# Cleaning Age
unique_age <- unique(data$age)
print(unique_age)

# Convert 'Age' column to numeric (assuming 'Age' is the correct column name)
data$age <- as.numeric(data$age)

# Remove negative values
data <- data[data$age >= 0, ]

# Remove ages that are below 21, over 100, and any weird symbols, including negative values
clean_age <- subset(data, age >= 21 & age <= 100 & !grepl("[_-]|^-", as.character(data$age)) & data$age >= 0)

# Logging the number of rows removed
rows_removed <- nrow(data) - nrow(clean_age)
cat("Number of rows removed:", rows_removed, "\n")

# Summary of the cleaned Age column
summary(clean_age$age)
View(data)









-------------------------------------------------------------------------------



# Summary of the data set with its data types displayed

glimpse(data)



# View data structure

str(data)

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







