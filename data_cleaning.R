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










-------------------------------------------------------------------------------


# Check for missing values in each data set column

missing_data <- colSums(is.na(data))

# Print missing data in an organized format

cat("Column Name                      Missing Values\n")
cat("----------------------------------------------\n")
for (col in names(missing_data)) {
  cat(sprintf("%-30s %d\n", col, missing_data[col]))
}






# Summary of the data set with its data types displayed

glimpse(data)



# View data structure

str(data)

# Convert data set data types

data$ID <- as.character(data$ID)
data$Customer_ID <- as.character(data$Customer_ID)
data$Month <- as.factor(data$Month)
data$Name <- as.character(data$Name)
data$Age <- as.numeric(data$Age)
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







