# Load libraries/packages
library(tidyr)
library(dplyr)
library(janitor)

# Things to explore
# require(stringer)
# require(stats)
# require(DataExplorer)


# Clean data set (nrow - N/A) # STILL HAVENT FIGURE OUT

# Load Data set (replace with your own file path)
filePath <- "C:/Users/User/OneDrive/Documents/Degree Sem 1/Programming for Data Analysis/Sample Coursework Question Papers, Coursework Model Answers, and Marking Rubrics-20240329/5. credit score classification data.csv"
data <- read.csv(filePath)
View(data)

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

# Validate the data set
if (any(is.na(data))) {
  cat("Warning: The dataset has missing values")
}

# Show unique types of loans
unique_loan_type <- unique(data$Type_of_Loan)
cat("Loan Types:\n")
for (loan in unique_loan_type) {
  cat("-", loan, "\n")
}








# Delete Rows and Cols with missing data
data$ID <- janitor::remove_empty(data)
view(data$ID)

data$ID <- janitor::remove_empty_rows(data, which = "rows")
data$ID <- janitor::remove_empty_cols(data, which = "cols")











