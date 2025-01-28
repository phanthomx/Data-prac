install.packages(dplyr)
install.packages(ggplot2)
install.packages(readr)
library("dplyr")
library("ggplot2")
library("readr")
set.seed(123)


transaction_data <- data.frame(
  CustomerID = rep(1:100, each=5),
  TransactionDate = sample(seq(as.Date("2024-01-01"),as.Date("2025-01-01"),by="day"),500,replace = TRUE),
  PurchaseAmount = round(runif(500 ,min=10,max=200),2)
)


write.csv(transaction_data, "transaction_data.csv", row.names = FALSE) 
transaction_data <- read.csv("transaction_data.csv") 
head(transaction_data)
transaction_data$TransactionDate <- as.Date(transaction_data$TransactionDate) 
str(transaction_data)

current_date <- as.Date("2025-08-31")
current_date <- as.Date("2024-12-09")

recency_data <- transaction_data %>%
  group_by(CustomerID) %>%
  summarize(Recency = as.numeric(difftime(max(TransactionDate), current_date, units = "days")))

rfm_data <- transaction_data %>% group_by(CustomerID) %>% summarize(
  Recency = as.numeric(difftime(current_date, max(TransactionDate), units = "days")), Frequency = n(), # Count of transactions
  Monetary = sum(PurchaseAmount), # Total amount spent
  .groups = 'drop'
)

recency_data <- transaction_data %>%
  group_by(CustomerID) %>%
  summarize(Recency = as.numeric(difftime(max(TransactionDate), current_date, units = "days")))
frequency_data <- transaction_data %>% group_by(CustomerID) %>%
  summarize(Frequency = n_distinct(TransactionDate))
monetary_data <- transaction_data %>% group_by(CustomerID) %>% summarize(Monetary = sum(PurchaseAmount))


rfm_data <- merge(merge(recency_data, frequency_data, by = "CustomerID", all = TRUE), monetary_data, by = "CustomerID", all = TRUE)
print(rfm_data) 



