install.packages("recosystem")         # For association rule mining
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(recosystem)  # For collaborative filtering

# Create or load a sample transactional dataframe
# Replace this with your actual dataset
data <- data.frame(
  TransactionID = c(1, 1, 2, 2, 3, 3, 4, 4, 5),
  Item = c("Milk", "Bread", "Milk", "Butter", "Bread", "Butter", "Milk", "Eggs", "Eggs")
)

# Explore the data
head(data)

# Convert the dataframe to a transactional format for association rule mining
transaction_data <- data %>% 
  group_by(TransactionID) %>% 
  summarise(Items = paste(Item, collapse = ", "))

# Calculate support, confidence, and lift using association rules
library(arules)
transaction_list <- split(data$Item, data$TransactionID)
transactions <- as(transaction_list, "transactions")
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.5))

# Analyze rules
inspect(head(sort(rules, by = "lift"), 10))

# Collaborative Filtering Recommender System
# Create a user-item matrix from the dataframe
user_item_matrix <- data %>% 
  mutate(Value = 1) %>% 
  spread(Item, Value, fill = 0)

# Convert to a matrix format
ratings_matrix <- as.matrix(user_item_matrix[,-1])  # Exclude TransactionID
rownames(ratings_matrix) <- user_item_matrix$TransactionID

# Split the data into training and testing sets
set.seed(123)
split <- sample(c(TRUE, FALSE), size = nrow(ratings_matrix), replace = TRUE, prob = c(0.8, 0.2))
train <- ratings_matrix[split, ]
test <- ratings_matrix[!split, ]

# Build a recommender system using the recosystem package
recommender <- Reco()
data_train <- data_memory(user_index = rep(1:nrow(train), ncol(train)),
                          item_index = rep(1:ncol(train), each = nrow(train)),
                          rating = as.vector(train))

recommender$train(data_train, opts = list(dim = 10, lrate = 0.1, nthread = 2, niter = 20))

# Make predictions
predictions <- recommender$predict(data_memory(user_index = 1:nrow(test), item_index = 1:ncol(test)), out_memory())

# Evaluate the performance of the recommender system
library(Metrics)
rmse <- rmse(as.vector(test), as.vector(predictions))
cat("RMSE:", rmse, "\n")
