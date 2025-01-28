# Load necessary library
library(dplyr)
library(tidyr)

# Create the data
data <- data.frame(
  TransactionID = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
  Item = c(
    "Milk", "Bread", "Milk", "Butter", "Bread", "Butter", "Milk", "Eggs", "Eggs", "Cheese",
    "Milk", "Bread", "Butter", "Jam", "Eggs", "Milk", "Milk", "Bread", "Butter", "Jam",
    "Eggs", "Milk", "Milk", "Butter", "Bread", "Milk", "Butter", "Cheese", "Milk", "Jam"
  )
)

# Step 1: Calculate the occurrences of each item
item_occurrences <- table(data$Item)
item_occurrences

# Step 2: Get the total number of transactions
total_transactions <- length(unique(data$TransactionID))

# Step 3: Calculate the total number of items purchased
total_items_purchased <- nrow(data)
total_items_purchased

# Step 4: Generate pairs of items bought together in each transaction
pair_counts <- data %>%
  group_by(TransactionID) %>%
  summarise(Pairs = combn(Item, 2, FUN = paste, collapse = ", "), .groups = "drop") %>%
  unnest(cols = Pairs) %>%
  count(Pairs)
pair_counts

# Step 5: Split Pairs into Item1 and Item2, and calculate the ratio for each pair
pair_counts <- pair_counts %>%
  separate(Pairs, into = c("Item1", "Item2"), sep = ", ") %>%
  mutate(
    # Occurrence of each item
    Item1_Occurrence = item_occurrences[Item1],    
    Item2_Occurrence = item_occurrences[Item2],    
    # Support for the pair
    Pair_Support = n / total_items_purchased,    
    # Confidence for the pair (A -> B)
    Confidence = n / Item1_Occurrence,            
    # Lift for the pair
    Lift = Confidence / (item_occurrences[Item2] / total_items_purchased)
  )

# Step 6: Display the final results with Pair_Support, Confidence, and Lift
pair_counts

