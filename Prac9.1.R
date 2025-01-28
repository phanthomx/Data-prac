library("dplyr")
library("ggplot2")
library("readr")
set.seed(123)
transaction_data <- data.frame(
  CustomerID = rep(1:100, each=5),
  TransactionDate = sample(1:90,500,replace = TRUE),
  PurchaseAmount = round(runif(500 ,min=10,max=200),2)
)
transaction_data
current_date <- 90
customer_summary <- transaction_data %>%
  group_by(CustomerID) %>%
  summarize(
    Recency = 90 - max(TransactionDate),    # Days since last purchase
    Frequency = n(),                        # Count of transactions
    Monetary = sum(PurchaseAmount),         # Total purchase amount
    .groups = "drop"
  )

customer_summary

customer_summary <- customer_summary %>%
  mutate(
    RecencyScore = ntile(-Recency, 5),      # Negate Recency so lower values get higher scores
    FrequencyScore = ntile(Frequency, 5),  # Higher frequency gets higher scores
    MonetaryScore = ntile(Monetary, 5),    # Higher monetary gets higher scores
    RFMScore = RecencyScore * 100 + FrequencyScore * 10 + MonetaryScore
  )

customer_summary# Normalize the RFM metrics
rfm_normalized <- customer_summary %>%
  select(Recency, Frequency, Monetary) %>%
  mutate(across(everything(), scale))

# View normalized data
head(rfm_normalized)

# Compute distance matrix
distance_matrix <- dist(rfm_normalized, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, cex = 0.6, hang = -1)

# Cut the dendrogram into k clusters
hc_clusters <- cutree(hc, k = 3)

# Add cluster labels to the customer summary
customer_summary$Cluster <- hc_clusters

# View segmented data
head(customer_summary)


# Descriptive statistics for each cluster
cluster_summary_stats <- customer_summary %>%
  group_by(Cluster) %>%
  summarize(
    AvgRecency = mean(Recency),
    MedianRecency = median(Recency),
    MinRecency = min(Recency),
    MaxRecency = max(Recency),
    
    AvgFrequency = mean(Frequency),
    MedianFrequency = median(Frequency),
    MinFrequency = min(Frequency),
    MaxFrequency = max(Frequency),
    
    AvgMonetary = mean(Monetary),
    MedianMonetary = median(Monetary),
    MinMonetary = min(Monetary),
    MaxMonetary = max(Monetary),
    
    .groups = "drop"
  )

# View summary statistics
print(cluster_summary_stats)







