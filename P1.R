library(dplyr)
library(ggplot2)
airquality
head(airquality)

go <- c(1,2,0,7,5,4,0,4)

# Example dataset
data <- data.frame(
  Name = c("Alice", "Bob", "Charlie", NA, "Eve"),
  Age = c(25, 25, NA, 22, 29),
  Salary = c(50000, NA, 70000, 40000, 60000),
  Department = c("HR", "Finance", NA, "IT", "Marketing")
)

# Display the dataset
print("Original Dataset:")
print(data)

data$Name[is.na(data$Name)] <- "Arie" 
data
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data

data$Salary[is.na(data$Salary)] <-median(data$Salary , na.rm = TRUE)


df <- na.omit(data)
df

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(
    title = "Histogram of Age",
    x = "Age",
    y = "Frequency"
  ) +
  theme_minimal()

