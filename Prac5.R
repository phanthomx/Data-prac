library(dplyr) 
library(ggplot2) 
set.seed(123)

customers <- data.frame(cust_id=1:100,
                        total_spend=runif(100,min=100,max=1000),
                      total_orders=sample(1:10,100,replace = TRUE),
                      tenure_months = sample(6:60,100,replace=TRUE))


customers

customers$avg_spend_per_order <- customers$total_spend / customers$total_orders
customers$avg_spend_per_order

lm_model <- lm(avg_spend_per_order ~ total_orders + tenure_months , data =customers)
lm_model

ggplot(customers, aes(x = total_orders, y = avg_spend_per_order)) +
  geom_point(color = "blue") + # Actual data points
  geom_smooth(method = "lm", se = FALSE, color = "red")


customers$high_spender <- ifelse(customers$avg_spend_per_order >180, 1,0)

print(customers$high_spender)
customers$high_spender <- factor(customers$high_spender, levels = c(0, 1), labels = c("Not HighSpender", "High Spender"))

logit_model <- glm(high_spender ~ total_orders + tenure_months, data = customers, family = "binomial")
print(logit_model)
summary(logit_model)


library(ggplot2)

# Create the plot
ggplot(customers, aes(x = total_orders, y = tenure_months, color = high_spender)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  labs(title = "Logistic Regression: Predicting High Spenders",
x = "Total Orders",
y = "Tenure Months", color = "High Spender") +
  scale_color_manual(values = c("blue", "red")) + # Customizing color scale
  theme_minimal()


avg_purchase_value = customers$total_spend /customers$total_orders
purchase_frequency = customers$total_orders / customers$tenure_months
clv = avg_purchase_value * purchase_frequency * customers$tenure_months
head(clv)
ggplot(customers, aes(x = clv)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") + labs(title = "Distribution of CLV",
                                                                       x = "Customer Lifetime Value",
                                                                       y = "Frequency") + theme_minimal()

summary(customers$clv)


current_date

