install.packages(dplyr)
install.packages(ggplot2)
install.packages(readr)
library("dplyr")
library("ggplot2")
library("readr")
set.seed(123)
control_data <- data.frame(user_id=1:500,group="Control")
test_data <- data.frame(user_id=501:1000,group="Test")

control_data$conversion <- rbinom(500,1,0.05)
test_data$conversion <- rbinom(500,1,0.08)
ab_test_data <- bind_rows(control_data,test_data)
ab_test_data
ab_test_data <- ab_test_data[sample(nrow(ab_test_data)),]

conversion_rates <- ab_test_data %>% group_by(group) %>% summarise(conversion = mean(conversion))
print(conversion_rates)




strategy_data <- data.frame(
  strategy = rep(c("A","B"),each=500),
  engagement = rbinom(1000,1,c(0.05,0.08))
)
summary(strategy_data)

ggplot(data= strategy_data , aes(x=strategy,y=engagement,fill = strategy))+geom_boxplot()
t.test(engagement ~strategy  , data= strategy_data)


data <- data.frame(
  user_id = 1:1000,
  strategy = rep(c("A", "B"), each = 500),
  clicks = rbinom(1000, 1, c(0.2, 0.3)), # Simulated click data
  conversions = rbinom(1000, 1, c(0.05, 0.08)) # Simulated conversion data
)

data$ctr <- data$clicks / sum(data$clicks)
data$cr <- data$conversions / sum(data$conversions) 
revenue_per_conversion <- 50
data$revenue <- data$conversions * revenue_per_conversion 
summary(data)


set.seed(123)
campaign_performance <- data.frame(Marketing_Strategy = c("Re-Engagement Campaign", "Loyalty Rewards Program", "VIP or Exclusive Membership"),
                                   Customers = c(500, 700, 300), Average_Purchase = c(3.5, 5.2, 8.9), Repeat_Purchase_Rate = c(25, 40, 60) )
print(campaign_performance)
ggplot(campaign_performance, aes(x = Marketing_Strategy)) +
  geom_bar(aes(y = Customers), stat = "identity", fill = "skyblue", alpha = 0.8) + geom_line(aes(y = Average_Purchase * 10, group = 1), color = "red", size = 1.5) + geom_point(aes(y = Average_Purchase * 10, group = 1), color = "red", size = 3) + geom_line(aes(y = Repeat_Purchase_Rate * 5, group = 1), color = "green", size =
                                                                                                                                                                                                                                                                  1.5) +
  geom_point(aes(y = Repeat_Purchase_Rate * 5, group = 1), color = "green", size =
               3) +
  labs(title = "Campaign Performance Metrics by Strategy",
       y = "Count / Metric Value",
       x = "Marketing Strategy" ) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Average Purchase /
10\nRepeat
Purchase Rate / 5")) +
  theme_minimal()


