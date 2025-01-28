library(ggplot2)
library(dplyr)
set.seed(123)

emails <- data.frame(recipient_id=1:1000 , group = sample(c("A","B"),size = 1000, replace = TRUE,prob = c(0.5,0.5)))
emails

table(emails$group)

emails$open_rates <- ifelse(emails$group=="A",rnorm(500,mean=0.1,sd=0.02),rnorm(500,mean = 0.12,sd=0.02))

emails <- emails[sample(nrow(emails)),]

mean_os <- emails %>%
  group_by(emails$group) %>%
  summarise(mean_os = mean(open_rates))
mean_os

ggplot(emails,aes(x=group,y=open_rates))+geom_boxplot()




