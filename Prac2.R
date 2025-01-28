install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(dplyr)
library(readr)
library(ggplot2)

barplot(airquality$Ozone, main="Ozone" ,xlab="Ozone quality" , horiz="TRUE")
barplot(airquality$Ozone, main="Ozone" ,xlab="Ozone quality",col="Blue" )

hist(airquality$Temp, main="Temprature",col="red",xlab="Temprature levels")

boxplot(airquality$Wind , main="Windy" ,col="pink",border = "black" , horizontal = "TRUE" )

boxplot(airquality[,0:4],col="blue", main='Box plot for Air Quality parameter')

#USING GGPLOT

data <- data.frame(name=c("A","B","C","D","E") , value=c(3,12,5,18,45) )
data

ggplot(data,aes(x=name,y=value))+geom_bar(stat="identity")

df <- data.frame(gender=factor(rep(c("Average Female Income","Average Male Income"),each=20000)),
                 Average_income = round(c(rnorm(20000,mean=15500,sd=500),c(rnorm(20000,mean=17500,sd=600)))
                 ))

ggplot(df,aes(x=Average_income))+geom_histogram()


ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()

ggplot(data,aes(x=name,y=value,group=1,colour = "red"))+geom_line()+geom_point()


