install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(dplyr)
library(readr)
library(ggplot2)

mat=matrix(c(1:24),nrow=6,ncol = 4,byrow = TRUE)
mat

sam <- sample(c(TRUE,FALSE),nrow(mat),replace = TRUE,prob = c(0.6,0.4))
sam

train=mat[sam,]
test=mat[!sam,]

train
test  

df <- data_frame(col1=c(1:15),col2=letters[1:15], col3 = c(0,1,1,1,0,0,0,0,
                                                           0,1,1,0,1,1,0))
df


tdf <- df%>%dplyr::sample_frac(0.7)
tdf
testd <- dplyr::anti_join(df, tdf, by = 'col1')
