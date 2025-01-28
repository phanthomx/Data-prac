install.packages("pwr")

library(pwr)   # Preferred method; throws an error if the package isn't installed
pwr.anova.test(k=4,f=.25,sig.level=.05,power=.8)
pwr.t.test(n=12,d=0.75,sig.level=.05,alternative="greater")
pwr.2p.test(n=20,sig.level=0.05,power=0.75)
if(!require(lsr)){install.packages("lsr")}
binom.test(5, 100, 0.5)
Input <- "
Classroom Passed Failed
A 8 2
B 3 7
"

Matrix <- as.matrix(read.table(textConnection(Input), header = TRUE, row.names = 1))
Matrix 
fisher.test(Matrix)

x=c(6.2,6.6,7.1,7.4,7.9,8,8.4,8.3,8.5,8.6,8.8,8.8,9.1,9.2,9.4,9.4,9.7,9.9,10.2,10.4,10.8,11.3,11)
mean(x)
t.test(x-9,alternative="two.sided",conf.level=0.95) 
a=c(418,421,421,422,425,427,431,434,437,439,446,447,448,453,454,463,465) 
b=c(429,430,430,431,36,437,440,441,445,446,447) 
test=t.test(a,b,alternative="two.sided",mu=0,var.equal=F,conf.level=0.95)
test
x1=c(418,421,421,422,425,427,431,434,437,439,446,447,448,453,454,463,465) 
y1=c(429,430,430,431,36,437,440,441,445,446,447) 
test2<-t.test(x1,y1,alternative = "two.sided",mu=0,var.equal = F,conf.level = 0.95) 
test2
