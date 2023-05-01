# Proportion Test , 1-Sample Test , 2-Sample Test , F-Test , Chi-Square Test

#Load the Data
df<-read.csv('E:/Task/CO2.csv')
df

# Two populations P1 and P2
P1 <- df$conc1
P1

P2 <- df$conc2
P2

# Calculate the means for the two populations
#mean 1
m1 <- mean(P1)
m1

#mean 2
m2 <- mean(P2)
m2

#Welch Two Sample t-test

t2 <- t.test(P1, P2, var.equal =FALSE,conf.level = 0.95)
t2

# Create a table of counts for chilled and nonchilled
table <- table(df$Treatment)
table

#chilled
n1 <- table[1]
n1

#nonchilled
n2 <- table[2]
n2

# Proportion test
treatment <- c(rep("A", 42), rep("B", 42))
outcome <- c(rep(1, 42), rep(1, 42))

treatment

outcome

Ch<-prop.table(table(outcome, treatment), margin = 2)
X_sq<-chisq.test(Ch,correct = TRUE)
X_sq$p.value

pro_test <- prop.test(table(outcome, treatment))
pro_test

# 2 sample test
number.of.success<-c(42, 42)
number.of.trials<-c(84,84)
prob.of.success<-NULL

prop.test(number.of.success,number.of.trials,prob.of.success,correct=TRUE,conf.level = 0.95)

Ch<-prop.table(table(outcome, treatment), margin = 2)
X_sq<-chisq.test(Ch,correct = TRUE)
X_sq$p.value

#uptake 1
uptake_1<-df$uptake1
uptake_1

#uptake 2
uptake_2<-df$uptake2
uptake_2

#F-test
var.test(uptake_1,uptake_2,alternative = "greater")
