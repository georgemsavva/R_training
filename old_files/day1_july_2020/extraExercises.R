

## Script for additional exercises


# Exercise 1: Power calculations:


help(power.t.test)

power.t.test(n=NULL, delta=1, sd=2, sig.level=0.05, power=0.8) 

# We would need 64 mice per group, so 128 mice for this experiment!

t1 <- power.t.test(n=NULL, delta=1, sd=2, sig.level=0.05, power=0.8) 

class(t1)
attributes(t1)

t1$n

t1 <- power.t.test(n=NULL, delta=1, sd=2, sig.level=0.05, power=0.8) 

# Setting the delta to NULL and the sample size to 10 gives us the smallest detectable difference
power.t.test(n=10, delta=NULL, sd=2, sig.level=0.05, power=0.8) 



## Exercise 2

library(readxl)

speeddata <- read_excel( path= "walkingspeed.xlsx")

# summary statistics

mean(speeddata$time)
sd(speeddata$time)

table(speeddata$sex)

table1::table1(data=speeddata, ~ time + age | sex)
table1::table1(data=speeddata, ~ time + age + sex | treated)



# using aggregate

aggregate( data=speeddata, time ~ treated, mean)
aggregate( data=speeddata, time ~ treated, sd)

boxplot( data=speeddata, time ~ treated)

with(speeddata, hist(time[treated=="Control"]))
with(speeddata, hist(time[treated=="Treated"]))

# There are some big outliers among both groups
# Neither group is normally distributed

# logarithmic or inverse transformations might be OK?


boxplot( data=speeddata, log(time) ~ treated)
boxplot( data=speeddata, (1/time) ~ treated)

# The inverse transformation seems better, but either is fine.

# Making two new variables for our dataframe
speeddata$invtime <- 1/speeddata$time
speeddata$logtime <- log(speeddata$time)

t.test( data=speeddata, invtime ~ treated)
t.test( data=speeddata, logtime ~ treated)

t.test( data=speeddata, invtime ~ sex)

plot(speeddata$age , speeddata$time)
plot(speeddata$age , speeddata$logtime)

lm1 <- lm(speeddata$logtime ~ speeddata$age)
summary(lm1)
abline(lm1)