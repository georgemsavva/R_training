

1+2   # should be 3

# Worked example 1

x <- 1 + 2
x

x = 1+2
x

x * 2

class(x)


## Answer to exercise 1

y <- x+3
y # should be 6
x <- 6
y # still 6!  

myname <- "George"
myname2 <- toupper("George")
myname2

# Maths functions

3^2
sqrt(16)
a = log(10)
a
log(10,base=10)
exp(a)

# Worked example 2

a <- c(3,4,5)

a

a+1

a*2

a+x

mean(a)

sum(a)

summary(a)

plot(a)

a[1]

a[3]

a[4]

a[a>3]

a[2] <- 10

a

myvector <- c(10,21,32,NA,54)

class(myvector)

plot(myvector)

mean(myvector)

mean(myvector, na.rm=TRUE)

is.na(myvector)

sum(is.na(myvector))

# Exercise 2

# I can never remember this..

b <- c(3,4,5,4,3,2,1,6)
hist(b)

normsample <- rnorm(100,0,1)
hist(normsample[normsample>0], col="red")


# Worked example 3

# it's difficult to predict what might go wrong if people try this on their own...
install.packages("readxl") # I already have this, not sure what happens with a clean install
library(readxl)

# setwd("u:/work/training") Don't need to so this because we are using projects
# note case senstivity here



help(read_excel)


# Worked example 4

TreeData <- read_excel(path="introstat.xlsx", sheet="P1-TreeSpeciesData",.name_repair="universal")

class(TreeData)

View(TreeData)

head(TreeData)

summary(TreeData)

str(TreeData)

# Worked example 5


TreeData$height

mean(TreeData$height)

boxplot(TreeData$height)

# Could have used the formula interface for this,
# but I introduce that later for other functions.
# Sandro's notes uses the formula interface. 
# Good for people to see more than one way of doing things?

aggregate(TreeData$height, by=list(TreeData$species.name), FUN=mean)

## Here it is with the formula interface
aggregate( data = TreeData, height ~ species.name, FUN=mean)
help(aggregate)

aggregate(TreeData$height, by=list(TreeData$species.name), FUN=summary)

table(TreeData$species.name)

table(TreeData$species.name, TreeData$health)

# Table cannot produce percentages on its own.  
# However we can pass the 'table' output into another function to do this:

# This does the cell percentages.  Do people spot it?
table1 <- table(TreeData$species.name, TreeData$health)
prop.table(table1)

proptab1 <- prop.table(table1, 1)
round(proptab1, digits=2)

proptab1[1,1] 

proptab1[,2] 

proptab1[4,] 

#  Exercise 4

100*round(proptab1,digits=2)
#or
round(100*proptab1,digits=2)


proptab2 <- prop.table(table1, 2)






round(prop.table(table(TreeData$species.name, TreeData$health),1),digits=2) 




# Worked example 5b

install.packages("table1")

table1::table1( data = TreeData, ~ health | species ) 

library(table1)
table1::table1( data = TreeData, ~ health | species.name ) 
# Worked example 6

boxplot(height ~ species.name, data=TreeData)
boxplot(height ~ species.name, data=TreeData, horizontal=TRUE)

par(las=1, mar=c(5,9,4,2))
boxplot(height ~ species.name, data=TreeData, horizontal=TRUE,ylab=NULL)



plot( height ~ dgl, data=TreeData  )

par(mar=c(5.1,4.1,4.1,2))
plot( height ~ dgl, data=TreeData  )




# Worked Example 7

boxplot( height ~ health, data=TreeData , xlab="Height", ylab="Health")

t.test( height ~ health , data=TreeData)

t.test( height ~ health , data=TreeData, var.equal=TRUE)

# Try some logicals (if we get to this.. essential for subsetting but probably too advanced for this tutorial)

1==1
1==2

#Worked example 8

# These are the lines to be added to the script:

# Highlight then press Ctrl+Enter to run them:
TreeData$logHeight <- log(TreeData$height)
plot( logHeight ~ dgl, data = TreeData)


# Worked example 9

lm1 <-  lm( height ~ dgl, data=TreeData  )

summary(lm1)

anova(lm1)

plot( height ~ dgl, data = TreeData)
abline(lm1, col="red")

# Worked example 10

plot(lm1)

predict(lm1)
resid(lm1)

## Note base R boxplots are not very good:
boxplot(height~species.name, 
        data=TreeData)

##  ggplot boxplots are nicer:
install.packages(ggplot2)
library(ggplot2)
# Notice here than one command is spread over several lines.
# If I end a line with '+' R realises that the command isn't 
# finished and starts to read the next line.
# This can make code more readable
ggplot(data=TreeData, aes(x=species.name, y=height)) + 
  geom_boxplot() + 
  geom_point() + 
  coord_flip()




lm2 <-  lm( height ~ species.name, data=TreeData  )
summary(lm2)


# Worked example 11

lm3 <-  lm( height ~ species, data=TreeData  )
summary(lm3)


# This is a numeric variable (a number)
class(TreeData$species)
summary(TreeData$species)

# Make a a factor variable 
TreeData$speciesF <- factor(TreeData$species)
class(TreeData$speciesF)
summary(TreeData$speciesF)


lm4 <- lm( height ~ speciesF, data=TreeData  )
summary(lm4)




