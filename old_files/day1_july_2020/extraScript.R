


#############################################################
#  Script to support Introduction to R for data analysis.   #
#  July'19 version                                          #
#  Extra exercises and notes for each worked example        #
#  George Savva (savvag@nbi.ac.uk)                          #
#############################################################



# 1

# This illustrates the use of named arguments for functions.  This is useful because sometimes its ambiguous which argument you are trying to set.
samplesize1 <- power.t.test(n=NULL, delta=1, sd=2, sig.level=0.05, power=0.8)
class(samplesize1)
attributes(samplesize1)
# This prints just the sample size.
samplesize1$n
# This prints the whole object
print(samplesize1)


# 2

random.numbers <- rnorm(100,0,1)
random.numbers[3] <- NA
hist(random.numbers)
mean(random.numbers)
mean(random.numbers, na.rm=TRUE)

library(ggplot2)
ggplot(mapping=aes(x=random.numbers)) + geom_histogram()
ggplot() + geom_histogram(aes(x=random.numbers))

# Suppose we entered a bad value:
b <- c(2,4,66,8,10)

# we can fix it:
b[3] <- 6

# or we could drop it altogether:
b.cleaned <- b[-3]




#5

# The use of 'with'
with(TreeData, table(species.name, health))

# is sometimes better than:
table( TreeData$species.name, TreeData$health )

# we can split a 'with' command across lines to make it more readable.  So long as a command obviously isn't finished, R will keep reading:

with(TreeData,
          table(species.name, health)
)

# we can also do this with ggplot:

ggplot() + geom_histogram(aes(x=random.numbers))

ggplot(mapping=aes(x=random.numbers)) + 
    geom_histogram()


#5 The formula interface to aggregate

# we could have written the aggregate function as:

aggregate( data=TreeData, height ~ species.name , mean)
# or better (naming all the arguments)
aggregate( formula = height ~ species.name ,data=TreeData,  FUN=mean_ci)

#5 Extra descriptives

library(psych)

# Note I can select columns by putting a vector of names as well as numbers
describeBy(TreeData$height, group=TreeData$species.name, mat=TRUE)[,c("group1","mean","sd","se" )]

# This doesn't work, with the + on the next line R doesn't realise that we haven't finished.

ggplot(mapping=aes(x=random.numbers))  
  + geom_histogram()


#6

# We've already seen ggplot, how does it work:

ggplot(data=TreeData, aes(x=species.name, y=height)) +  # Set the data, Aesthetic mappings relate the data to plot elements.
         geom_boxplot() +  # now say what layers you want
         geom_point() +  # why not add some points.  we could control the point appearance with arguments here
         coord_flip()  # and some other elements to the plot.
  
# note the split line command makes this easy to read.
# you can do most things that you would like to do!

# bar chart in ggplot:

ggplot(data=TreeData, aes(y=height, x=species.name)) + 
  stat_summary(fun.data = "mean_cl_normal", geom="bar",fill="red") + 
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",width=0.5,size=1) 
  

#7

t1 <- t.test( height ~ health , data=TreeData)
class(t1)
attributes(t1)
t1$conf.int


#8 

# This line doesn't work, but we don't notice.

b <- c(1,2,3,4,5)
b[3] <- O # O here is the letter not the number, which is wrong.
b

# We may think we've changed b[3] to 0 but we haven't!  For long scripts this can be a problem because R might not stop.