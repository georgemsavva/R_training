#' 
#' 
#' --- 
#' title: "Introduction to R for statistics Part 2. Reading data, graphing and linear models"
#' author: "George Savva (QIB)"
#' date: "January 2020"
#' output:
#'   word_document: default
#'   pdf_document: default
#'   html_document: default
#' --- 
#' 
#' In this example we will:
#' 
#' * create a dataset using data stored in three separate Excel sheets  
#' * create new variables based on existing variables
#' * identify and remove some obvious outliers and mistakes
#' * create bar charts, box plots and scatter plots of the data using ggplot2
#' * estimate and diagnose linear models using the lm() function
#' 
#' 
#' # Loading data from Excel, merging, cleaning and coding.
#' 
#' First I made a new project in RStudio, and made sure that the data file I working with was in the root directory of the project.  Then I could just refer to the data by its filename and R would know where to look.
#' 
#' ## Loading data
#' 
#' Our data for this example is in three separate sheets in the same workbook.  Rather than change the data in the workbook we keep the raw data the way it is and do all of our processing in R.
#' 
#' We'll use the read_excel() function in the readxl package to load the data.  Look at the help file for read_excel() to understand what the options do, what other options exist and what their default values are.  Then we are ready to load the data.  Note I always specify sheet and range when using read_excel() even though it may not be necessary, because it is safer and makes my code more readable.
#' 
## ---------------------------------------------------------------------------------------
library(readxl)
treated <- read_excel(path="walkingspeed2.xlsx", 
                      sheet="treated", 
                      range="A1:B68")

control <- read_excel(path="walkingspeed2.xlsx", 
                      sheet="control", 
                      range="A1:B70")

meta <- read_excel(path="walkingspeed2.xlsx", 
                   sheet="meta", 
                   range="A1:D139")

#' 
#' Keep in mind the goal here:  We want to end up with one tidy dataframe that includes all the outcomes and meta data, and that has one row per participant and one column per variable.
#' 
#' ## Appending data using rbind()
#' 
#' So we'll need to rbind the data from the treated and control patients together, then merge this with the meta data.
#' 
#' First we want to 'rbind' the rows from the treated and control patients so that they are in the same data frame.  But becuase the column names are not exactly the same this won't work:
#' 
## ----eval=FALSE-------------------------------------------------------------------------
## # This gives an error
## outcomes <- rbind(treated, control)

#' 
#' To look at the names of a data frame, use the 'names()' function.
#' 
## ---------------------------------------------------------------------------------------
names(treated)
names(control)

#' 
#' The names() function is also how we assign new names to columns of the dataframe.  We could do this:
#' 
## ----eval=FALSE-------------------------------------------------------------------------
## names(treated) <- c("patid", "time")
## names(control) <- c("patid", "time")

#' 
#' But we need to be sure we have the order right.  Here it's probably fine because we only have two columns, but for a more complex dataset it would be easy to get the column order wrong.  It's safer to rename the specific column that we need to rename:
#' 
## ---------------------------------------------------------------------------------------
# How does this command work?
names(control)[names(control)=="walktime"] <- "time"

#' 
#' Make sure you understand how this command worked, and check that it did what you expected!
#' 
## ---------------------------------------------------------------------------------------
names(control)

#' 
#' Now we can rbind the two frames together:
#' 
## ---------------------------------------------------------------------------------------
outcomes <- rbind(treated, control, make.row.names=TRUE)
str(outcomes)

#' 
#' ## Adding a new variable
#' 
#' Now although we have all our data in a single frame, we can no longer tell how each patient was treated!  We should have added a variable to each data frame before rbinding to say what the treatment was.
#' 
#' Lets make a new column called 'treatment' that will have two levels, 'treated' and 'control'.  You add a new variable to a dataframe as follows:
#' 
## ---------------------------------------------------------------------------------------
treated$treated = "Treated"

#' 
#' Now lets add the same column to the control file, and create our outcomes data frame again.
#' 
## ---------------------------------------------------------------------------------------
control$treated = "Control"
outcomes <- rbind(treated, control)


#' 
#' We can check that our new data frame has the treated variable and that it looks correct:
#' 
## ---------------------------------------------------------------------------------------
head(outcomes)
str(outcomes)
table(outcomes$treated)

#' 
#' ## Adding new columns using merge()
#' 
#' Now we need to add columns to our data frame for the patient meta-data.  This is in the 'meta' data frame that we created earlier.  Note that the outcomes and meta data frames are linked by the patient id number, but the columns have different names in each sheet. We could rename the columns of meta as we did above, or we can tell the 'merge' function that we want to merge dataframes on a key that is named differently in each parent set.  The former approach seems better to me:
#' 
## ---------------------------------------------------------------------------------------
names(meta)[1] <- "patid"

# Look at the help for the merge command to find out how this works:
alldata <- merge(outcomes, meta, by="patid", all=TRUE)


#' 
#' 
#' Now look at the 'alldata' data frame to check it has everything we need ready for analysis.  
#' 
#' Next we'll go on to plotting and modelling with this dataset.
#' 
#' ***
#' 
#' # 2. Plotting using ggplot2, transforming and fixing outliers
#' 
#' 
#' Following on from loading and reshaping data, we'll create some plots using ggplot2 to do some descriptive analysis.
#' 
#' First we'll load the libraries that we are going to need.  You might need to install them if they are not already installed on your system:
#' 
#' 
## ---------------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)

#' 
#' 
#' First make a simple scatter plot:
#' 
## ---------------------------------------------------------------------------------------
ggplot(alldata, aes(x=treated, y=time)) + geom_point()

#' 
#' We want to remove any rows with missing data.  There are lots of ways we could do this!
#' 
#' ggplot2 comes with the 'remove_missing' function, that takes away any row with any missing data
#' 
## ----eval=FALSE-------------------------------------------------------------------------
## ggplot(remove_missing(alldata), aes(x=treated, y=time)) + geom_point()

#' 
#' Or we could run 'remove_missing' first to create a new dataset without these rows, then pass that to ggplot.
#' 
## ---------------------------------------------------------------------------------------
alldata2 <- remove_missing(alldata)

#' 
#' Another method is to use the base command 'subset'.  Subset returns the rows of a dataset that meet the condition specified in it's second argument.  Here we will ask for the rows of 'alldata' where 'treated' is not NA, and where time is not NA.
#' 
## ---------------------------------------------------------------------------------------
alldata <- subset(alldata, !is.na(treated) & !is.na(time))
ggplot(alldata, aes(x=treated, y=time)) + geom_point()

#' 
#' The plot above suggests an outlier.  We should use our knowledge of the experiment and our research question to decide what do with this point.  But it is often the case that what seems like an outlier on one scale might not look like an outlier on another.  Switching to a log-scale for the y-axis we now see a second possible problem observation:
#' 
## ---------------------------------------------------------------------------------------
ggplot(alldata, aes(x=treated, y=time)) + geom_point() + scale_y_log10()

#' 
#' Our experiment is a walking time experiment.  It is not feasible that anybody completed this task in less than a second, or took more than a couple of minutes, so we can safely remove these observations by setting their values to NA.
#' 
## ---------------------------------------------------------------------------------------
alldata$time[alldata$time>100] <- NA
alldata$time[alldata$time<0.1] <- NA
ggplot(alldata, aes(x=treated, y=time)) + geom_point() + scale_y_log10()

#' 
#' Even on a log scale some of the data points look unusual.  Another possible transformation is the inverse transformation.  This would turn our 'time' variable into a 'speed' variable.
#' 
#' Our 'time' variable corresponds to the time taken to complete a 3m walk, so to get the speed over the walk we'll divide 3 by the time taken.
#' 
#' We can make a new column in our dataframe, then plot this instead:
#' 
## ---------------------------------------------------------------------------------------

# What does this do?
alldata$speed <- 3 / alldata$time

ggplot(alldata, aes(x=treated, y=speed)) + geom_point()

#' 
#' 
#' ## Embellishing the graph with ggplot2
#' 
#' See the associated powerpoint to understand the development of this plot!
#' 
## ----warning=FALSE----------------------------------------------------------------------

## Setting the scale
ggplot(alldata, aes(x=treated, y=speed)) +
  geom_point() +
  scale_y_log10()
  
## Adding a facet
ggplot(alldata, aes(x=treated, y=speed)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~sex)

## Adding a boxplot, changing the points to beeswarm
ggplot(alldata, aes(x=treated, y=speed, fill=treated)) +
  geom_boxplot() + 
  geom_beeswarm(cex=3) +
  scale_y_log10() + 
  facet_wrap(~sex)


## Pulling it all together.  Changing back to 'speed'
## Adding the p-values, labels, themes, manual colours.
ggplot(alldata, aes(x=treated, y=time, fill=treated)) +
  geom_boxplot() +
  geom_beeswarm(cex=3) +
  facet_wrap(~sex, labeller=labeller(sex=c(M="Male",F="Female"))) +
  scale_fill_manual(values=c("red","green")) +
  scale_y_log10() +
  stat_compare_means(method="t.test", comparisons = list(c("Control","Treated"))) +
  labs(x="Treatment", y="Time (s) ", fill="Treatment") +
  theme_bw() + theme(legend.position = "none")

#' 
#' ## Plotting summary statistics
#' 
#' You might want to plot only summary statistics, or place summaries along with points on the same graph.
#' 
#' You can do this with 'stat_summary()' as follows:
## ----warning=FALSE----------------------------------------------------------------------
ggplot(alldata, aes(x=treated, y=time, fill=treated)) +
  stat_summary( geom="bar", fun.y=mean,width=0.5) +
  stat_summary( geom="errorbar", fun.data=mean_se,width=0.3 ,size=1) + 
  geom_beeswarm(cex=3,alpha=0.5) +
  facet_wrap(~sex, labeller=labeller(sex=c(M="Male",F="Female"))) +
  scale_fill_manual(values=c("red","green")) +
  scale_y_log10() +
  stat_compare_means(method="t.test", comparisons = list(c("Control","Treated"))) +
  labs(x="Treatment", y="Time (s) ", fill="Treatment") +
  theme_bw() + theme(legend.position = "none")

#' 
#' 
#' # Linear Modelling using lm()
#' 
#' Linear modelling is the basis of modern statistical inference.  
#' 
#' Most statistical procedures can be framed as linear models.
#' 
#' The R function that estimates linear models is 'lm'.  To use this function you need to supply a dataset, and a model expressed as a formula.
#' 
#' Suppose we want to test the relationships we were plotting in the graphing exericise above.  We would like to understand the relationship between time taken to complete our walking speed task, the age of the participant, and whether they were treated.
#' 
#' First lets plot the relationship between walking speed and age:
## ---------------------------------------------------------------------------------------
ggplot(data = alldata, aes(x=age, y=speed)) + geom_point()

#' 
#' The command below estimates a simple linear model testing the relationship between walking speed and age.
#' 
## ---------------------------------------------------------------------------------------
lm( data=alldata, formula = speed ~ age )

#' 
#' This has estimated the model but doesn't show us very useful output, but we can save the resulting object which contains all of the information we want.
#' 
## ---------------------------------------------------------------------------------------
model1 <- lm( data=alldata, formula = speed ~ age )

#' 
#' What class is model1?
#' 
#' If we run the 'summary' function on an object of class 'lm', we see a more traditional regression model output
#' 
## ---------------------------------------------------------------------------------------
model1 <- lm( data=alldata, formula = speed ~ age )
summary(model1)

#' 
#' How do you interpret these model coefficients?  Does walking speed depend on age?  By how much?
#' 
#' We can add a similar model to our ggplot to look the fit and get a rough indication of how appropriate it is likely to be:
#' 
## ---------------------------------------------------------------------------------------
ggplot(data = alldata, aes(x=age, y=speed)) + geom_point() + stat_smooth(method="lm")

#' 
#' 
#' ## Regression diagnostics
#' 
#' There are certain assumptions for a linear regression model like this to be valid.  These are:
#' The error variance is normally distributed
#' The error variance is equal for all points
#' There is a linear relationship between our variables to begin with!
#' The errors are independent of each other
#' 
#' The first three conditions can be tested from the data, but the fourth cannot, we need to understand how the data arose to understand this.  If the errors are not independent, (for example if more than one point came from the same patient or biological replicate, or from the same block in block randomised study) then we would need to build a more complex model, but this is beyond the scope of todays work.
#' 
#' Often a visual inspection of the model in this way is enough to satisfy us that conditions 1-3 are met.  We might also be interested in whether any one individual has high leverage over the regression line, which can happen if one has a predictor variable that is a long way from all the others.
#' 
#' Otherwise R can generate diagnostic plots from the model to help us judge whether the diagnostic conditions are met.
#' 
#' Base R will plot these diagnostics if a lm object is passed to the 'plot' function:
#' 
## ---------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model1)

#' 
#' But there is a new package called ggfortify that enables lots of different types of object to be directly plotted using ggplot:
#' 
## ---------------------------------------------------------------------------------------
library(ggfortify)
autoplot(model1)

#' 
#' I have no concerns about my model from a validity point of view.
#' 
#' ## Categorical predictors and interaction terms
#' 
#' Now lets test whether speed depends on treatment.
#' 
#' We can do this using a linear model in exactly the same way as we did above:
#' 
## ---------------------------------------------------------------------------------------
model2 <- lm(data=alldata, speed ~ treated)
summary(model2)

#' 
#' Does treatment improve walking speed?  By how much?
#' 
#' Note:  This is exactly the same calculation as an unpaired t-test.  If you can use lm() there is no need to run the t.test() command.  In my view its easier to use lm because the interface and the output is cleaner.  However there is no easy way to do the Welsh's T-test for unequal variances using lm.
#' 
#' ## Factor variables
#' 
#' We entered a 'character' object into our regression model.  R realised this and treated it as if the different levels represented categories of a categorical variable.  This was the correct thing to do in this case.
#' 
#' If we had a numerical variable reprenting categories we would have to explictly change the type of variable to be 'factor', otherwise the levels would be treated as if they represented quantities.
#' 
#' For example, our 'department' variable was read as a numeric, even though it represents unordered categories.  It would not make sense to fit:
#' 
## ---------------------------------------------------------------------------------------
model2 <- lm(data=alldata, speed ~ department)
summary(model2)

#' 
#' Because this suggests that each increment in the 'department' variable increases walking speed by some amount.  Which is clearly nonsense!
#' 
#' We need to convert department to a factor before we can enter it into the model.  We can use the 'factor()' function for this.
#' 
## ---------------------------------------------------------------------------------------
alldata$departmentF <- factor(alldata$department)
str(alldata)

#' 
#' Now lm() will realise that 'departmentF' is a factor and will treat it appropriately:
#' 
## ---------------------------------------------------------------------------------------
model2 <- lm(data=alldata, speed ~ departmentF)
summary(model2)

#' 
#' Just as the regression on treatment was equivalent to a t-test, note that the F-statistic line at the bottom of the model corresponds to an ANOVA of walking speed across groups.  We can see the full ANOVA table using the anova() command on the lm() object.
#' 
#' There are several functions and packages that will allow you to perform post-hoc pairwise tests between the levels.  These are:
#' 
#' pairwise.t.test()
#' TukeyHSD()
#' emmeans package
#' 
#' You can explore these if it is something you need to do.
#' 
## ---------------------------------------------------------------------------------------
# Note R generates type I ANOVAs.  This is important in multivariate regressions with unbalanced designs.  See the 'car' package for type II and type III ANOVAs.
anova(model2)

#' 
#' ## Multivariable models
#' 
#' We easily extend our lm() to estimate a multivariable model:
#' 
## ---------------------------------------------------------------------------------------
model3 <- lm(data=alldata, speed ~ age + sex + treated + departmentF)
summary(model3)

#' 
#' 
#' Finally, there are several packages available for making outputs more visually appealing.  These include the 'table1' package for descriptive tables and 'sjPlot' package that includes the 'tab_model' function for displaying regression models.
#' 
#' Although its harder to visualise this complex model graphically, we can still produce the diagnostic graphics that we made earlier with ggfortify.  The sjPlot package also includes various functions for visualising linear models.
#' 
#' 
## ---------------------------------------------------------------------------------------
library(table1)
table1(data=alldata, ~ age + sex + departmentF + speed + time | treated)
library(sjPlot)
tab_model(model3)

