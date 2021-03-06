#' ---
#' title: "Linear modelling with R"
#' author: "George Savva"
#' date: "23/07/2020"
#' output: 
#'   html_document:
#'     theme: simplex
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)

#' 
#' ## Introduction
#' 
#' Here we will use R to answer some research questions based on the walking speed data.
#' 
#' The data are from a randomised experiment of a rehabilition intervention (compared to control) aimed at improving the walking speed of stroke survivors.
#' 
#' We have recorded the age and sex of each participant, the treatment allocation, the hospital department from which they were recruited and their walking speed.
#' 
#' Our questions are:
#' 
#' * What are the mean and standard deviation of walking speed for treated and untreated participants?
#' * Does the treatment improve walking speed compared to controls?
#' * By how much, and how certain are we of this?
#' * Does age affect walking speed?
#' * Does sex affect walking speed?
#' * Does sex affect the success of the treatment?
#' * Was there any difference in treatment effect by department?
#' 
#' ## Loading and exploring the data
#' 
#' First we'll load the data from the excel sheet, and explore using the summary functions and graphics:
#' 
## ----------------------------------------------------------------------------------------------------
library(readxl)
library(ggplot2)

alldata <- read_excel("walkingspeed.xlsx", sheet="combined", range="A1:F139")
head(alldata)
summary(alldata)
ggplot(alldata, aes(x=age, y=time, shape=treated)) + geom_point() + facet_wrap(~sex)

#' 
#' We'll put the graph onto a log scale:
#' 
## ----------------------------------------------------------------------------------------------------
ggplot(alldata, aes(x=age, y=time, shape=treated)) + 
  geom_point() + 
  facet_wrap(~sex) + 
  scale_y_continuous(trans="log10")

#' 
#' Now we might decide to remove those outlying points, because they are likely to be highly influential in our modelling and they are likely to be wrong.
#' 
#' The subset() function returns the subset of a dataframe that meets the criteria in its second argument.
#' 
## ----------------------------------------------------------------------------------------------------
alldata2 <- subset(alldata, time>1 & time<100)

#' 
#' Now we can plot the dataset without outliers:
#' 
## ----------------------------------------------------------------------------------------------------
ggplot(alldata2, aes(x=age, y=time, shape=treated)) + 
  geom_point() + 
  facet_wrap(~sex) + 
  scale_y_continuous(trans="log10") 

#' 
#' What can you see from the graph?
#' 
#' 
#' ## Estimating our model
#' 
#' Our first question concerned descriptive statistics around walking time amongst men and women.  We saw in the last session that R does not have a good built in way to make nice descriptive tables.  In the last session we saw the 'table1' package but now we can use the new tbl_summary() function from the gtsummary package to get these.
#' 
## ----------------------------------------------------------------------------------------------------
library(gtsummary)

tbl_summary(alldata)


#' 
#' This is really nice!.  Something to note: tbl_summary has detected that 'department' has only four values so has treated it as a categorical variable. This is fine but in general R functions will not do this (as we will see later) so be careful.
#' 
#' We want our data stratified by treatment group, so we can use:
#' 
## ----warning=TRUE------------------------------------------------------------------------------------
tbl_summary(alldata, by=treated)

#' 
#' We won't spend a lot of time on this table, but lets change the statistics displayed, and some of the row names, and drop the rows we don't want to include.  For more customisations see the tbl_summary vignette.
#' 
#' Take some time to study the tbl_summary command below, the tbl_summary help files and vignettes to see how these are specified:
#' 
#' 
## ----------------------------------------------------------------------------------------------------
tbl1 <- tbl_summary(alldata2[,c("treated","time","age", "sex")], 
                    by=treated,
                    label=list(time  ~ "Time (s)", age ~ "Age (yrs)", sex~"Sex"),
                    statistic=list(time~"{mean} ({sd})"))
add_overall(tbl1)

#' 
#' 
#' ## Regression modelling
#' 
#' First we will test the treatment effect on walking speed. We will use a linear regression model for this.  Make sure you understand the commands below:
#' 
## ----------------------------------------------------------------------------------------------------
model1 <- lm( data = alldata2 , time ~ treated)
summary(model1)

#' 
#' How do you interept this model output?  
#' 
#' We could also satisfy ourselves that the linear regression here is the same as an unpaired t-test with the equal variances assumption.  
#' 
## ----------------------------------------------------------------------------------------------------
t.test(data=alldata2, time~ treated, var.equal=TRUE)

#' 
#' So if these outcomes are the same you might wonder why we prefer the linear model function?  We should prefer the linear regression because it offers us a lot more flexibility later on.
#' 
#' ## Checking the validity of the model
#' 
#' We should always check that the assumption underlying a linear model are met.  The assumptions are:
#' 
#' * The residuals (differences between observations and 'predicted' values) are identically normally distributed
#' * The observations are independent of each other
#' 
#' There is no statistical test for these assumptions, we need to use graphical methods to judge visually whether the first is likely to be reasonable, and our knowledge of the experimental design to know whether the second is true.
#' 
#' When you 'plot' a linear model object the plot() function makes graphs to help you check the distribution of residuals from the model:
#' 
## ----------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model1)

#' 
#' The second graph shows a normal qqplot of residuals from the model.  If the times were normally distributed aroud their predicted values this would follow the straight dotted line.  As it is we can see a significant deviation; there are a lot of residuals that are a lot bigger than the model thinks they should be.  
#' 
#' The first and third graphs are less useful for this regression (because there are only two possible 'predicted' values) but they still illustrate that although the residuals are not normally distributed they do at least seem to be similarly distriuted across groups.
#' 
#' ## Transformations and linear models
#' 
#' In the last section we considered two different transformations of the data for the sake of plotting.  We looked at the log transformation and the inverse transformation.  We could try to model log(time) or 1/time as a function of treatment, to see if these meet the assumptions of the regression model better.
#' 
#' We could create a new variable with the transformed values, or we can add the transformation to our model.  First we'll look at the log-transformation:
#' 
## ----------------------------------------------------------------------------------------------------
model2 <- lm( data=alldata2 , log(time) ~ treated)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

#' 
#' 
## ----------------------------------------------------------------------------------------------------
model3 <- lm( data=alldata2 , 1/time ~ treated)
summary(model3)
par(mfrow=c(2,2))
plot(model3)

#' 
#' 
#' The final model looks the best.  It seems that if we model the inverse of time (speed) instead of time itself then the distribution of the residuals is close to normal.
#' 
#' How should we interpret the final model?
#' 
#' ## Presenting the results
#' 
#' An advantage of regression models is that we get an estimate and confidence interval for our effect as well as a p-value.  This is a major disadvantge of analysis or reporting just be placing p-values on plots; by restricting ourselves to this we never get to discuss how much of a difference the treatment makes, and our certainty around that estimate of effect.
#' 
#' The plain text summary of the model gives us all of the information we need, but there are other packages to organise regression model output in a more comprehensible and publication-ready form:
#' 
## ----------------------------------------------------------------------------------------------------
library(sjPlot)
tab_model(model1, model2, model3)

tbl_regression(model1, intercept = TRUE, )

#' 
#' ## A continuous predictor
#' 
#' We can add the effect of age into our model, by changing the model formula in the lm() call:
#' 
## ----------------------------------------------------------------------------------------------------
model4 <- lm(data=alldata2, 1/time ~ treated + age)
tbl_regression(model4)
tab_model(model4)

#' 
#' It looks like the effect of age is 0!  But it is statistically significant, so the low effect size this is probably just a rounding error.  We'll have to change the level of precision being reported in the tabular output (and tweak another couple of options):
#' 
## ----------------------------------------------------------------------------------------------------
tab_model(model4, digits = 4)
tbl_regression(model4,  
               show_single_row="treated",
               intercept=TRUE,
               estimate_fun = function(x) style_ratio(x, digits = 4))

#' 
#' We should continue to check that the model assumptions are still met.
#' 
## ----------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model4)

#' 
#' 
#' ## Plotting the model equations 
#' 
#' To plot the fitted values from our model we need to get convert the model object into a dataframe that can be plotted.
#' 
#' Before I do this I will make a new variable 'speed' corresponding to our transformed outcome.  We can use this instead of doing the transformation in the model equation.
#' 
#' predict() creates predictions from a model object, and can calculate confidence or prediction intervals.  I will use cbind to stick the predictions and the existing data together.
#' 
## ----------------------------------------------------------------------------------------------------

alldata2$speed <- 6 / alldata2$time
head(alldata2)
model5 <- lm(data=alldata2, speed ~ age + treated + sex)
summary(model5)

model5predictions <- predict(model5, interval = "confidence", newdata = alldata2 ) # its helpful to specify 'newdata' in predict.
alldata3 <- cbind(alldata2,model5predictions)

head(alldata3)

ggplot(alldata3, aes(x=age, y=speed, shape=treated)) + 
  geom_point() + 
  facet_wrap(~sex) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=treated), alpha=0.2) + 
  geom_line(aes(y=fit))
  


#' 
#' 
#' ## Testing interactions
#' 
#' Our existing model does not allow the effect of treatment on walking speed to vary with sex.  But we might be interested in whether the effect is the same in men or women (a so called 'interaction' effect).  
#' 
#' Note it is not valid to do this by comparing models estimated in men and women separately.  We should instead estimate a model that includes the interaction between sex and treatment on walking speed.
#' 
## ----------------------------------------------------------------------------------------------------
model6 <- lm(data=alldata2, speed ~ age + treated*sex)
summary(model6)
tab_model(model6)

#' 
#' Look at the plot of model6 compared to model5:
#' 
## ----------------------------------------------------------------------------------------------------
# its helpful to specify 'newdata' in predict.
model6predictions <- predict(model6, interval = "confidence", newdata = alldata2 ) 
head(model6predictions)

alldata3 <- cbind(alldata2,model6predictions)

head(alldata3)

ggplot(alldata3, aes(x=age, y=speed, shape=treated)) + 
  geom_point() + 
  facet_wrap(~sex) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=treated), alpha=0.2) + 
  geom_line(aes(y=fit))

#' 
#' Finally, we can test whether model6 fits the data better than model5.  Anova can be used to compare the fit of two models, and give a p-value for whether the more complex model provides a significantly better fit than the simpler one.
#' 
## ----------------------------------------------------------------------------------------------------
anova(model5, model6)

#' 
#' 
#' 
#' ## A model with a categorial predictor
#' 
#' We might be interested in whether walking speed varies by department.  We could add the department variable to our regression model as follows:
#' 
## ----------------------------------------------------------------------------------------------------
model7 <- lm(data=alldata2, speed ~ age + sex + treated + department)
summary(model7)

#' 
#' But notice that R has not recognised that the 'department' variable should be treated as a categorical variable.  To make sure that 'department' is treated as categorical we should make a new variable in our data frame:
#' 
#' 
#' 
## ----------------------------------------------------------------------------------------------------
alldata2$department_category <- factor(alldata2$department)
summary(alldata2)

#' 
#' Note that in the summary of our dataframe, 'department_factor' is now treated appropriately.  Lets see how the regression output changes:
#' 
## ----------------------------------------------------------------------------------------------------
model7 <- lm(data=alldata2, speed ~ age + sex + treated + department_category)
summary(model7)
tab_model(model7)

#' 
#' R has given us an estimate of the effect of each department, compared to department 1, with a confidence interval and p-value.  It is probably a better question to ask whether the addition of 'department' led to a better model, that is, ask for an omnibus test of effect of 'department'.
#' 
## ----------------------------------------------------------------------------------------------------
anova(model5, model7)

#' 
#' Finally, we might also be interested in comparisons between each pair of levels.  The best way to get this is via the 'emmeans' package as follows:
#' 
## ----------------------------------------------------------------------------------------------------
library(emmeans)

pairs(emmeans(model7, ~department_category))


#' 
#' The output from emmeans includes a 'marginal' estimate for the walking speed in each department, plus an estimate and statistical test for each department compared to every other, with a suitable p-value correction for multiple testing.
#' 
#' ## Exericse:
#' 
#' Can you use lm to test whether walking speed varies with department, and whether the effect of treatment on walking speed varies with department?
#' 
#' 
#' 
#' ## Extensions to other models
#' 
#' Almost every experiment you do can be analysed with this paradigm, that is an outcome variable depending on one or more predictors.  And so data from almost every experiment can be analysed and reported with lm() or a related function.
#' 
#' In practice the modelling functions I find useful for most analyses are:
#' 
#' * lm() - regression models and ANOVA
#' * glm() - generalised linear models (count data and binary outcomes)
#' * lmer() and glmer() - from the lme4 package for mixed effects models (when the assumption of independence is not met, analogous to repeated measures ANOVA)
#' * nlme() for non-linear models
#' 
#' ## Further reading on analysis with R
#' 
#' More detailed linear modelling tutorial.  http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html
#' 
#' Understanding the linear regression diagnostic plots:  http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
#' 
#' Using emmeans to get contrasts and margins https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
#' 
