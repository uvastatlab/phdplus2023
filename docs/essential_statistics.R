# PhD Plus: Essential Statistics
# Clay Ford
# Spring 2023

# Load packages we'll use today
# In the first line, we only load two functions from the Hmisc package 
library(Hmisc, include.only = c("smean.cl.normal", "smean.cl.boot"))
library(tidyverse)
library(ggeffects)

# Read in data from GitHub
URL <- "https://github.com/uvastatlab/phdplus2023/raw/main/data/albemarle_homes_2023.rds"
d <- readRDS(url(URL))

# drop homes that are not assigned to a hsdistrict and then drop the unused levels
d <- d %>% 
  filter(hsdistrict != "Unassigned") %>%  
  mutate(hsdistrict = droplevels(hsdistrict),
         msdistrict = droplevels(msdistrict),
         esdistrict = droplevels(esdistrict))


# Counts and proportions --------------------------------------------------

# simple counts of one variable can be obtained with table()
table(d$hsdistrict)

# to calculate proportions, we can pipe into proportions()
table(d$hsdistrict) %>% proportions()

# can also use xtabs()
xtabs( ~ hsdistrict, data = d)
xtabs( ~ hsdistrict, data = d) %>% proportions()

# 2-way tables
# create a cross tabulation using xtabs()
# ~ row_variable + column_variable

# cross tabulation of hsdistrict and fp (fireplace status)
xtabs(~ hsdistrict + fp, data = d)

# Missing values are dropped by default!
# set addNA = TRUE to see missing as a separate category
xtabs(~ hsdistrict + fp, data = d, addNA = TRUE)

# save table
tab <- xtabs(~ hsdistrict + fp, data = d)

# portions of table can be extracted with indexing brackets

# row 1
tab[1,]

# column 1
tab[,1]

# row 1, column 2
tab[1,2]

# adding drop=FALSE preserves table structure
tab[1,,drop = FALSE]
tab[,1, drop = FALSE]
tab[1,2, drop = FALSE]

# Can also use row and column names 
tab["Albemarle", ]
tab[, "1"]
tab["Albemarle", "1"]

# Given high school district, what proportion of homes do and do not have fireplaces? Condition on rows. (row proportions sum to 1)
tab %>% 
  proportions(margin = 1) %>% 
  round(2)

# Given homes with and without fireplaces, what proportions are in each high school district? Condition on columns (column proportions sum to 1)
tab %>% 
  proportions(margin = 2) %>% 
  round(2)

# save table of proportions
tab_p <- tab %>% 
  proportions(margin = 1) %>% 
  round(2)
tab_p

# What's the difference in proportion of homes with a fireplace between
# Albemarle and Monticello? 
tab_p["Albemarle", "1"]
tab_p["Monticello", "1"]


# absolute difference = 0.16 or 16%
tab_p["Albemarle", "1"] - tab_p["Monticello", "1"]
tab_p[1,2] - tab_p[2,2]

# The difference in proportions is 0.16. The proportion of homes with fireplaces
# in Albemarle is 0.16 higher than the proportion of homes with fireplaces in
# Monticello.

# relative difference = 1.25 or 25% 
tab_p["Albemarle", "1"]/tab_p["Monticello", "1"]
tab_p[1, 2]/tab_p[2, 2]

# The ratio of proportions is 1.25. Albemarle has about 25% more homes with
# fireplaces than Monticello.

# 3-way tables (and beyond)

# We can also go beyond two dimensions. This is sometimes called "stratifying"
# on additional variables.

# cross tabulation of hsdistrict and fp (fireplace status) stratified by cooling
# status
tab2 <- xtabs(~ hsdistrict + fp + cooling, data = d)
tab2

# portions of table can be extracted with indexing brackets; need to use 3
# dimensions.

# The third dimension refers to the strata
tab2[,,1]  # cooling = No Central Air
tab2[,,2]  # cooling = Central Air

# or using names
tab2[,,"No Central Air"]
tab2[,,"Central Air"]  

# adding drop = FALSE preserves row/column/strata names
# This is useful when working with 3 or dimensions.
tab2[, ,"Central Air", drop = FALSE]

# Look at Monticello
tab2["Monticello", , , drop = FALSE]

# Look at fp = 1
tab2[, "1", ,drop = FALSE]

# Calculate proportions across rows within each strata;

# within each cooling strata, for each high school district, what proportion of
# homes have and do not have fireplaces
tab2 %>% 
  proportions(margin = c(1, 3)) %>% 
  round(2)

# Within each cooling strata, for homes without and with fireplaces, what
# proportion are in each high school district.
tab2 |>
  proportions(margin = c(2, 3)) |>
  round(2)

# For homes with central air, Monticello has 0.32 without a fireplace versus
# 0.19 for Albemarle. 
tab2 %>% 
  proportions(margin = c(1, 3)) %>% 
  round(2)

# What's the absolute and relative differences?
tab2_p <- tab2 |>
  proportions(margin = c(1, 3)) |>
  round(2)

# Absolute
tab2_p["Monticello", "0", "Central Air"] - 
  tab2_p["Albemarle", "0", "Central Air"]

# Of all homes with central air, the proportion of homes without a fireplace in
# Monticello is 0.13 higher than the proportion of homes with a fireplace in
# Albemarle.


# Relative
tab2_p["Monticello", "0", "Central Air"]/
  tab2_p["Albemarle", "0", "Central Air"]

# Of all homes with central air, Monticello has about 68% more homes without a
# fireplace than Albemarle


# Can also calculate counts and proportions based on a condition.
# Proportion of homes over 2000 finsqft in size
sum(d$finsqft > 2000)
mean(d$finsqft > 2000)

# Proportion of homes with 3 or 4 bedrooms
mean(d$bedroom %in% 3:4)

# These conditions can be included in a call to xtabs()
xtabs(~ (finsqft > 2000) + hsdistrict, data = d) %>% 
  proportions(margin = 2) %>% 
  round(2)

# the tidyverse way for counts and proportions requires more work and is
# documented in an appendix at the end of this script.



# CODE ALONG 1 ------------------------------------------------------------

# (1) Compare the absolute and relative proportions of homes with no central air
# in the Burley and Walton middle school districts.
tp <- xtabs(~ msdistrict + cooling, data = d) %>% 
  proportions(margin = 1) %>% 
  round(3)
tp

# absolute
tp["Burley", "No Central Air"] - tp["Walton", "No Central Air"]

# relative
tp["Burley", "No Central Air"]/tp["Walton", "No Central Air"]

# (2) What proportion of homes are on more than 1 acre of land (lotsize)
mean(d$lotsize > 1)

# (3) What proportion of homes are on more than 1 acre of land (lotsize) within each hsdistrict?
xtabs(~ (lotsize > 1) + hsdistrict, data = d) %>% 
  proportions(margin = 2) %>% 
  round(2)

# Summarizing numeric data ------------------------------------------------

# summary() and hist() make a powerful combination
summary(d$lotsize)
hist(d$lotsize)
# use breaks to increase/decrease number of bins
hist(d$lotsize, breaks = 100)

# standard deviation and IQR
sd(d$lotsize)
IQR(d$lotsize)

# discrete data 
summary(d$bedroom)
hist(d$bedroom)

# better visualized with a bar plot
plot(table(d$bedroom))
barplot(table(d$bedroom))

# ggplot makes it pretty easy to add axis tick mark for 11
ggplot(d) +
  aes(x = bedroom) +
  geom_bar() +
  scale_x_continuous(breaks = 0:12, minor_breaks = FALSE)

# quantiles/percentiles
# given the quantile/percentile what's the value?
quantile(d$totalvalue)
quantile(d$totalvalue, probs = 1:9/10)
quantile(d$totalvalue, probs = 1:19/20)

# Empirical cumulative distribution (ECD)
# inverse of quantile
# given the value, what's the quantile/percentile?
Fn <- ecdf(d$totalvalue)
# what percentile is a $500,000 home in?
Fn(500000) 
# what percentile is a $1,500,000 home in?
Fn(1e6)   

# we can plot the ECD
plot(Fn)

# summaries by group
# aggregate() returns a data frame
# can use formula notation
aggregate(totalvalue ~ hsdistrict, data = d, mean)
aggregate(totalvalue ~ hsdistrict, data = d, median)
aggregate(totalvalue ~ hsdistrict, data = d, IQR)
aggregate(totalvalue ~ hsdistrict, data = d, summary) # kind of messy

# tapply() returns a vector or list
# works with vectors 
tapply(d$totalvalue, d$hsdistrict, mean)
tapply(d$totalvalue, d$hsdistrict, median)
tapply(d$totalvalue, d$hsdistrict, IQR)
tapply(d$totalvalue, d$hsdistrict, summary)  # list
tapply(d$totalvalue, d$hsdistrict, quantile)  # list

# by 2 or more groups
# returns a data frame
aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
# returns a matrix
tapply(d$totalvalue, list(d$hsdistrict, d$cooling), mean)


# A matrix is desirable for reporting
# A data frame is desirable for plotting 

# For example, make data frame for plotting
m_df <- aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
ggplot(m_df) +
  aes(x = hsdistrict, y = totalvalue, shape = cooling) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = 'Median home values by HS District and Central Air status')


# more summaries available in packages
psych::describe(d$totalvalue)
Hmisc::describe(d$totalvalue)

# Correlation
# summarize linear relationship between finsqft and totalvalue
cor(d$finsqft, d$totalvalue, method = "pearson")
cor(d$finsqft, d$totalvalue, method = "spearman")

# Correlation should always be accompanied by a scatter plot

# the smooth trend line can help us assess the nature of the linear relationship
# (if any exists)
ggplot(d) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth()

# correlation between bedrooms and full bathrooms
cor(d$bedroom, d$fullbath) # Why NA?

# Missing data
summary(d$bedroom)
summary(d$fullbath)

# When data is missing can specify use = "complete.obs"
cor(d$bedroom, d$fullbath, use = "complete.obs")

# It's worth noting this is very discrete data
plot(bedroom ~ fullbath, data = d)

# Jittering can help reveal where most of the data is located
ggplot(d) +
  aes(x = fullbath, y = bedroom) +
  geom_jitter(alpha = 1/5) +
  scale_x_continuous(breaks = 0:11, minor_breaks = FALSE) +
  scale_y_continuous(breaks = 0:12, minor_breaks = FALSE)

# Correlation matrix for more than 2 variables
d %>% 
  select(totalvalue, finsqft, lotsize) %>% 
  cor()

# quick way to visualize all pairwise scatterplots
d %>% 
  select(totalvalue, finsqft, lotsize) %>% 
  pairs()

# Log transformations
# Take positive, skewed data and hopefully make it more symmetric.
# values clumped together get spread apart,
# values far away are moved closer to the rest of the data

hist(d$totalvalue)
# log transformed histogram
hist(log(d$totalvalue))

# to undo a log transformation we take the "anti-log", or exponentiate using the
# exp() function.
x <- 1200000
logx <- log(x)
logx
exp(logx)

# mean of untransformed data
mean(d$totalvalue)
# mean of log transformed data
mean(log(d$totalvalue))
# mean of log transformed data returned to original scale,
# sometimes called "geometric mean"
exp(mean(log(d$totalvalue)))


# CODE ALONG 2 ------------------------------------------------------------

# (1) summarize finsqft numerically and visually
summary(d$finsqft)
hist(d$finsqft)

# (2) What is the 90th percentile of finsqft? In what percentile is 1500
# finsqft?
quantile(d$finsqft, probs = 0.90)
Fsq <- ecdf(d$finsqft)
Fsq(1500)

# (3) what is the correlation between finsqft and totalrooms (presumably they're
# correlated)? Compare the correlation to a scatter plot between the two
# variables.
cor(d$finsqft, d$totalrooms, use = "complete.obs")
cor(d$finsqft, d$totalrooms, use = "complete.obs", method = "spearman")

ggplot(d) +
  aes(x = totalrooms, y = finsqft) +
  geom_point() +
  geom_smooth() +
  xlim(0, 20)

# (4) summarize the log-transformed improvements value. What do we notice?
summary(d$improvementsvalue)
hist(d$improvementsvalue, breaks = 200)
summary(log(d$improvementsvalue))
summary(log(d$improvementsvalue[d$improvementsvalue > 0]))

d %>% 
  filter(improvementsvalue > 0) %>% 
  summarise(m = exp(mean(log(improvementsvalue))))

# Uncertainty -------------------------------------------------------------

# Uncertainty about a mean
# randomly sample 50 total home values

# Recall: we have all the data! This is for illustration purposes, as if we only
# had a random sample of homes.
samp <- sample(d$totalvalue, 50)
mean(samp)
sd(samp)/sqrt(50)  # standard error of the mean

# repeat 10,000 times
means <- replicate(n = 10000, expr = {
  samp <- sample(d$totalvalue, 50)
  mean(samp)
})
hist(means)
sd(means) # standard error based on 10,000 means

# add and subtract 2 standard errors from mean to form approximate 95%
# confidence interval
SE_m <- sd(means)
mean(samp) + c(-1, 1)*2*SE_m

# The above was for educational purposes;
# In practice we would do something like this using t.test()
t.test(samp)
t.test(samp)$conf.int

# The Hmisc package provides the smean.cl.normal() function that returns the
# means and the lower/upper limits.
Hmisc::smean.cl.normal(samp)

# Uncertainty about a proportion
# random sample of 50 homes' fireplace status
samp2 <- sample(d$fp, 50)
mean(samp2, na.rm = TRUE)

# repeat 10,000 times
props <- replicate(n = 10000, expr = {
  samp2 <- sample(d$fp, 50)
  mean(samp2, na.rm = TRUE)
})
hist(props)
sd(props) # standard error based on 10,000 means

# add and subtract 2 standard errors from mean to form approximate 95%
# confidence interval
SE_p <- sd(props)
mean(samp2, na.rm = TRUE) + c(-1, 1)*2*SE_p

# The above was for educational purposes;
# In practice we would do something like this using prop.test()
prop.test(x = sum(samp2, na.rm = TRUE), n = 50)
prop.test(x = sum(samp2, na.rm = TRUE), n = 50, correct = FALSE)

# 95% Confidence intervals
# The process of calculating a confidence interval works 95% of the time;
# The "95%" is based on tradition. Can do 89%, 78%, etc.

# sample 30 values from a Normal distribution with mean 10 and sd 2
z <- rnorm(30, mean = 10, sd = 2)
# calculate confidence interval on estimated mean
tout <- t.test(z)
# is true value of 10 contained in confidence interval?
tout$conf.int[1] < 10 & tout$conf.int[2] > 10

# repeat 1000 times
ci_test <- replicate(n = 1000, expr = {
  z <- rnorm(30, mean = 10, sd = 2)
  tout <- t.test(z)
  tout$conf.int[1] < 10 & tout$conf.int[2] > 10
})

# proportion of times CI contains 10
mean(ci_test)

# Try it with very skewed homes total values
true_mean <- mean(d$totalvalue)
true_mean
samp <- sample(d$totalvalue, 50)
tout <- t.test(samp)
tout$conf.int[1] < true_mean & tout$conf.int[2] > true_mean

ci_test <- replicate(n = 1000, expr = {
  samp <- sample(d$totalvalue, 50, replace = TRUE)
  tout <- t.test(samp)
  tout$conf.int[1] < true_mean & tout$conf.int[2] > true_mean
})
mean(ci_test)
# coverage is actually not 95%!

# Try it with log-transformed home values
true_log_mean <- mean(log(d$totalvalue))
ci_test_log <- replicate(n = 1000, expr = {
  samp <- sample(d$totalvalue, 50, replace = TRUE)
  tout <- t.test(log(samp))
  tout$conf.int[1] < true_log_mean & tout$conf.int[2] > true_log_mean
})
mean(ci_test_log)

# In practice we only get one sample so we can't do what we did above.

# However we can do something called "bootstrapping" that treats the sample like
# the population.

# take one sample
tv <- sample(d$totalvalue, 50)

# now resample from sample with replacement
boot_mean <- replicate(n = 1000, expr = {
  tv_samp <- sample(tv, replace = TRUE)
  mean(tv_samp)
})
# get 95% CI as 2.5 and 97.5 percentiles
quantile(boot_mean, probs = c(0.025, 0.975))

# or use smean.cl.boot() function from the Hmisc package
Hmisc::smean.cl.boot(tv)

# Bootstrapping works well for creating confidence intervals for medians
boot_median <- replicate(n = 1000, expr = {
  tv_samp <- sample(tv, replace = TRUE)
  median(tv_samp)
})
# get 95% CI as 2.5 and 97.5 percentiles
quantile(boot_median, probs = c(0.025, 0.975))

# CODE ALONG 3 ------------------------------------------------------------

# The following code randomly samples 120 homes in the Western Albemarle High
# school district. Use this data frame to answer the following questions.
set.seed(123)
wa_samp <- d %>% 
  filter(hsdistrict == "Western Albemarle") %>% 
  sample_n(120)

# (1) Calculate 95% confidence interval on mean finsqft
t.test(wa_samp$finsqft)
smean.cl.normal(wa_samp$finsqft)

# (2) Does the 95% confidence interval capture the true value?
d %>% 
  filter(hsdistrict == "Western Albemarle") %>% 
  summarize(mean(finsqft))

# (3) Use a bootstrap to calculate a 95% confidence interval for the median age
# of homes in our sample. Use 1000 bootstraps.

bootout <- replicate(n = 1000, expr = {
  sage <- sample(wa_samp$age, replace = TRUE)
  median(sage)
})
quantile(bootout, probs = c(0.025, 0.975))

# (4) Does the 95% bootstrap CI capture the true median age value?
d %>% 
  filter(hsdistrict == "Western Albemarle") %>% 
  summarize(median(age))


# Hypothesis Testing ------------------------------------------------------

# Sometimes researchers do hypothesis testing on "all the data" based on the
# assumption the data collected is just one sample of a super-population of
# possibilities.

# Is condition of home associated with high school district?
# Can use a Chi-square test of association to investigate
ct <- xtabs(~ condition + hsdistrict, data = d)
chisq.test(ct)

# As small p-value provides evidence against the null of no association, but
# does not tell us anything about how they may be associated.

# Residuals tell us which cell counts are bigger/smaller than expected. Values
# greater than 2.5 or 3 in absolute value are of interest.
ctest <- chisq.test(ct)
ctest$residuals

# A mosaic plot can visualize residuals
mosaicplot(ct, shade = TRUE)

# compare to proportions
ct %>% proportions(margin = 2)

# To compare two proportions we can use the prop.test() function.

# Is the difference in proportions of Excellent homes in Albemarle and Western
# Albemarle "significant"? (Many statisticians hate that phrase)
xtabs(~ condition + hsdistrict, data = d) %>% 
  addmargins()
prop.test(x = c(189, 239), n = c(12714, 9421))


# Does totalvalue differ between homes with and without fireplaces?
# Could investigate with a t-test
t.test(totalvalue ~ fp, data = d)

# A small p-value provides evidence against the null of no difference, but
# does not tell us about the difference. Instead look at CI on difference.

# Maybe we should look at distribution of totalvalue by fp
ggplot(filter(d, !is.na(fp))) +
  aes(x = totalvalue, fill = factor(fp)) +
  geom_density(alpha = 1/3)

# Ideally we would like to see the distributions of each group looking roughly
# symmetric. One option is a log transformation
ggplot(filter(d, !is.na(fp))) +
  aes(x = log(totalvalue), fill = factor(fp)) +
  geom_density(alpha = 1/3)

tout <- t.test(log(totalvalue) ~ fp, data = d)
tout

# undo the log transformation to see the expected mean values, ie the geometric
# means.
exp(tout$estimate)

# the confidence limits for the difference between means cannot be transformed
# back to the original scale
tout$conf.int

# Instead, if we exponentiate, we get a 95% CI for the ratio of the geometric
# means. The geometric mean of homes without fireplaces is about 44% - 43% lower
# than homes with fireplaces. (ie, $277,741 is about 43-44% lower than $491,747)
exp(tout$conf.int)
1 - exp(tout$conf.int) # about 44% - 43% lower

# We can calculate the ratio of the geometric means. It's about 0.56. The 95% CI
# is about [0.55, 0.57]
exp(12.53444)/exp(13.10572)

# Another approach is a non-parametric test that makes no assumptions about the
# distributions. This compares the ranks of the data instead of the values
# themselves. This takes a second!
wilcox.test(totalvalue ~ fp, data = d, conf.int = TRUE)

# The "difference in location" estimates the median of the difference between a
# sample from homes with fireplaces and a sample from homes without fireplaces.



# CODE ALONG 4 ------------------------------------------------------------

# observe the following proportions
xtabs(~ hsdistrict + fp, data = d) %>% 
  proportions(margin = 1)

# (1) Assuming no difference in proportion of fireplaces in Albemarle and
# Western Albemarle high school districts, what is probability we would get data
# like this or more extreme (0.789 vs 0.747)?
xtabs(~ hsdistrict + fp, data = d) %>% 
  addmargins()
prop.test(x = c(9962, 7014), n = c(12615, 9382))


# Modeling ----------------------------------------------------------------

# Look at the distribution of total values.
# Obviously there is a great deal of variability in the value of a home.
# What might explain that variability?

hist(d$totalvalue, breaks = 40)

# Notice some of that variability appears to be associated with finsqft
plot(totalvalue ~ finsqft, data = d)

# We can attempt to model the totalvalue of a home using finsqft. In other
# words, create a a math formula that allows us to estimate the expected
# totalvalue of a home based on its finsqft.

# MODEL 1
# simple linear model (one predictor)
# model totalvalue as a function of finsqft: totalvalue ~ finsqft
# save to an object called "m1"
m1 <- lm(totalvalue ~ finsqft, data = d)
summary(m1)

# e+05 is scientific notation; it means move the decimal 5 places to the right.

# This sometimes helps with the scientific notation; just look at the
# coefficients component of the model object
m1$coefficients

# Model:
# totalvalue = -164163.6587 + 314.6012*finsqft + ERROR
# ERROR ~ N(0, 259300) 
# ERROR drawn from a Normal dist with mean = 0 and sd = 259300

# Interpretation:
# Each additional finsqft adds about $314 to value of home;
# Predicted home value is expected to be off by about $259,300

# The confint() function reports 95% confidence intervals on the model
# coefficients.
confint(m1)

# Let's use our model to make a prediction.

# What is the expected mean value of homes with 2500 finsqft?
predict(m1, newdata = data.frame(finsqft = 2500), 
        interval = "confidence")

# What is the expected value of a single home with 2500 finsqft?
predict(m1, newdata = data.frame(finsqft = 2500), 
        interval = "prediction")

# But is this a good model? Should we trust these predictions?

# Adjusted R-squared:  0.578 
# This suggests about 58% of the variation in totalvalue is "explained" by
# finsqft.

# Can also look at residuals versus fitted value plot
# residual = observed value - fitted value
# There are 6 available plots; we ask for the first plot
plot(m1, which = 1)

# The dotted line is 0. Values above the line have higher totalvalues than the
# model predicts. Values below the line have lower totalvalues than the model
# predicts. Ideally we would like to see these points hover steadily around 0
# throughout the range of predicted values.

# Verdict: Not great, but perhaps somewhat useful. Predicted values seem to get
# worse as the predicted values get bigger. 

# MODEL 2
# log transform totalvalue
# This changes how we interpret the finsqft coefficient
m2 <- lm(log(totalvalue) ~ finsqft, data = d)
summary(m2)
coef(m2)["finsqft"] * 100

# Every additional 100 finsqft adds about 5% to totalvalue of home

# Is this a good model?
plot(m2, which = 1)

# Verdict: Now predicted values seem worse for smaller predicted values. 

# MODEL 3
# non-linear effect of finsqft using natural splines (ns)
# The ns(finsqft, df = 2) code is similar to fitting a polynomial: x + x^2
library(splines)
m3 <- lm(log(totalvalue) ~ ns(finsqft, df = 2), data = d)
summary(m3)

# There is no interpretation of coefficients

# Is this a good model?
plot(m3, which = 1)

# Verdict: This seems like an improvement on model 2

# Let's visualize and use the model
ggpredict(m3, terms = "finsqft") %>% 
  plot()

# dot.alpha = 0.1 changes transparency of dots
ggpredict(m3, terms = "finsqft") %>% 
  plot(add.data = TRUE, dot.alpha = 0.1)

# Use predict() to make model predictions; need to exponentiate
predict(m3, newdata = data.frame(finsqft = 2500)) %>% exp()

# expected mean value of homes with 2500 finsqft
predict(m3, newdata = data.frame(finsqft = 2500), 
        interval = "confidence") %>% exp()
# expected value of a home with 2500 finsqft
predict(m3, newdata = data.frame(finsqft = 2500), 
        interval = "prediction") %>% exp()

# ggpredict makes it easy to get multiple predictions and 95% CIs
# The CIs are for expected mean values
ggpredict(m3, terms = "finsqft[1500:3000 by=500]") 


# Let's build a more complex model with multiple predictors, including finsqft, cooling, and fullbath.

# Notice we allow finsqft to interact with cooling;
# This says the association between totalvalue and finsqft depends on cooling.
m4 <- lm(log(totalvalue) ~ ns(finsqft, df = 2) + cooling + 
           ns(finsqft, df = 2):cooling + 
           fullbath, 
         data = d)
summary(m4)

# An Analysis of Variance Table can help us assess whether or not interactions
# are warranted. 
anova(m4)

# The first line is a hypothesis test comparing a model with no predictors (just
# the overall mean) to a model with ns(finsqft, df = 2). The null is no
# difference between the models. This is soundly rejected.

# The second line is a hypothesis test comparing a model with ns(finsqft, df =
# 2) to a model with ns(finsqft, df = 2) and cooling. The null is no difference
# between models. This is also soundly rejected. And so on down the table.

# The last line tests whether the interaction should be included. Based on the
# p-value it seems warranted.

# This seems like an OK model, at least for most homes
plot(m4, which = 1)

# Visualize model 

# The interaction appears to happen after 5000 finsqft. But the thick confidence
# ribbon indicates there are few (if any) homes that big without cooling.
ggpredict(m4, terms = c("finsqft", "cooling")) %>% 
  plot()

# The "significant" interaction doesn't seem that interesting when zoomed in on
# a reasonable range of finsqft
ggpredict(m4, terms = c("finsqft[1000:5000]", "cooling")) %>% 
  plot()

# View the effect of fullbaths from 1 - 5
ggpredict(m4, terms = "fullbath[1:5]") %>% 
  plot()

# Notice this plot is for cooling = No Central Air and finsqft = 1936.
ggpredict(m4, terms = "fullbath[1:5]")

# We can manually set the "conditional" values using the condition argument;
# notice the shape doesn't change, just the y-axis.
ggpredict(m4, terms = "fullbath[1:5]", 
          condition = list(cooling = "Central Air",
                           finsqft = 2000)) %>% 
  plot()


# Many hypothesis tests are special cases of linear models

# ANOVA (one categorical predictor)
m5 <- lm(totalvalue ~ hsdistrict, data = d)
anova(m5)
ggpredict(m5, terms = "hsdistrict") %>% plot()

# t-test (one binary predictor)
m6 <- lm(log(totalvalue) ~ cooling, data = d)
summary(m6)
ggpredict(m6, terms = "cooling") %>% plot()

# 2-sample proportion test
# modeling a binary variable
# logistic regression
m7 <- glm(fp ~ hsdistrict, data = d, family = binomial)
summary(m7)
ggpredict(m7, terms = "hsdistrict") %>% 
  plot()

# There are many big books on linear modeling. This was not comprehensive!

# CODE ALONG 5 ------------------------------------------------------------

# (1) Model log(totalvalue) as a function of finsqft, hsdistrict, and their
# interaction using lm(). Call the model "mod1". Visualize the interaction
# between finsqft and hsdistrict.
mod1 <- lm(log(totalvalue) ~ finsqft + hsdistrict + finsqft:hsdistrict, data = d)
anova(mod1)

ggpredict(mod1, terms = c("finsqft[1000:4000 by = 200]", "hsdistrict")) %>% 
  plot()


# (2) Model remodel as a function of hsdistrict using glm(). Name the model
# mod2. In other words, model the probability a home has been remodeled based on
# the high school district it is located in. Visualize the model.

mod2 <- glm(remodel ~ hsdistrict, data = d, family = binomial)
summary(mod2)

ggpredict(mod2, terms = "hsdistrict")
ggpredict(mod2, terms = "hsdistrict") %>% plot()



# Appendix: odds and risk ratios ------------------------------------------




(tab_p[1, 2] - tab_p[2, 2])/tab_p[2, 2]
tab_p[2, 2]/tab_p[1, 2]
epitools::riskratio(tab)

tab_p

# odds
p1 <- tab_p[1, 2]
p2 <- tab_p[2, 2]

odds1 <- p1/(1 - p1)
odds2 <- p2/(1 - p2)

# odds ratio
odds1/odds2
odds2/odds1

epitools::oddsratio(tab)



# Appendix: tidyverse counts and proportions ------------------------------

# Calculating counts and proportions in tidyverse takes a little more work. And
# you always get a data frame.

# One-way
d %>% 
  count(hsdistrict)

d %>% 
  count(hsdistrict) %>% 
  mutate(p = n/sum(n))

# Two-way
# notice it reports NA as a separate level
d %>% 
  group_by(hsdistrict, fp) %>% 
  tally()

# proportions within hsdistrict
d %>% 
  group_by(hsdistrict, fp) %>% 
  tally() %>% 
  mutate(p = n/sum(n)) 

# proportions within fp (switch order in group_by)
d %>% 
  group_by(fp, hsdistrict) %>% 
  tally() %>% 
  mutate(p = n/sum(n)) 

# drop NA; need to use select() and drop_na()
d %>% 
  select(fp, hsdistrict) %>% 
  drop_na() %>% 
  group_by(fp, hsdistrict) %>% 
  tally() %>% 
  mutate(p = n/sum(n)) 

# 3-way
# the stratification variable comes first in group_by()
d %>%
  select(hsdistrict, fp, cooling) %>% 
  group_by(cooling, hsdistrict, fp) %>% 
  drop_na() %>% 
  tally() %>% 
  mutate(p = n/sum(n)) 

# We can pipe into xtabs() to get a matrix-like table
d %>%
  select(hsdistrict, fp, cooling) %>% 
  group_by(cooling, hsdistrict, fp) %>%  
  drop_na() %>% 
  tally() %>% 
  mutate(p = n/sum(n)) %>% 
  xtabs(p ~ hsdistrict + fp + cooling, .) %>% 
  round(3)


# Appendix: bootstrap CI coverage -----------------------------------------


# How often does a bootstrap CI capture the true mean?
ci_test_boot <- pbapply::pbreplicate(n = 1000, expr = {
  boot_mean <- replicate(n = 1000, expr = {
    tv_samp <- sample(tv, replace = TRUE)
    mean(tv_samp)
  })
  boot_ci <- quantile(boot_mean, probs = c(0.025, 0.975))
  boot_ci[1] < true_mean & boot_ci[2] > true_mean
})
mean(ci_test_boot)



# Appendix: CIs by group --------------------------------------------------

# The smean.cl.normal() function can be applied to groups using aggregate or
# tapply;

# sample 100 rows from the homes data set using dplyr::sample_n()
d_samp <- sample_n(d, 100)
aggregate(totalvalue ~ hsdistrict, data = d_samp, smean.cl.normal)
tapply(d_samp$totalvalue, d_samp$hsdistrict, smean.cl.normal)

# We can plot this but it takes some work to wrangle into shape
ci_df <- tapply(d_samp$totalvalue, d_samp$hsdistrict, smean.cl.normal) %>%
  do.call(rbind, .) %>%    # collapse the rows into a matrix
  as.data.frame() %>%      # convert matrix to data frame
  rownames_to_column(., var = "hsdistrict")  # add rownames as variable to data

# create the plot
ggplot(ci_df) +
  aes(x = hsdistrict, y = Mean,
      ymin = Lower, ymax = Upper) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  scale_y_continuous(labels = scales::dollar) +
  labs(y = "Mean total value", x = "high school district")

# This sort of plot is easier to do with a linear model. 

# Confidence intervals for proportions by groups are harder to get.
# Here's one way using the binom.prop.test() function from the binom package
library(binom)
tab_df <- xtabs(~ hsdistrict + fp, data = d_samp) %>% 
  addmargins(margin = 2) %>% 
  apply(MARGIN = 1, function(x)binom.prop.test(x = x["1"], n = x["Sum"])) %>% 
  bind_rows(.id = "hsdistrict")

ggplot(tab_df) +
  aes(x = hsdistrict, y = mean,
      ymin = lower, ymax = upper) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  labs(y = "proportion of homes with fireplaces", 
       x = "high school district")

# This sort of plot is easier to do with a logistic regression model. 


