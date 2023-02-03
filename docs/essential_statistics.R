# PhD Plus: Essential Statistics
# Clay Ford
# Spring 2023


library(tidyverse)

# Read in data from GitHub
URL <- "https://github.com/uvastatlab/phdplus2023/raw/main/data/albemarle_homes_2023.rds"
d <- readRDS(url(URL))

# drop the Unasssigned hsdistrict homes from the data 
d <- d %>% 
  filter(hsdistrict != "Unassigned") %>% 
  mutate(hsdistrict = droplevels(hsdistrict))


# Counts and proportions --------------------------------------------------

# how to create a cross tabulation
xtabs(~ hsdistrict + fp, data = d)

# save table
tab <- xtabs(~ hsdistrict + fp, data = d)

# Given high school district, what proportion of homes do and do not have fireplaces? Condition on rows. (ie rows sum to 1)
tab %>% 
  proportions(margin = 1) %>% 
  round(2)

# Given homes with and without fireplaces, what proportions are in each high school district? Condition on columns (ie, columns sum to 1)
tab %>% 
  proportions(margin = 2) %>% 
  round(2)

# save table
tab_p <- tab %>% 
  proportions(margin = 1) %>% 
  round(2)
tab_p

# what's the difference in proportion of homes with a fireplace between
# Albemarle and Monticello? 

# absolute difference = 0.16 or 16%
tab_p["Albemarle", "1"] - tab_p["Monticello", "1"]
tab_p[1,2] - tab_p[2,2]

# Albemarle has 0.16, or 16%, more homes with fireplaces.

# relative difference = 1.25 or 25% 
tab_p["Albemarle", "1"]/tab_p["Monticello", "1"]
tab_p[1, 2]/tab_p[2, 2]

# We expect a given home in Albemarle is 0.25, or 25%, more likely to have a
# fireplace than a home in Monticello.

# Can also calculate proportions based on a condition.
# Proportion of homes over 2000 fin sq ft in size
mean(d$finsqft > 2000)

# Proportion of homes with 3 or 4 bedrooms
mean(d$bedroom %in% 3:4)


# CODE ALONG 1 ------------------------------------------------------------

# Compare the absolute and relative proportions of homes with no central air in
# the Burley and Walton middle school districts.
tp <- xtabs(~ msdistrict + cooling, data = d) %>% 
  proportions(margin = 1) %>% 
  round(3)
tp

# absolute
tp["Burley", "No Central Air"] - tp["Walton", "No Central Air"]

# relative
tp["Burley", "No Central Air"]/tp["Walton", "No Central Air"]



# Summarizing numeric data ------------------------------------------------

# summary() and hist() make a powerful combination
summary(d$lotsize)
hist(d$lotsize)
# use breaks to increase/decrease number of bins
hist(d$lotsize, breaks = 100)

# standard deviation and IQR
sd(d$totalvalue)
IQR(d$totalvalue)

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
Fn(500000) # 66th percentile
Fn(1e6)    # 93rd percentile

# we can plot the ECD
plot(Fn)

# summaries by group
# returns a data frame
aggregate(totalvalue ~ hsdistrict, data = d, mean)
aggregate(totalvalue ~ hsdistrict, data = d, median)
aggregate(totalvalue ~ hsdistrict, data = d, IQR)
aggregate(totalvalue ~ hsdistrict, data = d, summary) # kind of messy

# returns a vector or list
tapply(d$totalvalue, d$hsdistrict, mean)
tapply(d$totalvalue, d$hsdistrict, median)
tapply(d$totalvalue, d$hsdistrict, IQR)
tapply(d$totalvalue, d$hsdistrict, summary)  # list

# by 2 or more groups
# returns a data frame
aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
# returns a matrix
tapply(d$totalvalue, list(d$hsdistrict, d$cooling), mean)


# A matrix is desirable for reporting
# A data frame is desirable for plotting 
m_df <- aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
ggplot(m_df) +
  aes(x = hsdistrict, y = totalvalue, color = cooling, group = cooling) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = 'Median home values by HS District and Central Air status')


# summaries available in packages
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
cor(d$bedroom, d$fullbath, use = "complete.obs") # Why NA?

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
  geom_smooth()

# (4) summarize the log-transformed improvements value. What do we notice?
summary(d$improvementsvalue)
hist(d$improvementsvalue, breaks = 200)
summary(log(d$improvementsvalue))
summary(log(d$improvementsvalue[d$improvementsvalue > 0]))

d %>% 
  filter(improvementsvalue > 0) %>% 
  summarise(m = exp(mean(log(improvementsvalue))))

# Uncertainty -------------------------------------------------------------

# uncertainty about a mean
# random sample 50 total home values
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

# a mathematical approach is provided via t.test()
# This is what we typically do in practice
t.test(samp)
t.test(samp)$conf.int
Hmisc::smean.cl.normal(samp)


# uncertainty about a proportion
# random sample of 50 homes' fireplace status
samp2 <- sample(d$fp, 50)
table(samp2)
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
mean(samp2) + c(-1, 1)*2*SE_p

# a mathematical approach is provided via prop.test
prop.test(x = sum(samp2), n = 50)
prop.test(x = sum(samp2), n = 50, correct = FALSE)



# 95% Confidence intervals
# The process of calculating a confidence interval works 95% of the time
# The "95%" is based on tradition. Can do 89%, 78%, etc.
z <- rnorm(30, mean = 10, sd = 2)
tout <- t.test(z)
tout$conf.int[1] < 10 & tout$conf.int[2] > 10

ci_test <- replicate(n = 1000, expr = {
  z <- rnorm(30, mean = 10, sd = 2)
  tout <- t.test(z)
  tout$conf.int[1] < 10 & tout$conf.int[2] > 10
})

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

# take our one sample
tv <- sample(d$totalvalue, 50)

# now resample from sample with replacement
boot_mean <- replicate(n = 1000, expr = {
  tv_samp <- sample(tv, replace = TRUE)
  mean(tv_samp)
})
# get 95% CI as 2.5 and 97.5 percentiles
quantile(boot_mean, probs = c(0.025, 0.975))

# or just smean.cl.boot() function from the Hmisc package
Hmisc::smean.cl.boot(tv)


# CODE ALONG 3 ------------------------------------------------------------


prop.test(x = sum(d$finsqft > 2000), 
          n = nrow(d))

# Hypothesis Testing ------------------------------------------------------

library(ISwR)
data("energy")
str(energy)

ggplot(energy) +
  aes(x = expend, fill = stature) +
  geom_density(alpha = 1/4)

ggplot(energy) +
  aes(y = stature, x = expend) +
  geom_jitter(height = 0.05)


aggregate(expend ~ stature, data = energy, mean)

# Assuming no difference between lean and obese, what is probability we would
# get data like this or more extreme (8.1 vs 10.3)?
t.test(expend ~ stature, data = energy)



# compare proportions
# save table
tab <- xtabs(~ hsdistrict + fp, data = d, 
             subset = hsdistrict != "Unassigned", 
             drop.unused.levels = TRUE)

tab %>% 
  proportions(margin = 1) %>% 
  round(2)

# Assuming no difference in proportion of fireplaces in Albemarle and Western
# Albemarle high school districts, what is probability we would get data like
# this or more extreme (0.79 vs 0.75)?
addmargins(tab)
prop.test(x = c(9962, 7014), n = c(12615, 9382))


aggregate(totalvalue ~ fp, data = d, mean)
# not appropriate to do after peeking at the data
t.test(totalvalue ~ fp, data = d)

# HARKing: Hypothesizing After the Results are Known


# CODE ALONG 4 ------------------------------------------------------------




# Modeling ----------------------------------------------------------------


library(ggeffects)

# Look at the distribution of total values.
# Obviously there is a great deal of variability in the value of a home.
# What might explain that variability?

hist(d$totalvalue, breaks = 40)

# Notice some of that variability appears to be associated with finsqft
plot(totalvalue ~ finsqft, data = d)

# We can attempt to model the value of a home using a linear model

# simple linear model (one predictor)
# model totalvalue as a function of finsqft (totalvalue ~ finsqft)
# save to an object called "m1"
m1 <- lm(totalvalue ~ finsqft, data = d)
summary(m1)
coef(m1)

# Model:
# totalvalue = -164163.6587 + 314.6012*finsqft + ERROR
# ERROR ~ N(0, 259300) 
# ERROR drawn from a Normal dist with mean = 0 and sd = 259300

# Is this a good model? 
# Look at residuals versus fitted value plot
# residual = observed value - fitted value
plot(m1, which = 1)

# the model under-predicts totalvalue for number of homes
# for home 11719, it's off by about 6 million dollars
d[11719, "totalvalue"]

# Use predict() to make model predictions
# Model predictions expected to be off by about $260,000
predict(m1, newdata = data.frame(finsqft = 1800))
predict(m1, newdata = data.frame(finsqft = 1800), interval = "confidence")

# Visualize model;
# For homes under 2.5 million and less than 5000 finsqft, it seems ok.
ggpredict(m1, terms = "finsqft") %>% 
  plot(add.data = TRUE)

# Now add cooling to model
m2 <- lm(totalvalue ~ finsqft + cooling, data = d)
summary(m2)
coef(m2)

plot(m2, which = 1)

# Visualize model;
ggpredict(m2, terms = c("finsqft", "cooling")) %>% 
  plot(add.data = TRUE)


# log transform totalvalue
m3 <- lm(log(totalvalue) ~ finsqft + cooling, data = d)
summary(m3)
coef(m3)
plot(m3, which = 1)
ggpredict(m3, terms = c("finsqft", "cooling")) %>% 
  plot(add.data = TRUE)

# non-linear effect of finsqft using natural splines
library(splines)
m4 <- lm(log(totalvalue) ~ ns(finsqft, df = 2) + cooling, data = d)
summary(m4)
plot(m4, which = 1)
ggpredict(m4, terms = c("finsqft", "cooling")) %>% 
  plot(add.data = TRUE)

# allow effect of finsqft to interact with cooling
# The effect of finsqft depends on cooling
m5 <- lm(log(totalvalue) ~ ns(finsqft, df = 2) * cooling, data = d)
summary(m5)
plot(m5, which = 1)
ggpredict(m5, terms = c("finsqft", "cooling")) %>% 
  plot()

# can use ggpredict to see predictions and 95% CIs
eff <- ggpredict(m5, terms = c("finsqft[1000:3000 by=500]", "cooling")) %>% 
  plot()

# ANOVA (one categorical predictor)
aov1 <- aov(log(totalvalue) ~ hsdistrict, data = d, 
            subset = hsdistrict != "Unassigned")
summary(aov1)
# using lm()
m4 <- lm(log(totalvalue) ~ hsdistrict, data = d, 
         subset = hsdistrict != "Unassigned")
summary(m4)
anova(m4)

# t-test (one binary predictor)
t.test(log(totalvalue) ~ cooling, data = d)
# using lm()
m5 <- lm(log(totalvalue) ~ cooling, data = d)
summary(m5)

# multiple linear regression
m6 <- lm(log(totalvalue) ~ ns(finsqft, df = 3) + hsdistrict + 
           cooling, data = d,
         subset = hsdistrict != "Unassigned")
summary(m6)

# Use model to make a prediction
home <- data.frame(finsqft = 2500, hsdistrict = "Albemarle", 
                   cooling = "Central Air")
predict(m6, newdata = home, interval = "confidence")

# convert to original scale
predict(m6, newdata = home, interval = "confidence") %>% exp()


# modeling a binary variable
ggplot(d) +
  aes(x = finsqft, y = fp) +
  geom_point(alpha = 1/10, shape = ".") +
  geom_smooth() +
  scale_y_continuous(breaks = 0:1, minor_breaks = FALSE, limits = c(0,1))

# logistic regression
lrm1 <- glm(fp ~ finsqft, data = d, family = binomial)
summary(lrm1)
exp(coef(lrm1)["finsqft"])
exp(coef(lrm1)["finsqft"]*100)

home <- data.frame(finsqft = seq(1500,2000,100))
predict(lrm1, newdata = home, type = "response")

ggpredict(lrm1, terms = "finsqft[1500:2000 by=100]")

# odds ratio by hand
p1 <- 0.6055023
p2 <- 0.6396780

odds1 <- p1/(1 - p1)
odds2 <- p2/(1 - p2)

odds2/odds1
# compare to
exp(coef(lrm1)["finsqft"]*100)


ggpredict(lrm1, terms = "finsqft") %>% plot()



# CODE ALONG 5 ------------------------------------------------------------




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