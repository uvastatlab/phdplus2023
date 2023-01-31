# PhD Plus: Essential Statistics
# Clay Ford
# Spring 2023


library(tidyverse)

# Read in data from GitHub
URL <- "https://github.com/uvastatlab/phdplus2023/raw/main/data/albemarle_homes_2023.rds"
d <- readRDS(url(URL))

# drop the Unasssigned homes from the data 
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

# discrete data are better visualized with a bar plot
summary(d$bedroom)
hist(d$bedroom)
plot(table(d$bedroom))
barplot(table(d$bedroom))

ggplot(d) +
  aes(x = bedroom) +
  geom_bar() +
  scale_x_continuous(breaks = 0:12, minor_breaks = FALSE)

# Means/Medians/summaries by group
aggregate(totalvalue ~ hsdistrict, data = d, mean)
aggregate(totalvalue ~ hsdistrict, data = d, median)
aggregate(totalvalue ~ hsdistrict, data = d, summary) # kind of messy

tapply(d$totalvalue, d$hsdistrict, mean)
tapply(d$totalvalue, d$hsdistrict, median)
tapply(d$totalvalue, d$hsdistrict, summary) 

# by 2 or more groups
# returns a data frame
aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
# returns a matrix
tapply(d$totalvalue, list(d$hsdistrict, d$cooling), mean)

# Correlation
cor(d$finsqft, d$totalvalue, method = "pearson")
cor(d$finsqft, d$totalvalue, method = "spearman")

# Correlation should always be accompanied by a scatter plot
plot(totalvalue ~ finsqft, data = d)

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
  select(finsqft, lotsize, totalvalue) %>% 
  cor()

# Base 10 log transformations
# what power do we raise 10 to get x?
x <- c(10, 100, 1000, 10000, 100000)
log10(x) # number of zeroes after 1 (ie, orders of magnitude)


# natural log
# what power do we raise e to get x?
log(x)

# to undo a log transformation we take the "anti-log", or exponentiate
logx <- log(x)
logx
exp(logx)

# before/after log transformation
hist(d$totalvalue)
hist(log(d$totalvalue))

hist(d$lotsize)
hist(log(d$lotsize))


# CODE ALONG 2 ------------------------------------------------------------



# Uncertainty -------------------------------------------------------------

# uncertainty about a mean
# random sample 30 total home values
samp <- sample(d$totalvalue, 30)
mean(samp)
sd(samp)/sqrt(30)  # standard error of the mean

# repeat 10,000 times
means <- replicate(n = 10000, expr = {
  samp <- sample(d$totalvalue, 30)
  mean(samp)
})
sd(means) # standard error based on 10,000 means

# add and subtract 2 standard errors from mean to form approximate 95%
# confidence interval
SE_m <- sd(means)
mean(samp) + c(-1, 1)*2*SE_m

# a mathematical approach is provided via t.test()
t.test(samp)


# uncertainty about a proportion
# random sample of 30 homes' fireplace status
samp2 <- sample(d$fp, 30)
table(samp2)
mean(samp2, na.rm = TRUE)

# repeat 10,000 times
props <- replicate(n = 10000, expr = {
  samp2 <- sample(d$fp, 30)
  mean(samp2, na.rm = TRUE)
})
sd(props) # standard error based on 10,000 means

# add and subtract 2 standard errors from mean to form approximate 95%
# confidence interval
SE_p <- sd(props)
mean(samp2) + c(-1, 1)*2*SE_p

# a mathematical approach is provided via prop.test
prop.test(x = sum(samp2), n = 30)
prop.test(x = sum(samp2), n = 30, correct = FALSE)



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
samp <- sample(d$totalvalue, 30)
tout <- t.test(samp)
tout$conf.int[1] < true_mean & tout$conf.int[2] > true_mean

ci_test <- replicate(n = 1000, expr = {
  samp <- sample(d$totalvalue, 30, replace = TRUE)
  tout <- t.test(samp)
  tout$conf.int[1] < true_mean & tout$conf.int[2] > true_mean
})
mean(ci_test)

# Try it with log-transformed home values
true_log_mean <- mean(log(d$totalvalue))
ci_test_log <- replicate(n = 1000, expr = {
  samp <- sample(d$totalvalue, 30, replace = TRUE)
  tout <- t.test(log(samp))
  tout$conf.int[1] < true_log_mean & tout$conf.int[2] > true_log_mean
})
mean(ci_test_log)

# In practice we only get one sample so we can't do what we did above.

# However we can do something called "bootstrapping" that treats the sample like
# the population.

# take our one sample
tv <- sample(d$totalvalue, 30)

# now resample from sample with replacement
boot_mean <- replicate(n = 1000, expr = {
  tv_samp <- sample(tv, replace = TRUE)
  mean(tv_samp)
})
# get 95% CI as 2.5 and 97.5 percentiles
quantile(boot_mean, probs = c(0.025, 0.975))




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


mean(d$totalvalue)
mean(d$totalvalue[d$finsqft == 1280])
mean(d$totalvalue[d$finsqft == 1280 & d$hsdistrict == "Albemarle"])

library(ggeffects)

# simple linear model (one predictor)
m1 <- lm(totalvalue ~ finsqft, data = d)
summary(m1)
plot(m1, which = 1)
ggpredict(m1, terms = "finsqft") %>% plot(add.data = TRUE)


m2 <- lm(log(totalvalue) ~ finsqft, data = d)
summary(m2)
plot(m2, which = 1)
ggpredict(m2, terms = "finsqft") %>% plot(add.data = TRUE)


library(splines)
m3 <- lm(log(totalvalue) ~ ns(finsqft, df = 3), data = d)
summary(m3)
plot(m3, which = 1)
ggpredict(m3, terms = "finsqft") %>% plot(add.data = TRUE)

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