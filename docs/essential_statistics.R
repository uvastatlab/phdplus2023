# PhD Plus: Essential Statistics
# Clay Ford
# Spring 2023


library(tidyverse)

# Read in data from GitHub
URL <- "https://github.com/uvastatlab/phdplus2023/raw/main/data/albemarle_homes_2023.rds"
d <- readRDS(url(URL))


# Counts and proportions --------------------------------------------------

# how to create a cross tabulation
xtabs(~ hsdistrict + fp, data = d)

# drop the Unassigned row
xtabs(~ hsdistrict + fp, data = d, 
      subset = hsdistrict != "Unassigned", 
      drop.unused.levels = TRUE)

d %>% 
  filter(hsdistrict != "Unassigned") %>% 
  xtabs(~ hsdistrict + fp, data = ., drop.unused.levels = TRUE)

# save table
tab <- xtabs(~ hsdistrict + fp, data = d, 
      subset = hsdistrict != "Unassigned", 
      drop.unused.levels = TRUE)

# Given high school district, what proportion of homes do and do not have fireplaces? Condition on rows. (ie rows sum to 1)
proportions(tab, margin = 1) |>
  round(2)

# Given homes with and without fireplaces, what proportions are in each high school district? Condition on columns (ie, columns sum to 1)
proportions(tab, margin = 2) |>
  round(2)

# save table
tab_p <- proportions(tab, margin = 1) |>
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

# Proportion of homes over 2000 fin sq ft in size
mean(d$finsqft > 2000)

# Proportion of homes with 3 or 4 bedrooms
mean(d$bedroom %in% 3:4)

mean(d$totalvalue < 580200)
sum(d$totalvalue > 580200)

## CODE ALONG 1

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
aggregate(totalvalue ~ hsdistrict + cooling, data = d, median)
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
  geom_jitter(alpha = 1/5)

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

## CODE ALONG 2


# Uncertainty -------------------------------------------------------------

# uncertainty about a mean
# random sample 30 total home values
samp <- sample(d$totalvalue, 30)
mean(samp)
sd(samp)/sqrt(30)

# repeat 10,000 times
means <- replicate(n = 10000, expr = {
  samp <- sample(d$totalvalue, 30)
  mean(samp)
})
sd(means) # standard error based on 10,000 means

# add and subtract 2 standard errors from mean to form approximate 95%
# confidence interval
SE <- sd(means)
mean(samp) + c(-1, 1)*2*SE

# a mathematical approach is provided via t.test()
t.test(samp)
# by hand
qt(0.975, df = 29)
mean(samp) + c(-1, 1)*2.04523*sd(samp)/sqrt(30)


# uncertainty about a proportion
# random sample of 30 homes' fireplace status
samp2 <- sample(d$fp, 30)
table(samp2)
mean(samp2)

# quickly get a confidence interval
prop.test(x = 22, n = 30)
prop.test(x = 22, n = 30, correct = FALSE)

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

## CODE ALONG 3

# Hypothesis Testing ------------------------------------------------------

library(ISwR)
data("energy")
str(energy)

ggplot(energy) +
  aes(x = expend, fill = stature) +
  geom_density(alpha = 1/4)

aggregate(expend ~ stature, data = energy, mean)
t.test(expend ~ stature, data = energy)



# compare proportions
addmargins(tab)
prop.test(x = c(9962, 7014), n = c(12615, 9382))




prop.test(x = sum(d$finsqft > 2000), 
          n = nrow(d))



# Means and Medians

mean(d$totalvalue)
median(d$totalvalue)
summary(d$totalvalue)
hist(d$totalvalue)




aggregate(totalvalue ~ fp, data = d, mean)
# not really necessary
# also not appropriate to do after peeking at the data
t.test(totalvalue ~ fp, data = d)

# HARKing: hypothesizing after the results are known


# Correlation
# visualize
# limitations
cor.test(~ totalvalue + finsqft, data = d)
plot(totalvalue ~ finsqft, data = d)

library(datasauRus)
cor.test(~ dino_x + dino_y, datasaurus_dozen_wide)
plot(dino_y ~ dino_x, data = datasaurus_dozen_wide)


# linear model
m <- lm(totalvalue ~ finsqft, data = d)
coef(m)
confint(m)




# standard error is the standard deviation of a hypothetical sampling distribution





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

