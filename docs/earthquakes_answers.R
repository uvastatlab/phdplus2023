# PhD Plus Final Project code
# April 5, 2023
# Clay Ford, UVA Library


## 1. Read in the data
library(tidyverse)
URL <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
d <- read_csv(URL)
d <- read_csv(URL, show_col_types = FALSE) # turn off the messages

## 2. Take a quick glimpse of the data 
str(d)
summary(d)
names(d)
nrow(d)
summary(d$mag)
table(d$type)


## 3. Summarize magnitude numerically and visually

# - Summarize "mag" numerically
summary(d$mag)
quantile(d$mag, probs = 1:9/10)
Hmisc::describe(d$mag)

# - Summarize "mag" visually
ggplot(d) +
  aes(x = mag) +
  geom_density()

ggplot(d) +
  aes(x = mag) +
  geom_histogram(binwidth = 0.25)


# base R
hist(d$mag)


# - What are the top 10 biggest earthquakes in our data?
d %>% 
  arrange(desc(mag)) %>% 
  select(place, mag) %>% 
  head(10)



## 4. Summarize the relationship between depth and magnitude numerically and visually

# - Create a visualization with "mag" on the y-axis. 
ggplot(d) +
  aes(x = depth, y = mag) +
  geom_point() +
  geom_smooth()

# - Summarize the relationship numerically.
cor(d$depth, d$mag, method = "spearman")



## 5. Investigate the status of the seismic event

# - How many events have been reviewed?
table(d$status)

# - What proportion have been reviewed?
mean(d$status == "reviewed")
table(d$status) %>% proportions()


# - The magSource variable reports the network that originally authored the reported magnitude for this event. Create a cross tabulation between status and magSource showing status proportion for each magSource. See this page for network definitions: <https://www.emsc-csem.org/Earthquake/contributors.php>
xtabs(~ magSource + status, data = d) %>% 
  proportions(margin = 1) %>% 
  round(2)


## 6. Visualize the relationship between depth and magnitude by status

# Update your earlier plot that visualized the relationship between depth and magnitude to incorporate information about status. 

ggplot(d) +
  aes(x = depth, y = mag) +
  geom_point(alpha = 1/5) +
  facet_wrap(~status)

d %>% 
  group_by(status) %>% 
  summarise(cor = cor(depth, mag, method = "spearman"))


## 7. Investigate the various seismic events

# - What are the counts of the various seismic events? 
table(d$type)

d %>% 
  count(type)

# - What proportion of the seismic events were "earthquakes"?
mean(d$type == "earthquake")

d %>% 
  count(type) %>% 
  mutate(prop = n / sum(n)) %>% 
  slice(1)

# - What was the maximum magnitude for each seismic event?
aggregate(mag ~ type, data = d, max)


d %>% 
  group_by(type) %>% 
  summarize(max = max(mag, na.rm = TRUE))


# - What proportion of seismic events are NOT recorded as "earthquakes"?
mean(d$type != "earthquake")


## 8. Visualize the distribution of magnitude between seismic events
ggplot(d) +
  aes(x = type, y = mag) +
  geom_boxplot()

ggplot(d) +
  aes(x = type, y = mag) +
  geom_jitter(height = 0)

ggplot(d) +
  aes(x = mag, y = type) +
  ggridges::geom_density_ridges()


# 9. Visualize the relationship between depth and depth error

# - Create a scatter plot to visualize the relationship between "depthError" and "depth". Plot "depthError" on the y-axis. 
ggplot(d) +
  aes(x = depth, y = depthError) +
  geom_point() +
  geom_smooth()

# - Which observations (if any) had depth errors larger than 40? List those observations showing the status, depth, depthError, place, and mag columns.
d %>% 
  filter(depthError > 40) %>% 
  select(depth, depthError, place, mag, status)

## 10. Visualize the relationship between magError and magType

# How does the distribution of magError differ between magType? Are there certain methods that have higher error? _Create at least two visualizations to explore this_. 
ggplot(d) +
  aes(x = magType, y = magError) +
  geom_boxplot()

ggplot(d) +
  aes(x = magType, y = magError) +
  geom_jitter(width = 0.3, height = 0, alpha = 1/3)

ggplot(d) +
  aes(x = magType, y = magError) +
  geom_violin()


## 11. Investigate location of earthquakes

# - What are the top five locations for most seismic events? 
d %>% 
  count(location) %>% 
  slice_max(n, n = 5)

d %>% 
  count(location) %>% 
  arrange(desc(n)) %>% 
  head(n = 5)

# - How many locations have experienced more than 100 seismic events in the past 30 days?
d %>% 
  count(location) %>% 
  filter(n > 100) %>% 
  nrow()

# or this works too
sum(table(d$location) > 100)


# - Which locations experienced more than 5 seismic events classified as `type == "quarry blast"`. 
d %>% 
  filter(type == "quarry blast") %>% 
  count(location) %>% 
  arrange(desc(n)) %>% 
  filter(n > 5)

## 12. Do seismic events in some locations tend to have larger magError?

# The "magError" column reports the uncertainty of the reported magnitude of an event. (ie, the estimated standard error of the magnitude.) A "magError" greater than 3 is considered large. Using our new "location" column, is there a location that seems to consistently have large "magErrors" (greater than 3)? 
d %>% 
  filter(magError > 3) %>% 
  count(location)


## 13. Investigate elapsed time between seismic events

# - Visualize the distribution of elapsed time.
hist(d$elapsed_time)

ggplot(d) +
  aes(x = elapsed_time) +
  geom_density() 

ggplot(d) +
  aes(x = elapsed_time) +
  geom_density() +
  geom_function(fun = dexp, args = list(rate = 1/mean(d$elapsed_time, na.rm = TRUE)), color = "red")

# - What is the median elapsed time between seismic events? In other words, about how often is the USGS detecting seismic events on earth?

median(d$elapsed_time, na.rm = TRUE)

# - What's the estimated probability a seismic event happens in the next 60 seconds? (Hint: what is the proportion of elapsed times less than 1 minute?)

mean(d$elapsed_time < 1, na.rm = TRUE)
pexp(1, rate = 1/mean(d$elapsed_time, na.rm = T))


## 14. Investigate elapsed time in specific places

# - What was the median elapsed time between seismic events in "The Geysers, CA"? Hint: we need to recalculate elapsed time for just "The Geysers, CA".

d %>% 
  filter(location == "The Geysers, CA") %>% 
  mutate(elapsed_time = difftime(time, lead(time), units = "mins")) %>% 
  summarise(med = median(elapsed_time, na.rm = TRUE))

# 
# - What was the median elapsed time between seismic events in "Pāhala, Hawaii"? Tip: "Pāhala, Hawaii" has a special character in it. When you `filter()`, consider using something like `str_detect(location, "P*hala, Hawaii")`

d %>% 
  filter(str_detect(location, "P*hala, Hawaii")) %>% 
  mutate(elapsed_time = difftime(time, lead(time), units = "mins")) %>% 
  summarise(med = median(elapsed_time, na.rm = TRUE)) 


## 15. Seismic events in the United States

# - What proportion of all seismic events recorded by the USGS occurred in the United States?

mean(d$US)

# - Visualize the distribution of "mag" for those events that occurred in the US.
ggplot(filter(d, US)) +
  aes(x = mag) +
  geom_density()

# - Count up the types of seismic events that occurred in the US.
d %>% 
  filter(US) %>% 
  count(type)

# - What is the median seismic event magnitude in the US?
d %>% 
  filter(US) %>% 
  summarise(med = median(mag, na.rm = TRUE))


# - how does the median seismic event magnitude for the US differ from the rest of the world?
d %>% 
  group_by(US) %>% 
  summarise(med = median(mag, na.rm = TRUE))


## 16. Seismic events in the United States by State

# - Count the number of seismic events for each state and display in descending order
us %>% 
  count(state) %>% 
  arrange(desc(n)) 

# - What are the top five states for highest magnitude experienced? 
us %>% 
  group_by(state) %>% 
  summarize(max = max(mag)) %>% 
  arrange(desc(max)) %>% 
  head(n = 5)


# - Earthquakes occur rather frequently in Hawaii. What's the estimated mean magnitude of those events? Include a confidence interval on your estimate.
Hmisc::smean.cl.normal(us$mag[us$state == "Hawaii"])

# The tidyverse way requires the use of `pull()`
us %>% 
  filter(state == "Hawaii") %>% 
  pull(mag) %>% 
  Hmisc::smean.cl.normal()

