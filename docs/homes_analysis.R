# some analysis of Albemarle county homes data

library(tidyverse)
library(scales)
library(plotly)

URL <- "https://github.com/uvastatlab/phdplus2022/raw/main/data/albemarle_homes_2022.rds"

homes <- readRDS(url(URL)) %>% 
  filter(hsdistrict != "Unassigned")

medians <- homes %>% 
  group_by(hsdistrict) %>% 
  summarise(n = comma(n()), 
            median_value = dollar(median(totalvalue)))

homes_year_hs <- homes %>%  
  filter(yearbuilt >= 2000) %>% 
  group_by(hsdistrict) %>% 
  count(yearbuilt)

# extract values from data frame
medians$median_value[medians$hsdistrict == "Western Albemarle"]
medians$n[medians$hsdistrict == "Albemarle"]
