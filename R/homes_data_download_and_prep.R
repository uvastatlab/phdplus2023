# Clay Ford
# jcf2d@virginia.edu

# getting and munging albemarle county homes data

library(tidyverse)

# Albemarle county
# Office of Geographic Data Services
# https://www.albemarle.org/government/community-development/gis-mapping/gis-data

# Real Estate Information
# Under PARCELS - Primary Card Level Data - this file includes data
# such as year built, finished square footage, number of rooms, and condition. 
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip

setwd("data/")
link <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link)) # extract file to working directory


# Real Estate Information - Parcel Level Data.  This file contains information
# about the parcel itself such as owner information, deed acreage value, and
# assessed value

# https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip

link2 <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2)) # extract file to working directory

# Other Parcel Characteristics

# This file contains other parcel information that is managed in our development
# tracking system (e.g. Zoning, School Districts, Jurisdictional Areas, etc.).

link3 <- "https://gisweb.albemarle.org/gisdata/CAMA/CityView_View_OtherParcelCharacteristics_TXT.zip"
download.file(link3, destfile = basename(link3))
unzip(basename(link3)) # extract file to working directory

file.remove("GIS_View_Redacted_ParcelInfo_TXT.zip")
file.remove("GIS_CardLevelData_new_TXT.zip")
file.remove("CityView_View_OtherParcelCharacteristics_TXT.zip")

card_level <- read.csv("GIS_CardLevelData_new.txt", 
                       na.strings = "NULL")
parcel_level <- read.csv("GIS_View_Redacted_ParcelInfo.txt", 
                         na.strings = "NULL")
other_parcel <- read.csv("CityView_View_OtherParcelCharacteristics.txt", 
                         na.strings = c("NULL", "N/A"))

# card_level list of variables to keep
vars <- c("TMP", "CardNum", "YearBuilt", "YearRemodeled", 
          "UseCode", "Condition", "NumStories", "FinSqFt", "Cooling", 
          "FP_Open", "Bedroom", "FullBath", "HalfBath", "TotalRooms")

# select listed variables, add variable for source
card <- card_level %>% 
  select(all_of(vars)) %>% 
  mutate(source = "card")

# parcel_level list of variables to keep
vars <- c("ParcelID", "Owner", "LotSize", "PropName",
          "LandValue", "LandUseValue", "ImprovementsValue", 
          "TotalValue", "LastSalePrice", "LastSaleDate1", 
          "Cards")

# select listed variables, add variable for source = parcel
parcel <- parcel_level %>% 
  select(all_of(vars)) %>% 
  mutate(source = "parcel")


# select variables, add variable for source = parcel
other <- other_parcel %>% 
  select(ParcelID, ESDistrict:HSDistrict, CensusTract) %>% 
  mutate(source = "other")

# clean up
rm(card_level, parcel_level, other_parcel, vars)

# Merge card level data with parcel level, other parcel data 
parcel <- parcel %>% 
  left_join(other, by = "ParcelID")

parcel <- parcel %>% 
  select(-source.y) %>% 
  rename(source = source.x)

homes <- full_join(card, parcel, by = c("TMP" = "ParcelID"))

# b. card and parcel is many to one -- parcels can have multiple cards
homes <- full_join(card, parcel, by = c("TMP" = "ParcelID"))



# keep only records in both cards and parcels
# drop source
homes <- homes %>% 
  filter(!is.na(source.y) & !is.na(source.x)) %>% 
  select(-c(source.y, source.x))

# fix names
names(homes) <- tolower(names(homes))

# keep only residential home records (e.g., not businesses, apartment complexes)
res <- c("Duplex", "Single Family", "Single Family-Rental")

homes <- homes %>% 
  filter(usecode %in% res)

# set some features to factors
facvar <- c("cooling", "esdistrict", "msdistrict",
            "hsdistrict", "censustract")

homes <- homes %>% mutate_at(facvar, as.factor)

# sale date should be date
homes <- homes %>% 
  rename(lastsaledate = lastsaledate1) %>% 
  mutate(lastsaledate = lubridate::mdy(lastsaledate))

# remove the rows with TotalValue 0 or NA
homes <- homes %>% 
  filter(!is.na(totalvalue) & totalvalue > 0)

# remove rows with finsqft >= 10000, = 0, or missing
homes <- homes %>% 
  filter(!is.na(finsqft) & finsqft > 0 & finsqft < 10000)

# remove records with 2 or more cards associated with parcel
homes <- homes %>% 
  filter(cards < 2)


# more likely to use this as age than year, create age of home
homes <- homes %>% 
  mutate(age = 2022 - yearbuilt)

# impute median value within census tract for missing
tract_age <- homes %>% 
  group_by(censustract) %>% 
  summarize(med_age = round(median(age, na.rm = TRUE)))

homes <- left_join(homes, tract_age, by = "censustract")

homes <- homes %>% 
  mutate(age = if_else(is.na(age), med_age, age))
rm(tract_age)

# condition
table(homes$condition, useNA = "ifany") # re-order levels of factor

# combine unknown and NA into none and relevel
homes <- homes %>% 
  mutate(condition = if_else(is.na(condition) | 
                               condition == "Unknown",
                             "None",
                             condition))

# set condition to factor
cond_levels <- c("Very Poor", "Poor", "Fair", "Average", "Good", 
                 "Excellent", "None") # define levels/order

homes <- homes %>% 
  mutate(condition = fct_relevel(condition, cond_levels))

table(homes$condition)

# yearremodeled -> remodel indicator
summary(homes$yearremodeled) # NA not remodeled; not sure about 2 or 8
homes <- homes %>% 
  mutate(remodel = if_else(!is.na(yearremodeled), 1, 0))

# drop numstories
homes <- homes %>% select(-numstories)


# cooling
summary(homes$cooling) # fix factor -- assume 00, M1, and NULL are no air
homes <- homes %>% 
  mutate(cooling = fct_collapse(cooling, 
                                "No Central Air" = c("00", "M1", "")))


# fp_open (these are characters)
table(homes$fp_open) # make a binary indicator, 0 and Null are none
homes <- homes %>% 
  mutate(fp = if_else(fp_open == 0, 0, 1))

# create binary indicator for land use (land generates revenue)
homes <- homes %>% 
  mutate(landuse = if_else(landusevalue > 0, 1, 0)) %>% 
  select(-landusevalue) # remove variable

homes <- homes %>% 
  select(-c(tmp, cardnum, fp_open, owner, propname, cards, med_age))

rm(res, facvar, cond_levels)

# save everything to working directory
save.image("albemarle_homes_2022.Rdata") 
# load("albemarle_homes_2020.Rdata")

# save just the homes data frame 
saveRDS(homes, file = "albemarle_homes_2022.rds") 
# readRDS("albemarle_homes_2022.rds")

# save a csv file of the homes data
write_csv(homes, file = "albemarle_homes_2022.csv") 

# homes <- read_csv("albemarle_homes_2020.csv")
