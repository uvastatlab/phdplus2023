# essential stats data prep
# March 2023

URL <- "https://github.com/uvastatlab/phdplus2023/raw/main/data/albemarle_homes_2023.rds"
d <- readRDS(url(URL))

# drop the Unassigned homes
d <- subset(d, hsdistrict != "Unassigned")
d$hsdistrict <- droplevels(d$hsdistrict)
d$esdistrict <- droplevels(d$esdistrict)
d$msdistrict <- droplevels(d$msdistrict)

# set fp as a factor
d$fp <- factor(d$fp, labels = c("No", "Yes"))

saveRDS(object = d, file = "data/homes.rds")
