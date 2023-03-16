# USGS Earthquakes Code book
# https://earthquake.usgs.gov/data/comcat/index.php#event-terms

library(readr)
URL <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
d <- read_csv(URL)

# create data frame for code book
var <- names(d)
desc <- c(
  "Time when the event occurred. Times are reported in milliseconds since the epoch",
  "Decimal degrees latitude. Negative values for southern latitudes.",
  "Decimal degrees longitude. Negative values for western longitudes.",
  "The depth where the earthquake begins to rupture. (km)",
  "The magnitude for the event; logarithmic measure.",
  "The method or algorithm used to calculate the preferred magnitude for the event.",
  "The total number of seismic stations used to determine earthquake location.",
  "The largest azimuthal gap between azimuthally adjacent stations (in degrees). In general, the smaller this number, the more reliable is the calculated horizontal position of the earthquake. Earthquake locations in which the azimuthal gap exceeds 180 degrees typically have large location and depth uncertainties.",
 "Horizontal distance from the epicenter to the nearest station (in degrees). 1 degree is approximately 111.2 kilometers. In general, the smaller this number, the more reliable is the calculated depth of the earthquake.",
 "The root-mean-square (RMS) travel time residual, in sec, using all weights. This parameter provides a measure of the fit of the observed arrival times to the predicted arrival times for this location. Smaller numbers reflect a better fit of the data. The value is dependent on the accuracy of the velocity model used to compute the earthquake location, the quality weights assigned to the arrival time data, and the procedure used to locate the earthquake.",
 "The ID of a data contributor. Identifies the network considered to be the preferred source of information for this event.",
 "A unique identifier for the event.",
 "Time when the event was most recently updated.",
 "Textual description of named geographic region near to the event.",
 "Type of seismic event.",
 "Uncertainty of reported location of the event in kilometers.",
 "Uncertainty of reported depth of the event in kilometers.",
 "Uncertainty of reported magnitude of the event. The estimated standard error of the magnitude.",
 "The total number of seismic stations used to calculate the magnitude for this earthquake.",
 "Indicates whether the event has been reviewed by a human.",
 "The network that originally authored the reported location of this event.",
 "Network that originally authored the reported magnitude for this event."
)
cb <- data.frame(variable = var, description = desc)
saveRDS(cb, file = "data/codebook.rds")
