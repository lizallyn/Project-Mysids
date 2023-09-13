# Mysid Analysis 4 sites
# separated from full analysis file on Sept 13, 2023

# read in data
full.whales <- read.csv("Whales in full survey area 2019 2020.csv")
sites <- read.csv("Sample site coords for R.csv")
whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/whales%20per%20day%20for%20R.csv")
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")
# Pull out useful clean mysid data columns to simplify data frame
data <- data.full[,c(1,2,4:6,7,11,17:25,30,31)]
data <- data.frame(data)
# remove single rasmussen tow
data <- data[-which(data$Site == "Rasmussen"),]
# make names match whale data for sites
data$Site[which(data$Site == "North of Bodelteh Islands")] <- "North of Bodeltehs"
data$Site[which(data$Site == "South of Bodelteh Islands")] <- "South of Bodeltehs"
data$Site[which(data$Site == "Seal And Sail")] <- "Seal and Sail Rocks"
data$Site <- as.factor(data$Site)
data$Date_site <- paste(data$Date, data$Site, sep="_")
# simplified version of data
data.simple <- data[, c("Date", "Region", "MysidCount", "Avg.length", "Date_site")]

# Packages
library(tidyr)
library(dplyr)

# Assign whale sightings to a region

ES.Eastof <- -124.6008
WS.Eastof <- -124.726
WS.Northof <- 48.37437
O.Southof <- 48.38615
O.Westof <- -124.6529
# assign regions
full.whales$Region <- "Error"
full.whales$Region[full.whales$Start_Dec_Long > ES.Eastof] <- "East Strait"
full.whales$Region[between(full.whales$Start_Dec_Long, left = WS.Eastof, 
                           right = ES.Eastof) & 
                     full.whales$Start_Dec_Lat > WS.Northof] <- "West Strait"
full.whales$Region[which(full.whales$Start_Dec_Long < O.Westof & 
                           full.whales$Start_Dec_Lat < O.Southof)] <- "Ocean"

# Whale sightings within 600m grid square of tow sites

coordmatch <- data.frame(matrix(nrow= nrow(full.whales)))
# add sighting coords and IDs
coordmatch$Date <- full.whales$Date
coordmatch$Date_S <- full.whales$Date_S
coordmatch$lat <- full.whales$Start_Dec_Lat
coordmatch$long <- full.whales$Start_Dec_Long
# create columns for assigned site names
coordmatch$site.lat <- "Error"
coordmatch$site.long <- "Error2"
# add min lat columns
coordmatch$AR.lat.min <- sites$Min.lat[which(sites$Location=="Anderson Rocks")]
coordmatch$BB.lat.min <- sites$Min.lat[which(sites$Location=="Bullman Beach")]
coordmatch$SB.lat.min <- sites$Min.lat[which(sites$Location=="South of Bodeltehs")]
coordmatch$CB.lat.min <- sites$Min.lat[which(sites$Location=="Chito Beach")]
coordmatch$DP.lat.min <- sites$Min.lat[which(sites$Location=="Duk Point")]
coordmatch$NB.lat.min <- sites$Min.lat[which(sites$Location=="North of Bodeltehs")]
coordmatch$FB.lat.min <- sites$Min.lat[which(sites$Location=="First Beach")]
coordmatch$KL.lat.min <- sites$Min.lat[which(sites$Location=="Koitlah")]
coordmatch$OI.lat.min <- sites$Min.lat[which(sites$Location=="Ozette Island")]
coordmatch$PH.lat.min <- sites$Min.lat[which(sites$Location=="Portage Head")]
coordmatch$SR.lat.min <- sites$Min.lat[which(sites$Location=="Sail River")]
coordmatch$SS.lat.min <- sites$Min.lat[which(sites$Location=="Seal and Sail Rocks")]
coordmatch$SK.lat.min <- sites$Min.lat[which(sites$Location=="Skagway Rocks")]
coordmatch$SL.lat.min <- sites$Min.lat[which(sites$Location=="Slant Rock")]
# add max lat columns
coordmatch$AR.lat.max <- sites$Max.lat[which(sites$Location=="Anderson Rocks")]
coordmatch$BB.lat.max <- sites$Max.lat[which(sites$Location=="Bullman Beach")]
coordmatch$SB.lat.max <- sites$Max.lat[which(sites$Location=="South of Bodeltehs")]
coordmatch$CB.lat.max <- sites$Max.lat[which(sites$Location=="Chito Beach")]
coordmatch$DP.lat.max <- sites$Max.lat[which(sites$Location=="Duk Point")]
coordmatch$NB.lat.max <- sites$Max.lat[which(sites$Location=="North of Bodeltehs")]
coordmatch$FB.lat.max <- sites$Max.lat[which(sites$Location=="First Beach")]
coordmatch$KL.lat.max <- sites$Max.lat[which(sites$Location=="Koitlah")]
coordmatch$OI.lat.max <- sites$Max.lat[which(sites$Location=="Ozette Island")]
coordmatch$PH.lat.max <- sites$Max.lat[which(sites$Location=="Portage Head")]
coordmatch$SR.lat.max <- sites$Max.lat[which(sites$Location=="Sail River")]
coordmatch$SS.lat.max <- sites$Max.lat[which(sites$Location=="Seal and Sail Rocks")]
coordmatch$SK.lat.max <- sites$Max.lat[which(sites$Location=="Skagway Rocks")]
coordmatch$SL.lat.max <- sites$Max.lat[which(sites$Location=="Slant Rock")]
# assign site name if between range
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$AR.lat.min, coordmatch$AR.lat.max))] <- "Anderson Rocks"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$BB.lat.min, coordmatch$BB.lat.max))] <- "Bullman Beach"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$SB.lat.min, coordmatch$SB.lat.max))] <- "South of Bodeltehs"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$CB.lat.min, coordmatch$CB.lat.max))] <- "Chito Beach"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$DP.lat.min, coordmatch$DP.lat.max))] <- "Duk Point"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$NB.lat.min, coordmatch$NB.lat.max))] <- "North of Bodeltehs"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$FB.lat.min, coordmatch$FB.lat.max))] <- "First Beach"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$KL.lat.min, coordmatch$KL.lat.max))] <- "Koitlah"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$OI.lat.min, coordmatch$OI.lat.max))] <- "Ozette Island"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$PH.lat.min, coordmatch$PH.lat.max))] <- "Portage Head"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$SR.lat.min, coordmatch$SR.lat.max))] <- "Sail River"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$SS.lat.min, coordmatch$SS.lat.max))] <- "Seal and Sail Rocks"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$SK.lat.min, coordmatch$SK.lat.max))] <- "Skagway Rocks"
coordmatch$site.lat[which(between(coordmatch$lat, coordmatch$SL.lat.min, coordmatch$SL.lat.max))] <- "Slant Rock"
# add min long columns
coordmatch$AR.long.min <- sites$Min.long[which(sites$Location=="Anderson Rocks")]
coordmatch$BB.long.min <- sites$Min.long[which(sites$Location=="Bullman Beach")]
coordmatch$SB.long.min <- sites$Min.long[which(sites$Location=="South of Bodeltehs")]
coordmatch$CB.long.min <- sites$Min.long[which(sites$Location=="Chito Beach")]
coordmatch$DP.long.min <- sites$Min.long[which(sites$Location=="Duk Point")]
coordmatch$NB.long.min <- sites$Min.long[which(sites$Location=="North of Bodeltehs")]
coordmatch$FB.long.min <- sites$Min.long[which(sites$Location=="First Beach")]
coordmatch$KL.long.min <- sites$Min.long[which(sites$Location=="Koitlah")]
coordmatch$OI.long.min <- sites$Min.long[which(sites$Location=="Ozette Island")]
coordmatch$PH.long.min <- sites$Min.long[which(sites$Location=="Portage Head")]
coordmatch$SR.long.min <- sites$Min.long[which(sites$Location=="Sail River")]
coordmatch$SS.long.min <- sites$Min.long[which(sites$Location=="Seal and Sail Rocks")]
coordmatch$SK.long.min <- sites$Min.long[which(sites$Location=="Skagway Rocks")]
coordmatch$SL.long.min <- sites$Min.long[which(sites$Location=="Slant Rock")]
# add max long columns
coordmatch$AR.long.max <- sites$Max.long[which(sites$Location=="Anderson Rocks")]
coordmatch$BB.long.max <- sites$Max.long[which(sites$Location=="Bullman Beach")]
coordmatch$SB.long.max <- sites$Max.long[which(sites$Location=="South of Bodeltehs")]
coordmatch$CB.long.max <- sites$Max.long[which(sites$Location=="Chito Beach")]
coordmatch$DP.long.max <- sites$Max.long[which(sites$Location=="Duk Point")]
coordmatch$NB.long.max <- sites$Max.long[which(sites$Location=="North of Bodeltehs")]
coordmatch$FB.long.max <- sites$Max.long[which(sites$Location=="First Beach")]
coordmatch$KL.long.max <- sites$Max.long[which(sites$Location=="Koitlah")]
coordmatch$OI.long.max <- sites$Max.long[which(sites$Location=="Ozette Island")]
coordmatch$PH.long.max <- sites$Max.long[which(sites$Location=="Portage Head")]
coordmatch$SR.long.max <- sites$Max.long[which(sites$Location=="Sail River")]
coordmatch$SS.long.max <- sites$Max.long[which(sites$Location=="Seal and Sail Rocks")]
coordmatch$SK.long.max <- sites$Max.long[which(sites$Location=="Skagway Rocks")]
coordmatch$SL.long.max <- sites$Max.long[which(sites$Location=="Slant Rock")]
# assign site name if between range
coordmatch$site.long[which(between(coordmatch$long, coordmatch$AR.long.min, coordmatch$AR.long.max))] <- "Anderson Rocks"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$BB.long.min, coordmatch$BB.long.max))] <- "Bullman Beach"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$SB.long.min, coordmatch$SB.long.max))] <- "South of Bodeltehs"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$CB.long.min, coordmatch$CB.long.max))] <- "Chito Beach"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$DP.long.min, coordmatch$DP.long.max))] <- "Duk Point"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$NB.long.min, coordmatch$NB.long.max))] <- "North of Bodeltehs"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$FB.long.min, coordmatch$FB.long.max))] <- "First Beach"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$KL.long.min, coordmatch$KL.long.max))] <- "Koitlah"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$OI.long.min, coordmatch$OI.long.max))] <- "Ozette Island"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$PH.long.min, coordmatch$PH.long.max))] <- "Portage Head"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$SR.long.min, coordmatch$SR.long.max))] <- "Sail River"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$SS.long.min, coordmatch$SS.long.max))] <- "Seal and Sail Rocks"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$SK.long.min, coordmatch$SK.long.max))] <- "Skagway Rocks"
coordmatch$site.long[which(between(coordmatch$long, coordmatch$SL.long.min, coordmatch$SL.long.max))] <- "Slant Rock"
# assign all "errors" to "None"
coordmatch$r.site <- "Not Assigned" # both assigned but not to matching sites
coordmatch$r.site[which(coordmatch$site.lat == "Error")] <- "None"
coordmatch$r.site[which(coordmatch$site.long == "Error2")] <- "None"
# assign ones that matched
coordmatch$r.site[which(coordmatch$site.lat == coordmatch$site.long)] <- 
  coordmatch$site.lat[which(coordmatch$site.lat == coordmatch$site.long)]
# look at Not Assigned rows
problems <- coordmatch[which(coordmatch$r.site == "Not Assigned"),]
# with 300m square, 28 with conflicting assignations, 184 with None
# 13 of those 28 are actually far from each other
# The other 15 are close calls that might be encompassed with a larger buffer area
# moving forward without assigning them for now
# pull out version of coordmatch with useful columns
site.assigned <- coordmatch[, c("Date", "Date_S", "lat", "long", "r.site")]
# create column of date_site
site.assigned$Date_site <- paste(site.assigned$Date, site.assigned$r.site, sep = "_")
# pull out full.whales useful columns
whale.info <- full.whales[, c("Date", "Date_S", "Group_Beh", "IDs", "feed.IDs", "dailyID", "feed.dailyID", "Region")]
# merge with site.assigned
whales.with.sites <- merge(site.assigned, whale.info, all = T)
# merge with mysid info using Date_site
whales.and.mysids <- merge(whales.with.sites, data.simple, all.y = T, all.x = F)
# make whale NAs 0s
whales.and.mysids$IDs[which(is.na(whales.and.mysids$IDs))] <- 0
whales.and.mysids$feed.IDs[which(is.na(whales.and.mysids$feed.IDs))] <- 0
whales.and.mysids$dailyID[which(is.na(whales.and.mysids$dailyID))] <- 0
whales.and.mysids$feed.dailyID[which(is.na(whales.and.mysids$feed.dailyID))] <- 0

# sightings where site was assigned
sightings.300m <- coordmatch$sighting[which(coordmatch$site.lat == coordmatch$site.long)]
# only 27 at the moment...
# pull out rows of full.whales that are at-site sightings
at.sites <- full.whales[which(full.whales$Date_S %in% sightings.300m),]
# only feeding or no behavior - 
behaviors <- c("Feeding", "")
feed.at.sites <- at.sites[which(at.sites$Group_Beh %in% behaviors),]
# only 19 sightings then
# Pull out only feeding whales from full.whales
feeding <- full.whales[which(full.whales$Group_Beh %in% behaviors),]
# 184 sightings

