# Mysid Analysis
# composite ZI NBin with size only in the non-zero data

## read in data
CRC <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/CRC%20IDs%20per%20sighting%20June%20-%20Nov%202019%202020%20mysid%20survey%20area%20only%20all%20behaviors.csv")
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")

# Packages
library(tidyr)
library(dplyr)

## Mysid summary

# Pull out useful clean mysid data columns to simplify data frame
data <- data.full[,c(1,2,4:6,7,11,17:25,30,31)]
data$Site <- factor(data$Site, 
                    levels = c("Chito Beach", "Bullman Beach", "Seal And Sail",
                               "Sail River", "First Beach", "Koitlah", 
                               "Slant Rock", "Skagway", "Anderson Rocks", 
                               "Portage Head", "Duk Point", "North of Bodelteh Islands", 
                               "South of Bodelteh Islands", "Ozette Island"))
# format date and make Y_M column
data$Date <- as.Date(as.character(data$Date), format="%Y%m%d")
data$Y_M <- format(data$Date, format = "%Y_%m")
# 20200902 is paired with 20200824 - assign it to 2020_08
data$Y_M[which(data$Date == 20200902)] <- "2020_08"
# back to character format
data$Date <- format(data$Date, format="%Y%m%d")

# second region column for combined Strait category
no.pair.days <- c("20190603", "20190709", "20190830", "20200626")
straits <- c("East Strait", "West Strait")
data$Region.2 <- NA
data$Region.2[which(data$Region == "Ocean")] <- "Ocean"
data$Region.2[which(data$Region %in% straits)] <- "Strait"
data$Region.2[which(data$Date %in% no.pair.days)] <- "Not Complete"

mys.region.month <- data %>%
  group_by(Y_M, Region.2) %>%
  summarize(n.tows <- length(Sample),
            mysids <- mean(MysidCount, na.rm = T),
            size <- mean(Avg.length, na.rm = T))
colnames(mys.region.month) <- c("Y_M", "Region.2", "n.tows", "mysids", "size")
mys.region.month <- mys.region.month[which(mys.region.month$Region.2 != "Not Complete"),]

## Whale summary

# Assign whale sightings to a region
ES.Eastof <- -124.6008
WS.Eastof <- -124.726
WS.Northof <- 48.37437
O.Southof <- 48.38615
O.Westof <- -124.6529
# assign regions
CRC$Region <- "Error"
CRC$Region[CRC$Start.Dec.Long > ES.Eastof] <- "East Strait"
CRC$Region[between(CRC$Start.Dec.Long, left = WS.Eastof, 
                   right = ES.Eastof) & 
             CRC$Start.Dec.Lat > WS.Northof] <- "West Strait"
CRC$Region[which(CRC$Start.Dec.Long < O.Westof & 
                   CRC$Start.Dec.Lat < O.Southof)] <- "Ocean"
# assign Region.2
CRC$Region.2 <- NA
CRC$Region.2[which(CRC$Region %in% straits)] <- "Strait"
CRC$Region.2[which(CRC$Region == "Ocean")] <- "Ocean"
CRC$Region.2 <- as.factor(CRC$Region.2)

# remove blank IDs
CRC.1 <- CRC[-which(is.na(CRC$CRC.ID)),]
# only feeding whales
feed.behaviors <- c("Feeding", "")
CRC.feed <- CRC.1[which(CRC.1$Group.Beh %in% feed.behaviors),]

# format the date as yyyymmdd and extract Y_M into column
CRC.feed$Date <- as.Date(as.character(CRC.feed$Date), format="%Y%m%d")
CRC.feed$Month <- format(CRC.feed$Date, format = "%m")
CRC.feed$Year <- format(CRC.feed$Date, format = "%Y")
CRC.feed$Y_M <- format(CRC.feed$Date, format = "%Y_%m")
CRC.feed$Date <- format(CRC.feed$Date, format="%Y%m%d")

# list crc ids per region per month
CRC.region.mon <- CRC.feed %>%
  group_by(Y_M, Region.2, CRC.ID) %>%
  summarize(count = 1,
            n.sights = length(Date_S))
# Monthly ID summaries
IDs.region.mon <- CRC.region.mon %>%
  group_by(Y_M, Region.2) %>%
  summarize(IDs = sum(count),
            n.sights = sum(n.sights))


