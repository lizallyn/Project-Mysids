# Mysid Analysis 4 Monthly

## read in data
CRC <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/CRC%20IDs%20per%20sighting%20June%20-%20Nov%202019%202020%20mysid%20survey%20area%20only%20all%20behaviors.csv")

# remove blank IDs
CRC <- CRC[-which(is.na(CRC$CRC.ID)),]

# only feeding whales
feed.behaviors <- c("Feeding", "")
CRC.feed <- CRC[which(CRC$Group.Beh %in% feed.behaviors),]

# list crc ids per region per day
CRC.region.day <- CRC.feed %>%
  group_by(Date, Region, CRC.ID) %>%
  summarize(count <- 1)
colnames(CRC.region.day) <- c("Date", "Region", "CRC.ID", "count")
# sum CRC IDs per region perday
IDs.region.day <- CRC.region.day %>%
  group_by(Date, Region) %>%
  summarize(IDs <- sum(count))
colnames(IDs.region.day) <- c("Date", "Region", "IDs")

# format the date as yyyymmdd and extract Y_M into column
IDs.region.day$Date <- as.Date(as.character(IDs.region.day$Date), format="%Y%m%d")
IDs.region.day$Y_M <- format(IDs.region.day$Date, format = "%Y_%m")
IDs.region.day$Date <- format(IDs.region.day$Date, format="%Y%m%d")

# Monthly ID summaries
IDs.region.month <- IDs.region.day %>%
  group_by(Region, Y_M) %>%
  