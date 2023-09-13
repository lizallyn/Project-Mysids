# Mysid Analysis 4 - daily summaries


# read in data

full.whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Whales%20in%20full%20survey%20area%202019%202020.csv")
whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/whales%20per%20day%20for%20R.csv")
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")
# Pull out useful clean mysid data columns to simplify data frame
data <- data.full[,c(1,2,4:6,7,11,17:25,30,31)]
data$Site <- factor(data$Site, 
                    levels = c("Chito Beach", "Bullman Beach", "Seal And Sail",
                               "Sail River", "First Beach", "Koitlah", 
                               "Slant Rock", "Skagway", "Anderson Rocks", 
                               "Portage Head", "Duk Point", "North of Bodelteh Islands", 
                               "South of Bodelteh Islands", "Ozette Island"))

# Packages
library(tidyr)
library(dplyr)

# this sighting is on land, use sight end coords instead
full.whales$Start_Dec_Lat[which(full.whales$Date_S == "20191107_15")] <- 48.34167
full.whales$Start_Dec_Long[which(full.whales$Date_S == "20191107_15")] <- -124.7105

# Assign whale sightings to a region

ES.Eastof <- -124.6008
WS.Eastof <- -124.726
WS.Northof <- 48.37437
O.Southof <- 48.38615
O.Westof <- -124.6529
# assign regions
full.whales$region <- "Error"
full.whales$region[full.whales$Start_Dec_Long > ES.Eastof] <- "East Strait"
full.whales$region[between(full.whales$Start_Dec_Long, left = WS.Eastof, 
                           right = ES.Eastof) & 
                     full.whales$Start_Dec_Lat > WS.Northof] <- "West Strait"
full.whales$region[which(full.whales$Start_Dec_Long < O.Westof & 
                           full.whales$Start_Dec_Lat < O.Southof)] <- "Ocean"

## Avg Mysid size and daily summary
data$counter <- rep(1, nrow(data))
daily.mysids <- data %>%
  group_by(Date, Region) %>%
  summarize(n.tows <- sum(counter),
            mysids <- mean(MysidCount),
            size <- mean(Avg.length, na.rm = T))
colnames(daily.mysids) <- c("Date", "Region", "n.tows", "mysids", "size")
daily.mysids$Date_R <- paste(daily.mysids$Date, daily.mysids$Region, sep = "_")
daily.mysids$size[which(is.nan(daily.mysids$size))] <- NA

## daily whale summaries
data$Date <- as.factor(data$Date)
sample.days <- levels(data$Date)
# pull out survey days when tows happened
whales.on.mysid.days <- slice(.data = full.whales, 
                              which(full.whales$Date %in% sample.days))
# summarize by day and region
whales.on.mysid.days$dailyID <- as.numeric(whales.on.mysid.days$dailyID)
daily.region.whales <- whales.on.mysid.days %>%
  group_by(Date, region) %>%
  summarize(n.sights <- length(Date),
            reg.IDs <- sum(IDs),
            reg.feed.IDs <- sum(feed.IDs),
            reg.daily.IDs <- mean(dailyID),
            reg.daily.f.IDs <- mean(feed.dailyID))
colnames(daily.region.whales) <- c("Date", "Region", "N.Sights", "reg.IDs", 
                                   "reg.feed.IDs", "daily.IDs", 
                                   "daily.f.IDs")
daily.region.whales$Date_R <- paste(daily.region.whales$Date, 
                                    daily.region.whales$Region, sep = "_")


## combine mysid and whale daily summaries
daily <- merge(x = daily.mysids, y = daily.region.whales, all.x = T)
# make NA whale region-days 0s
daily$N.Sights[which(is.na(daily$N.Sights))] <- 0
daily$reg.IDs[which(is.na(daily$reg.IDs))] <- 0
daily$reg.feed.IDs[which(is.na(daily$reg.feed.IDs))] <- 0
daily$daily.IDs[which(is.na(daily$daily.IDs))] <- 0
daily$daily.f.IDs[which(is.na(daily$daily.f.IDs))] <- 0

plot(daily$mysids, daily$reg.feed.IDs)
plot(daily$size, daily$reg.feed.IDs)
plot(daily$mysids, daily$daily.f.IDs)
plot(daily$size, daily$daily.f.IDs)

### ok now some models
# this time with avg mysid size
# just regional unique IDs, feeding whales only
# sightings from mysid survey area only
# just tackling the daily first
library(glmmTMB)

hist(daily$reg.feed.IDs)
# use same model structure

m.reg.feed.mysids <- glmmTMB(data = daily, reg.feed.IDs ~ scale(mysids) + (1|Region), 
                             family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.mysids)
fixef(m.reg.feed.mysids)
m.reg.feed.size <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size), 
                           family = truncated_nbinom1, ziformula = ~.)
# not converging properly unless size is scaled and reg removed
summary(m.reg.feed.size) 
fixef(m.reg.feed.size)
m.reg.feed.ms <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size) + scale(mysids) + (1|Region), 
                         family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.ms)
ranef(m.reg.feed.ms)
fixef(m.reg.feed.ms)
confint(m.reg.feed.ms)

m.reg.feed.0 <- glmmTMB(data = daily, reg.feed.IDs ~ 1, 
                        family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.0)

# Plot predictions
size.range <- data.frame(size = 4:14)
plot(size.range$size, predict(object = m.reg.feed.size, type = "response", 
                              newdata = size.range))
# definitely not for use predicting beyond size ranges observed
mys.abundances <- data.frame(mysids = seq(0, 4000, 100), Region = "East Strait")
plot(mys.abundances$mysids, predict(object = m.reg.feed.mysids,
                                    type = "response", newdata = mys.abundances))
# interesting, predicts fewer whales for 0 mysids than for many, still messy
mys.input <- data.frame(mysids = seq(0, 4000, 100), Region = "East Strait", size = 14)
plot(mys.input$mysids, predict(object = m.reg.feed.ms,
                               type = "response", newdata = mys.input))
size.input <- data.frame(size = 4:14, mysids = 10, Region = "Ocean")
plot(size.input$size, predict(object = m.reg.feed.ms,
                              type = "response", newdata = size.input), main = "East Strait")
# the messiness goes away when size = bigger? just an increase with abundance?

## AICc model selection
library(wiqid) # for AICc
rows <- c("Mysids + Size", "Mysids", "Null")
columns <- c("AIC", "delta", "weight")
AICc.feed.area <-  data.frame(matrix(nrow = 3, ncol = 3, data = c(AICc(m.reg.feed.ms),
                                                                  AICc(m.reg.feed.mysids),
                                                                  AICc(m.reg.feed.0)), 
                                     dimnames = list(rows, columns)))
AICc.feed.area[,2] <- AICc.feed.area[,1] - min(AICc.feed.area)
AICc.feed.area[,3] <- exp(-0.5*AICc.feed.area[,2])/sum(exp(-0.5*AICc.feed.area[,2]))
AICc.feed.area
# prefers mysids + size, but not comparing to a size only model right now

# More Exploration Plots

# Mysid size gradients per region
# East Strait
plot(daily$size[which(daily$Region == "East Strait")], daily$reg.feed.IDs[which(daily$Region == "East Strait")], main = "East Strait")
size.input.10 <- data.frame(size = 4:14, mysids = 10, Region = "East Strait")
size.input.100 <- data.frame(size = 4:14, mysids = 100, Region = "East Strait")
size.input.1000 <- data.frame(size = 4:14, mysids = 1000, Region = "East Strait")
lines(size.input.10$size, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = size.input.10), 
      col = "lightblue2", lwd = 2)
lines(size.input.100$size, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = size.input.100), 
      col = "skyblue2", lwd = 2)
lines(size.input.1000$size, predict(object = m.reg.feed.ms,
                                    type = "response", newdata = size.input.1000), 
      col = "dodgerblue3", lwd = 2)
# west Strait
plot(daily$size[which(daily$Region == "West Strait")], daily$reg.feed.IDs[which(daily$Region == "West Strait")], main = "West Strait")
size.input.10 <- data.frame(size = 4:14, mysids = 10, Region = "West Strait")
size.input.100 <- data.frame(size = 4:14, mysids = 100, Region = "West Strait")
size.input.1000 <- data.frame(size = 4:14, mysids = 1000, Region = "West Strait")
lines(size.input.10$size, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = size.input.10), 
      col = "lightblue2", lwd = 2)
lines(size.input.100$size, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = size.input.100), 
      col = "skyblue2", lwd = 2)
lines(size.input.1000$size, predict(object = m.reg.feed.ms,
                                    type = "response", newdata = size.input.1000), 
      col = "dodgerblue3", lwd = 2)
# ocean
plot(daily$size[which(daily$Region == "Ocean")], daily$reg.feed.IDs[which(daily$Region == "Ocean")], main = "Ocean")
size.input.10 <- data.frame(size = 4:14, mysids = 10, Region = "Ocean")
size.input.100 <- data.frame(size = 4:14, mysids = 100, Region = "Ocean")
size.input.1000 <- data.frame(size = 4:14, mysids = 1000, Region = "Ocean")
lines(size.input.10$size, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = size.input.10), 
      col = "lightblue2", lwd = 2)
lines(size.input.100$size, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = size.input.100), 
      col = "skyblue2", lwd = 2)
lines(size.input.1000$size, predict(object = m.reg.feed.ms,
                                    type = "response", newdata = size.input.1000), 
      col = "dodgerblue3", lwd = 2)

# Mysid abundance gradients per region
# East Strait
plot(daily$mysids[which(daily$Region == "East Strait")], daily$reg.feed.IDs[which(daily$Region == "East Strait")], main = "East Strait")
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 4000, 100), Region = "East Strait")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 4000, 100), Region = "East Strait")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 4000, 100), Region = "East Strait")
lines(mys.input.4$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.4), 
      col = "thistle2", lwd = 2)
lines(mys.input.9$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.9), 
      col = "orchid1", lwd = 2)
lines(mys.input.14$mysids, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = mys.input.14), 
      col = "magenta2", lwd = 2)
# west Strait
plot(daily$mysids[which(daily$Region == "West Strait")], daily$reg.feed.IDs[which(daily$Region == "West Strait")], main = "West Strait")
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 4000, 100), Region = "West Strait")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 4000, 100), Region = "West Strait")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 4000, 100), Region = "West Strait")
lines(mys.input.4$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.4), 
      col = "thistle2", lwd = 2)
lines(mys.input.9$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.9), 
      col = "orchid1", lwd = 2)
lines(mys.input.14$mysids, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = mys.input.14), 
      col = "magenta2", lwd = 2)
# ocean
plot(daily$mysids[which(daily$Region == "Ocean")], daily$reg.feed.IDs[which(daily$Region == "Ocean")], main = "Ocean")
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 4000, 100), Region = "Ocean")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 4000, 100), Region = "Ocean")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 4000, 100), Region = "Ocean")
lines(mys.input.4$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.4), 
      col = "thistle2", lwd = 2)
lines(mys.input.9$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.9), 
      col = "orchid1", lwd = 2)
lines(mys.input.14$mysids, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = mys.input.14), 
      col = "magenta2", lwd = 2)
