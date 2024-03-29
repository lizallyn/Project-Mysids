# Mysid Analysis 4 - daily summaries
# separated from full analysis file on Sept 13, 2023

# read in data

full.whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Data%20Files/Whales%20in%20full%20survey%20area%202019%202020.csv")
whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Data%20Files/whales%20per%20day%20for%20R.csv")
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Data%20Files/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")
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

# check Region real quick
m.feed.rand <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size) + scale(mysids) + (1|Region), 
                         family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.rand)
m.feed.norand <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size) + scale(mysids), 
                       family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.norand)
library(lmtest)
lmtest::lrtest(m.feed.rand, m.feed.norand)
# doesn't favor having Region as a random effect...including anyway here...

m.reg.feed.mysids <- glmmTMB(data = daily, reg.feed.IDs ~ scale(mysids) + (1|Region), 
                             family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.mysids)
fixef(m.reg.feed.mysids)

m.reg.feed.size <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size),
                             family = truncated_nbinom1, ziformula = ~.)
# summary(m.reg.feed.size)
# fixef(m.reg.feed.size)
# probably complete separation, only NAs for a category?
checking.separation <- daily %>%
  group_by(Region) %>%
  summarize(avg.size <- mean(size, na.rm = T),
            avg.mysids <- mean(mysids),
            avg.whales <- mean(reg.feed.IDs))
# doesn't look like it?


m.reg.feed.ms <- glmmTMB(data = daily, reg.feed.IDs ~ scale(size) + scale(mysids) + (1|Region), 
                         family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.ms)
ranef(m.reg.feed.ms)
fixef(m.reg.feed.ms)
confint(m.reg.feed.ms)

m.reg.feed.0 <- glmmTMB(data = daily, reg.feed.IDs ~ 1 + (1|Region), 
                        family = truncated_nbinom1, ziformula = ~.)
summary(m.reg.feed.0)

# Plot predictions
# size.range <- data.frame(size = 4:14)
# plot(size.range$size, predict(object = m.reg.feed.size, type = "response", 
#                               newdata = size.range))
# definitely not for use predicting beyond size ranges observed
mys.abundances <- data.frame(mysids = seq(0, 4000, 100), Region = "East Strait")
plot(mys.abundances$mysids, predict(object = m.reg.feed.mysids,
                                    type = "response", newdata = mys.abundances))
# interesting, predicts fewer whales for 0 mysids than for many, still messy
mys.input <- data.frame(mysids = seq(0, 4000, 100), Region = "East Strait", size = 14)
plot(mys.input$mysids, predict(object = m.reg.feed.ms,
                               type = "response", newdata = mys.input))

## AICc model selection
library(wiqid) # for AICc
rows <- c("Mysids + Size", "Mysids", "Null")
columns <- c("AIC", "delta", "weight")
AICc.feed.area <-  data.frame(matrix(nrow = 3, ncol = 3, data = c(wiqid::AICc(m.reg.feed.ms),
                                                                  wiqid::AICc(m.reg.feed.mysids),
                                                                  wiqid::AICc(m.reg.feed.0)), 
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
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 500, 100), Region = "East Strait")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 500, 100), Region = "East Strait")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 500, 100), Region = "East Strait")
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
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 500, 100), Region = "West Strait")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 500, 100), Region = "West Strait")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 500, 100), Region = "West Strait")
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
mys.input.4 <- data.frame(size = 4, mysids = seq(0, 500, 100), Region = "Ocean")
mys.input.9 <- data.frame(size = 9, mysids = seq(0, 500, 100), Region = "Ocean")
mys.input.14 <- data.frame(size = 14, mysids = seq(0, 500, 100), Region = "Ocean")
lines(mys.input.4$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.4), 
      col = "thistle2", lwd = 2)
lines(mys.input.9$mysids, predict(object = m.reg.feed.ms,
                                  type = "response", newdata = mys.input.9), 
      col = "orchid1", lwd = 2)
lines(mys.input.14$mysids, predict(object = m.reg.feed.ms,
                                   type = "response", newdata = mys.input.14), 
      col = "magenta2", lwd = 2)

plot(daily$mysids, daily$reg.feed.IDs, main = "Mysid Model")
lines(seq(0, 500, by = 100), predict(object = m.reg.feed.mysids, type = "response", 
                                     newdata = data.frame(mysids = seq(0, 500, 100), 
                                                          Region = "East Strait")),
      col = "palegreen1", lwd = 2)
lines(seq(0, 500, by = 100), predict(object = m.reg.feed.mysids, type = "response", 
                                     newdata = data.frame(mysids = seq(0, 500, 100), 
                                                          Region = "West Strait")),
      col = "green3", lwd = 2)
lines(seq(0, 500, by = 100), predict(object = m.reg.feed.mysids, type = "response", 
                                     newdata = data.frame(mysids = seq(0, 500, 100), 
                                                          Region = "Ocean")),
      col = "darkgreen", lwd = 2)

## Without Region as a random effect

# create daily summaries without region

## Avg Mysid size and daily summary
data$counter <- rep(1, nrow(data))
just.daily.mysids <- data %>%
  group_by(Date) %>%
  summarize(n.tows <- sum(counter),
            mysids <- mean(MysidCount),
            size <- mean(Avg.length, na.rm = T))
colnames(just.daily.mysids) <- c("Date", "n.tows", "mysids", "size")
just.daily.mysids$size[which(is.nan(just.daily.mysids$size))] <- NA
## daily whale summaries
data$Date <- as.factor(data$Date)
sample.days <- levels(data$Date)
# pull out survey days when tows happened
whales.on.mysid.days <- slice(.data = full.whales, 
                              which(full.whales$Date %in% sample.days))
# summarize by day and region
whales.on.mysid.days$dailyID <- as.numeric(whales.on.mysid.days$feed.dailyID)
daily.whales <- whales.on.mysid.days %>%
  group_by(Date) %>%
  summarize(n.sights <- length(Date),
            IDs <- sum(IDs),
            feed.IDs <- sum(feed.IDs),
            daily.IDs <- mean(dailyID),
            daily.f.IDs <- mean(feed.dailyID))
colnames(daily.whales) <- c("Date", "N.Sights", "IDs", 
                                   "feed.IDs", "daily.IDs", 
                                   "daily.f.IDs")
## combine mysid and whale daily summaries
just.daily <- merge(x = just.daily.mysids, y = daily.whales, all.x = T)
# make NA whale region-days 0s
just.daily$N.Sights[which(is.na(just.daily$N.Sights))] <- 0
just.daily$IDs[which(is.na(just.daily$IDs))] <- 0
just.daily$feed.IDs[which(is.na(just.daily$feed.IDs))] <- 0
just.daily$daily.IDs[which(is.na(just.daily$daily.IDs))] <- 0
just.daily$daily.f.IDs[which(is.na(just.daily$daily.f.IDs))] <- 0

# input here is just daily whale summaries (just.daily$daily.f.IDs)

m.feed.mysids <- glmmTMB(data = just.daily, daily.f.IDs ~ scale(mysids), 
                             family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.mysids)
fixef(m.feed.mysids)

m.feed.size <- glmmTMB(data = just.daily, daily.f.IDs ~ scale(size), 
                           family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.size) 
fixef(m.feed.size)

m.feed.ms <- glmmTMB(data = just.daily, daily.f.IDs ~ scale(size) + scale(mysids), 
                         family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.ms)
ranef(m.feed.ms)
fixef(m.feed.ms)
confint(m.feed.ms)

m.feed.0 <- glmmTMB(data = just.daily, daily.f.IDs ~ 1, 
                        family = truncated_nbinom1, ziformula = ~.)
summary(m.feed.0)

# Plot predictions
size.range <- data.frame(size = 4:14)
plot(size.range$size, predict(object = m.feed.size, type = "response", 
                              newdata = size.range))
# definitely not for use predicting beyond size ranges observed
mys.abundances <- data.frame(mysids = seq(0, 4000, 100))
plot(mys.abundances$mysids, predict(object = m.feed.mysids,
                                    type = "response", newdata = mys.abundances))
# interesting, predicts fewer whales for 0 mysids than for many, still messy
mys.input <- data.frame(mysids = seq(0, 4000, 100), size = 9)
plot(mys.input$mysids, predict(object = m.feed.ms,
                               type = "response", newdata = mys.input))

## AICc model selection
library(wiqid) # for AICc
rows <- c("Mysids + Size", "Mysids", "Size", "Null")
columns <- c("AIC", "delta", "weight")
AICc.feed <-  data.frame(matrix(nrow = 4, ncol = 3, data = c(AICc(m.feed.ms),
                                                                  AICc(m.feed.mysids),
                                                                  AICc(m.feed.size),
                                                                  AICc(m.feed.0)), 
                                     dimnames = list(rows, columns)))
AICc.feed[,2] <- AICc.feed[,1] - min(AICc.feed)
AICc.feed[,3] <- exp(-0.5*AICc.feed[,2])/sum(exp(-0.5*AICc.feed[,2]))
AICc.feed

# Matching up east and west strait tows

