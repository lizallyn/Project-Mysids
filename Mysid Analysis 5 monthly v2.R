# Mysid Analysis 4 Monthly, Regionally
# version 2

## read in data
CRC <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/CRC%20IDs%20per%20sighting%20June%20-%20Nov%202019%202020%20mysid%20survey%20area%20only%20all%20behaviors.csv")
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")
all <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/All%20obs%20for%20R.csv")

# Packages
library(tidyr)
library(dplyr)

## Mysid summary

all$Date <- as.Date(as.character(all$Date), format="%Y%m%d")
all$Y_M <- format(all$Date, format = "%Y_%m")
all$Date <- format(all$Date, format="%Y%m%d")

# match column headings to data
all$Sample <- all$Assigned.ID
all$Region.3 <- all$Region

# length-weight from Burdi et al
constant <- 0.0000116
exponent <- 3.060
all$mysid. <- as.factor(all$mysid.)
mysids <- slice(.data = all, which(all$mysid. == "YES"))
mysids$length <- as.numeric(mysids$length)
mysids$weight <- constant*mysids$length^exponent

mysid.tow.summ <- mysids %>%
  group_by(Y_M, Region.3, Sample) %>%
  summarise(count = length(mysid.),
            biomass = sum(weight, na.rm = T))
which(is.na(mysids$weight))
# not many, should be fine to na.rm = T

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

# add weight column
data.bio <- merge(x = data, y = mysid.tow.summ, all.x = T)
# count column was just for verification, ignore, not fixing NAs to 0s
# fix NA biomass to 0
data.bio$biomass[which(is.na(data.bio$biomass))] <- 0

mys.region.month <- data.bio %>%
  group_by(Y_M, Region.2) %>%
  summarize(n.tows = length(Sample),
            mysids = mean(MysidCount, na.rm = T),
            size = mean(Avg.length, na.rm = T),
            biomass = mean(biomass))
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
CRC <- CRC[-which(is.na(CRC$CRC.ID)),]
# only feeding whales
feed.behaviors <- c("Feeding", "")
CRC.feed <- CRC[which(CRC$Group.Beh %in% feed.behaviors),]

# list crc ids per region per day
CRC.region.day <- CRC.feed %>%
  group_by(Date, Region.2, CRC.ID) %>%
  summarize(count = 1,
            n.sights = length(Date_S))
# # sum CRC IDs per region perday
IDs.region.day <- CRC.region.day %>%
  group_by(Date, Region.2) %>%
  summarize(IDs = sum(count),
            n.sights = sum(n.sights))
# format the date as yyyymmdd and extract Y_M into column
IDs.region.day$Date <- as.Date(as.character(IDs.region.day$Date), format="%Y%m%d")
IDs.region.day$Y_M <- format(IDs.region.day$Date, format = "%Y_%m")
IDs.region.day$Date <- format(IDs.region.day$Date, format="%Y%m%d")

# Monthly ID summaries
IDs.region.month <- IDs.region.day %>%
  group_by(Y_M, Region.2) %>%
  summarize(IDs = mean(IDs),
            n.sights = sum(n.sights))

## Merge Whale and Mysid Summaries
wm.regionYM <- merge(x = mys.region.month, y = IDs.region.month, all.x = T)
# NA whales and sightings to 0s
wm.regionYM$IDs[which(is.na(wm.regionYM$IDs))] <- 0
wm.regionYM$n.sights[which(is.na(wm.regionYM$n.sights))] <- 0
# NaN size to mean(size)
wm.regionYM$size[which(is.nan(wm.regionYM$size))] <- NA
# correct IDs per km surveyed
straitkm <- 27
oceankm <- 37
wm.regionYM$IDskm <- NA
wm.regionYM$IDskm[which(wm.regionYM$Region.2 == "Strait")] <- 
  wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Strait")]/straitkm
wm.regionYM$IDskm[which(wm.regionYM$Region.2 == "Ocean")] <- 
  wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Ocean")]/oceankm
# add year column
wm.regionYM$Year <- c(rep(2019, 8), rep(2020, 6))
# add Y_M_Reg column
wm.regionYM$Y_M_Reg <- paste(wm.regionYM$Y_M, wm.regionYM$Region.2)
# Region as factor
wm.regionYM$Region.2 <- as.factor(wm.regionYM$Region.2)

plot(wm.regionYM$mysids, wm.regionYM$IDskm)
plot(wm.regionYM$biomass, wm.regionYM$IDskm)
plot(wm.regionYM$size, wm.regionYM$IDskm)
hist(wm.regionYM$IDskm)


## Model Building and Diagnostics

library(lme4)
library(faraway)
library(car)
library(ggplot2)
library(AICcmodavg)
library(AER)

# lmer and lm attempts
ggplot(data = wm.regionYM) +
  geom_point(aes(x = mysids, y = IDskm, color = Region.2))
ggplot(data = wm.regionYM) +
  geom_point(aes(x = size, y = IDskm, color = Region.2))
ggplot(data = wm.regionYM) +
  geom_point(aes(x = biomass, y = IDskm, color = Region.2))
ggplot(data = wm.regionYM) +
  geom_point(aes(x = mysids, y = size, color = Region.2))
ggplot(data = wm.regionYM) +
  geom_point(aes(x = mysids, y = biomass, color = Region.2))

model4 <- lm(data = wm.regionYM, IDskm ~ scale(mysids) + scale(biomass) + Region.2)
summary(model4)
model5 <- lm(data = wm.regionYM, IDskm ~ scale(mysids) + Region.2)
summary(model5)
model6 <- lm(data = wm.regionYM, IDskm ~ scale(biomass) + Region.2)
summary(model6)
model7 <- lm(data = wm.regionYM, IDskm ~ scale(biomass) + scale(mysids))
summary(model7)
model8 <- lm(data = wm.regionYM, IDskm ~ Region.2)
summary(model8)
model9 <- lm(data = wm.regionYM, IDskm ~ scale(mysids))
summary(model9)
model10 <- lm(data = wm.regionYM, IDskm ~ scale(biomass))
summary(model10)
model11 <- lm(data = wm.regionYM, IDskm ~ 1)
summary(model11)
# Nothing is significant

rows <- c("Mysids + Biomass + Region", "Mysids + Region", "Biomass + Region", 
          "Mysids + Biomass", "Region", "Mysids", "Biomass", "Null")
columns <- c("AIC", "delta", "weight")
AICc.feed <-  data.frame(matrix(nrow = 8, ncol = 3, data = c(AICc(model4),
                                                             AICc(model5),
                                                             AICc(model6),
                                                             AICc(model7),
                                                             AICc(model8),
                                                             AICc(model9),
                                                             AICc(model10),
                                                             AICc(model11)), 
                                dimnames = list(rows, columns)))
AICc.feed[,2] <- AICc.feed[,1] - min(AICc.feed)
AICc.feed[,3] <- exp(-0.5*AICc.feed[,2])/sum(exp(-0.5*AICc.feed[,2]))
AICc.feed
# best model is the null model

ggplot(data = wm.regionYM) +
  geom_point(aes(x = biomass, y = IDskm, color = Region.2)) + 
  geom_line(aes(y = model11$coefficients, x = biomass), lwd = 1, color = "black") + 
  geom_line(aes(x = biomass, y = predict(model10, type = "response")), lwd = 1, color = "goldenrod")
# null model in black, model10 (biomass) in yellow

# run through diagnostics to make sure lm is ok, use model4 (all)

plot(fitted(model4), resid(model4))
# looks just like data, not being explained well at all...
qqnorm(model4$residuals)
qqline(model4$residuals)
# actually looks ok, squiggly, maybe left tailed a bit
model.test <- lm(I(sqrt(abs(resid(model4)))) ~ I(fitted(model4)))
sumary(model.test)
# slope not significant
acf(resid(model4))
# no autocorrelation
# check unusual observations
hv <- hatvalues(model4)
th <- 2 * (5 / length(hv))
which(hv > th)
# 10
cook <- cooks.distance(model4)
halfnorm(cook, 2, ylab="Cook’s Distance", labs = wm.regionYM$Y_M_Reg)
# 2020_07 Strait big biomass, 2020_09 Ocean zeroes across the board
# going to keep them in tho

### Recycle Bin

# write.csv(wm.regionYM,
#           "C:/Users/Elizabeth Allyn/Documents/GitHub/Project-Mysids/monthly_summ_whales_mysids.csv",
#           row.names = F)

# tobit interlude real quick
# tobit <- tobit(formula = IDskm ~ scale(mysids) * scale(size) + Region.2, data = wm.regionYM)
# summary(tobit)
# predict(tobit)
# glmm <- glmmTMB(formula = IDskm ~ scale(size) + scale(mysids) + (1|Region.2), 
#                 data = wm.regionYM, family = "gaussian", 
#                 ziformula = ~ scale(mysids) + (1|Region.2))
# summary(glmm)

# visualize
# by mysids
straitdata <- data.frame(mysids = seq(from = 0, to = 400, by = 25),
                         Region.2 = "Strait", 
                         size = 12)
oceandata <- data.frame(Region.2 = "Ocean", 
                        mysids = seq(from = 0, to = 400, by = 25),
                        size = 12)
plot(wm.regionYM$mysids[which(wm.regionYM$Region.2 == "Strait")], 
     wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Strait")], 
     main = "whales ~ mysids + size + (1|Region)", 
     sub = "Strait - pink, Ocean - blue, size = 12mm",
     ylab = "avg. IDs per region per day per month", 
     xlab = "avg. mysids per tow per region per month",
     pch = 19, col = "orchid3", ylim = c(0,10))
lines(straitdata$mysids, predict(object = model4,
                                 type = "response", newdata = straitdata), 
      col = "orchid2", lwd = 2)
points(wm.regionYM$mysids[which(wm.regionYM$Region.2 == "Ocean")], 
       wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Ocean")], pch = 19, col = "skyblue3")
lines(oceandata$mysids, predict(object = model4,
                                type = "response", newdata = oceandata), 
      col = "skyblue2", lwd = 2)
# by size
straitdata <- data.frame(mysids = 1000,
                         Region.2 = "Strait", 
                         size = 4:13)
oceandata <- data.frame(Region.2 = "Ocean", 
                        mysids = 1000,
                        size = 4:13)
plot(wm.regionYM$size[which(wm.regionYM$Region.2 == "Strait")], 
     wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Strait")], 
     main = "whales ~ mysids + size + (1|Region)", 
     sub = "Strait - pink, Ocean - blue, mysids = 1000",
     ylab = "avg. IDs per region per day per month", 
     xlab = "avg. msize in mm per region per month",
     pch = 19, col = "orchid3", ylim = c(0,15))
lines(straitdata$size, predict(object = model4,
                                 type = "response", newdata = straitdata), 
      col = "orchid2", lwd = 2)
points(wm.regionYM$size[which(wm.regionYM$Region.2 == "Ocean")], 
       wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Ocean")], pch = 19, col = "skyblue3")
lines(oceandata$size, predict(object = model4,
                                type = "response", newdata = oceandata), 
      col = "skyblue2", lwd = 2)
