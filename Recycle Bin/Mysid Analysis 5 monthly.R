# Mysid Analysis 4 Monthly, Regionally

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
# NaN size to NAs
wm.regionYM$size[which(is.nan(wm.regionYM$size))] <- NA
# add year column
wm.regionYM$Year <- c(rep(2019, 8), rep(2020, 6))
# add Y_M_Reg column
wm.regionYM$Y_M_Reg <- paste(wm.regionYM$Y_M, wm.regionYM$Region.2)

plot(wm.regionYM$mysids, wm.regionYM$IDs)
plot(wm.regionYM$size, wm.regionYM$IDs)
hist(wm.regionYM$IDs, breaks = c(-1:12))
# checking to see if we can just use lmer

## Diagnostics

library(lme4)
library(faraway)
library(car)

# Initial model
wm.regionYM$Region.2 <- as.factor(wm.regionYM$Region.2)
model1 <- lm(data = wm.regionYM, IDs ~
                    scale(mysids) + scale(size) + Region.2)
summary(model1)

# check residuals - constant variance
plot(model1$fitted, model1$residuals, xlab = "Fitted Values", 
     ylab = "Residuals",ylim = c(-max(model1$residuals)-1,
                                 max(model1$residuals)+1), pch = 19)
# errors heteroscedactic, though there's a funky gap in the middle
plot(model1$fitted, sqrt(abs(model1$residuals)), xlab = "Fitted Values", 
     ylab = "Residuals", pch = 19)
model.test <- lm(I(sqrt(abs(model1$residuals))) ~ I(model1$fitted))
sumary(model.test)
# slope not significant? so maybe ok?
error <- model1$residuals
leveneTest(error ~ Region.2, data = wm.regionYM)
# seems like somehow our variance is homo? Even tho it doesn't look it??

# check qq - normality
qqnorm(residuals(model1))
qqline(residuals(model1))
# looks kinda ok
# maybe a lttle left-skewed if anyting, but not a lot
# maybe bootstrap CIs anyway to be safe

# check independent errors
acf(resid(model1))
# no autocorrelation of errors

# check unusual observations
hv <- hatvalues(model1)
th <- 2 * (5 / length(hv))
which(hv > th)
# none
# don't expect outliers
plot(wm.regionYM$mysids, wm.regionYM$IDs)
cook <- cooks.distance(model1)
halfnorm(cook, 2, ylab="Cook’s Distance", labs = wm.regionYM$Y_M)
# unusual obs are 2020_07, really big mysids

# try with random effect Region.2
library(ggplot2)
ggplot(data = wm.regionYM) +
  geom_point(aes(x = mysids, y = IDs, color = Region.2))
model2 <- lmer(data = wm.regionYM, IDs ~ scale(mysids) + (1|Region.2))
model3 <- lmer(data = wm.regionYM, IDs ~ 1 + (mysids|Region.2))
model4 <- lmer(data = wm.regionYM, IDs ~ scale(mysids) + scale(size) + (1|Region.2))
# use this full one I think (model4)
summary(model4)
ranef(model2)
fixef(model2)
confint(model2)

plot(fitted(model4), resid(model4))
# ok, still weird gap in the middle tho
qqnorm(unlist(ranef(model4)$Region.2),main = "Random Effects")
abline(0, 1)
# not sure how a random effect with only two groups can be normal?
model.test <- lm(I(sqrt(abs(resid(model4)))) ~ I(fitted(model4)))
sumary(model.test)
# slope not significant
acf(resid(model4))
# no autocorrelation
# check unusual observations
hv <- hatvalues(model4)
th <- 2 * (5 / length(hv))
which(hv > th)
# none
cook <- cooks.distance(model4)
halfnorm(cook, 2, ylab="Cook’s Distance", labs = wm.regionYM$Y_M_Reg)
# 2020_07, 2019_11 big mysids or large whales

# visualize
straitdata <- data.frame(mysids = seq(from = 0, to = 400, by = 25),
                         Region.2 = "Strait", 
                         size = 9)
oceandata <- data.frame(Region.2 = "Ocean", 
                        mysids = seq(from = 0, to = 400, by = 25),
                        size = 9)
plot(wm.regionYM$mysids[which(wm.regionYM$Region.2 == "Strait")], 
     wm.regionYM$IDs[which(wm.regionYM$Region.2 == "Strait")], 
     main = "whales ~ mysids + size + (1|Region)", sub = "Strait - pink, Ocean - blue, size = 9mm",
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







##### need to figure out the NA size values - add overall average??
summary(m.region.ym.size)
plot(wm.regionYM$size, predict(m.region.ym.size, type = "response"))

## If we stick with count data, see below

# format the date as yyyymmdd and extract Y_M into column
CRC$Date <- as.Date(as.character(CRC$Date), format="%Y%m%d")
CRC$Y_M <- format(CRC$Date, format = "%Y_%m")
CRC$Date <- format(CRC$Date, format="%Y%m%d")

# try one that is total IDs per Y_M region
CRC.regionYM <- CRC.feed %>%
  group_by(Y_M, Region.2, CRC.ID) %>%
  summarize(count = 1,
            n.sights = length(Date_S))
IDs.regionYM <- CRC.regionYM %>%
  group_by(Y_M, Region.2) %>%
  summarize(IDs = sum(count),
            n.sights = sum(n.sights))

## Merge Whale and Mysid Summaries
wm.regionYM <- merge(x = mys.region.month, y = IDs.regionYM, all.x = T)
# NA whales and sightings to 0s
wm.regionYM$IDs[which(is.na(wm.regionYM$IDs))] <- 0
wm.regionYM$n.sights[which(is.na(wm.regionYM$n.sights))] <- 0
# NaN size to NAs
wm.regionYM$size[which(is.nan(wm.regionYM$size))] <- NA

# look at it
plot(wm.regionYM$mysids, wm.regionYM$IDs)
plot(wm.regionYM$size, wm.regionYM$IDs)

## Models

hist(wm.regionYM$IDs, breaks = c(-1:20))
# maybe not zero-inflated anymore?
library(lme4)
# nonzero <- wm.region.ym[which(wm.region.ym$IDs != 0),]
m.pois <- glmer(IDs ~ scale(mysids) + scale(size) + (1|Region.2), data = wm.regionYM, 
                  family = "poisson")
pchisq(sum(residuals(m.pois, type = "pearson")^2), nrow(nonzero)-2, 
       lower.tail = FALSE)
# not overdispersed??

# candidate models
m.region.ym.full <- glmer(data = wm.regionYM, formula = IDs ~ scale(mysids) + 
                            scale(size) + (1|Region.2), family = "poisson")
summary(m.region.ym.full)

m.region.ym.mys <- glmer(data = wm.regionYM, formula = IDs ~ scale(mysids) +
                             (1|Region.2), family = "poisson")
summary(m.region.ym.mys)

m.region.ym.size <- glmer(data = wm.regionYM, formula = IDs ~ scale(size) + 
                              (1|Region.2), family = "poisson")
summary(m.region.ym.size)

m.region.ym.0 <- glmer(data = wm.regionYM, formula = IDs ~ 1 + 
                         (1|Region.2), family = "poisson")
summary(m.region.ym.0)

## AICc model selection
library(wiqid) # for AICc
rows <- c("Mysids + Size", "Mysids", "Size", "Null")
columns <- c("AIC", "delta", "weight")
AICc.regionYM <-  data.frame(matrix(nrow = 4, ncol = 3, data = c(wiqid::AICc(m.region.ym.full),
                                                                 wiqid::AICc(m.region.ym.mys),
                                                                 wiqid::AICc(m.region.ym.size),
                                                                 wiqid::AICc(m.region.ym.0)), 
                                     dimnames = list(rows, columns)))
AICc.regionYM[,2] <- AICc.regionYM[,1] - min(AICc.regionYM)
AICc.regionYM[,3] <- exp(-0.5*AICc.regionYM[,2])/sum(exp(-0.5*AICc.regionYM[,2]))
AICc.regionYM
# prefers size, mysids + size delta = 5