### Mysid Analysis, Take 3
### Including corrections from QERM 514 feedback
### July 25, 2023

### Read In Data

# from file
# data <- data <- read.csv("Er prey analysis for R fixed whale presence.csv")
# from GitHub repo
data.full <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")
all <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/All%20obs%20for%20R.csv")


### Data Manipulation/Cleaning/Visualization

# Pull out useful clean mysid data columns to simplify data frame

data <- data.full[,c(1,2,4,6,7,11,17:25,30,31)]
# as.data.frame(data)

data$Site <- factor(data$Site, 
                    levels = c("Chito Beach", "Bullman Beach", "Seal and Sail",
                               "Sail River", "First Beach", "Koitlah", 
                               "Slant Rock", "Skagway Rocks", "Anderson Rocks", 
                               "Portage Head", "Duk Point", "North of Bodelteh Islands", 
                               "South of Bodelteh Islands", "Ozette Island"))

## Sex Summary Table
library(tidyr)
library(dplyr)

all$counter <- rep(1, nrow(all))

sex.summ <- all %>%
  filter(mysid. == "YES") %>%
  group_by(Year, gender, gravid.) %>%
  summarise(total = sum(counter))

### Inspect Distribution

hist(data$whalecount)
# zero-inflated
# will need hurdle model to account for this

# Check for overdispersion in nonzero counts
# pull out non-zero counts
nonzero <- data[which(data$whalecount > 0),]

# install.packages("VGAM")
library(VGAM)

# construct zero-truncated Poisson model with mysid count
m.zt.pois <- vglm(whalecount ~ MysidCount + Month, data = nonzero, 
                  family = "pospoisson")
summary(m.zt.pois)

# check for overdispersion
pchisq(sum(residuals(m.zt.pois,type = "pearson")^2), nrow(nonzero)-2, lower.tail = FALSE)
# p = 0.000005, overdispersed

# fit with ZT neg binom instead
m.zt.nbin <- vglm(whalecount ~ MysidCount + Month, data = nonzero, 
                  family = posnegbinomial)
summary(m.zt.nbin)

## Look for weird things in data
# hat values
library(faraway)
hval <- hatvalues(m.zt.nbin)
thresh <- 2*(4/length(hval))
unusual <- data$Sample[which(hval>thresh)]
uvals <- as.numeric(hval[which(hval>thresh)])
# plot points of high leverage
halfnorm(hval, labs = nonzero$Sample, nlab = length(uvals))
# model structure accounts for a lot of other weird things, so I don't
# think we need any other diagnostics yet

### Random Effects Structure

# Construct hurdle model with ZT neg binom and assess Site as random effect.
# Assess with LRT
# install.packages("RLRsim")
library(TMB) # glmmTMB needs this
library(glmmTMB) # glmmTMB for model construction

# model with the random effect of Site
model.rand <- glmmTMB(data = data, whalecount ~ scale(MysidCount) + Month + (1|Site), 
                  family = truncated_nbinom1, ziformula = ~.)

# # not running again, troubleshooting
# any(is.infinite(unlist(data)))
# any(is.na(unlist(data)))
# # both return false
# class(data)

summary(model.rand)
model.matrix(model.rand)
# returns design matrix

# model without the random effect of Site
model.norand <- glmmTMB(data = data, whalecount ~ scale(MysidCount) + Month, 
                  family = truncated_nbinom1, ziformula = ~.)
summary(model.norand)
model.matrix(model.norand)

library(lmtest)
lrtest(model.rand, model.norand)
# doesn't favor the random effect, taking it out moving forward.

### Fixed Effects

glmm.mm <- glmmTMB(data = data, whalecount ~ scale(MysidCount) + Month, 
                   family = truncated_nbinom1, ziformula = ~.)
summary(glmm.mm)
glmm.m <- glmmTMB(data = data, whalecount ~ scale(MysidCount), 
                  family = truncated_nbinom1, ziformula = ~.)
summary(glmm.m)
glmm.mo <- glmmTMB(data = data, whalecount ~ Month, 
                  family = truncated_nbinom1, ziformula = ~.)
summary(glmm.mo)
glmm.0 <- glmmTMB(data = data, whalecount ~ 1, 
                  family = truncated_nbinom1, ziformula = ~.)
summary(glmm.0)

# plot(data$MysidCount, predict(glmm.0))
# the intercept for the conditional model is very negative, weird

rows <- c("Mysids + Month", "Mysids", "Month", "Null")
columns <- c("AIC", "delta", "weight")
AIC.comp.mm <-  data.frame(matrix(nrow = 4, ncol = 3, data = c(AIC(glmm.mm),
                                                               AIC(glmm.m),
                                                               AIC(glmm.mo),
                                                               AIC(glmm.0)), 
                                                               dimnames = list(rows, columns)))
AIC.comp.mm[,2] <- AIC.comp.mm[,1] - min(AIC.comp.mm)
AIC.comp.mm[,3] <- exp(-0.5*AIC.comp.mm[,2])/sum(exp(-0.5*AIC.comp.mm[,2]))
AIC.comp.mm
# Null is the best model at small spatial scales
# but just month is also in the running, and month and mysids has some evidence

confint(glmm.0)

### Region-wide Mysid-Whale Patterns

## Read in data

tows <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Tow%20data%20for%20R.csv")
whales <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/whales%20per%20day%20for%20R.csv")

## Data Manipulation

data$counter <- rep(1, nrow(data))

daily <- data %>%
  group_by(Date) %>%
  summarize(n.tows <- sum(counter),
            mysids <- mean(MysidCount))
colnames(daily) <- c("Date", "n.tows", "mysids")

sample.days <- which(whales$Date %in% data$Date)
whale.days <- slice(.data = whales, sample.days)
daily$whales <- whale.days$Unique

colnames(daily) <- c("Date", "n.tows", "mysids", "whales")


### Construct Some Models!

hist(daily$whales)
# again lots of zeros

## overdispersion likely, checking anyway
# same code as above, will override
# caution!!!

nonzero <- daily[which(daily$whales > 0),]

# construct zero-truncated Poisson model with mysid count
m.zt.pois <- vglm(whales ~ mysids, data = nonzero, 
                  family = "pospoisson")
summary(m.zt.pois)

# check for overdispersion
pchisq(sum(residuals(m.zt.pois,type = "pearson")^2), nrow(nonzero)-2, lower.tail = FALSE)
# p = very small, overdispersed, duh

## Hurdle Models again, here we go!

# no random effects to consider I don't think?

## Fixed Effects

area.model <- glmmTMB(data = daily, whales ~ mysids, 
                      family = truncated_nbinom1, ziformula = ~.)
summary(area.model)
# why is the mysid coefficient negative tho??

plot(daily$mysids, daily$whales)
# yeah this is too messy to pull anything out of for a story

null.area.model <- glmmTMB(data = daily, whales ~ 1, 
                           family = truncated_nbinom1, ziformula = ~.)

rows2 <- c("Area", "Null")
columns2 <- c("AIC", "delta", "weight")
AIC.comp.area <-  data.frame(matrix(nrow = 2, ncol = 3, data = c(AIC(area.model),
                                                               AIC(null.area.model)),
                                    dimnames = list(rows2, columns2)))
AIC.comp.area[,2] <- AIC.comp.area[,1] - min(AIC.comp.area)
AIC.comp.area[,3] <- exp(-0.5*AIC.comp.area[,2])/sum(exp(-0.5*AIC.comp.area[,2]))

AIC.comp.area
# mysid model is preferred...but not meaningful I don't think

confint(area.model)
#...but the CI's all include 0
# so it might be the preferred model, but not a ton of evidence,
# and likely picking up on something else

plot(daily$mysids, predict(area.model))

### Pull out just Sail River/Bullman area

SS.data <- data[which(data$Site %in% c("Seal And Sail", "Sail River", "Bullman Beach")),]

par(mfrow = c(2,1))
plot(data$MysidCount, data$whalecount)
plot(SS.data$MysidCount, SS.data$whalecount)
# looks just as confused as the other ones

hist(SS.data$whalecount)
hist(SS.data$MysidCount)

model.full <- glm(data = SS.data, formula = whalecount ~ scale(MysidCount), 
                      family = truncated_nbinom1, ziformula = ~.)

# not converging, trying with whale presence/absence

SS.data$whalepres <- NULL
SS.data$whalepres[which(SS.data$whalecount > 0)] <- 1
SS.data$whalepres[which(SS.data$whalecount == 0)] <- 0

plot(SS.data$MysidCount, SS.data$whalepres)

model.bin <- glm(data = SS.data, formula = whalepres ~ scale(MysidCount),
                 family = "binomial")
summary(model.bin)

### Adrianne's extras

## Avg Mysid size

data$counter <- rep(1, nrow(data))

daily <- data %>%
  group_by(Date) %>%
  summarize(n.tows <- sum(counter),
            mysids <- mean(MysidCount),
            size <- mean(Avg.length, na.rm = T))
colnames(daily) <- c("Date", "n.tows", "mysids", "size")

sample.days <- which(whales$Date %in% data$Date)
whale.days <- slice(.data = whales, sample.days)
daily$whales <- whale.days$Unique

colnames(daily) <- c("Date", "n.tows", "mysids", "whales")

# model with the random effect of Site
model.rand <- glmmTMB(data = data, whalecount ~ scale(MysidCount) + Month + (1|Site), 
                      family = truncated_nbinom1, ziformula = ~.)

summary(model.rand)
model.matrix(model.rand)
# returns design matrix

# model without the random effect of Site
model.norand <- glmmTMB(data = data, whalecount ~ scale(MysidCount) + Month, 
                        family = truncated_nbinom1, ziformula = ~.)
summary(model.norand)
model.matrix(model.norand)

# Whale sightings within 300m of tow sites

full.whales <- read.csv("Whales in full survey area 2019 2020.csv")
sites <- read.csv("Sample site coords for R.csv")

coordmatch <- data.frame(matrix(nrow= nrow(full.whales)))
# add sighting coords and IDs
coordmatch$sighting <- full.whales$Date_S
coordmatch$lat <- full.whales$Start_Dec_Lat
coordmatch$long <- full.whales$Start_Dec_Long
# create columns for assigned site names
coordmatch$site.lat <- "Error"
coordmatch$site.long <- "Error"
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
