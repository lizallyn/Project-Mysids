### Mysid Analysis, Take 3
### Including corrections from QERM 514 feedback
### July 25, 2023

### Read In Data

# from file
# data <- data <- read.csv("Er prey analysis for R fixed whale presence.csv")
# from GitHub repo
data <- read.csv(url("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv"))
all <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/All%20obs%20for%20R.csv")

### Data Manipulation/Cleaning/Visualization

data$Site <- factor(data$Site, levels = c("Chito Beach", "Bullman Beach", "Seal and Sail", "Sail River", "First Beach", "Koitlah", "Slant Rock", "Skagway Rocks", "Anderson Rocks", "Portage Head", "Duk Point", "North Bodelteh", "South Bodelteh", "Ozette Island"))

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
m.zt.pois <- vglm(whalecount ~ MysidCount + StudyMon, data = nonzero, 
                  family = "pospoisson")
summary(m.zt.pois)

# check for overdispersion
pchisq(sum(residuals(m.zt.pois,type = "pearson")^2), nrow(nonzero)-2, lower.tail = FALSE)
# p = 0.000005, overdispersed

# fit with ZT neg binom instead
m.zt.nbin <- vglm(whalecount ~ MysidCount + StudyMon, data = nonzero, 
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

data$Site <- as.factor(data$Site)
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

### Figures

## Prey density map combined years and months

library(ggmap)

# Summarize by site

site.summ <- tows %>%
  group_by(Location) %>%
  summarize(myspertow <- mean(Mysids))

# bounds for tow map
long1 <- -124.9
lat1 <- 48.1
long2 <- -124.2
lat2 <- 48.45

# sort tow data
tows <- tows[order(-tows$Mysids),]

# load terrain map
tow_ter <- get_stamenmap(bbox = c(long1, lat1, long2, lat2), zoom=11, maptype = "terrain")

map2019 <- ggmap(tow_ter) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.4) +
  lims(size = c(0,2000)) +
  labs(x = "Longitude", y = "Latitude", title = "2019") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

map2020 <- ggmap(tow_ter) +
  geom_point(data = tows2020,
             alpha = 0.4,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month, alpha = 0.4)) +
  labs(x = "Longitude", y = "Latitude", title = "2020") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  lims(size = c(0,2000)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

maplegend <- ggmap(tow_ter) +
  geom_point(data = tows,
             alpha = 0.4,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "right",
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 13, colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha = 0.5)))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(maplegend)

mysiddensitymap <- grid.arrange(arrangeGrob(map2019,map2020), 
                                legend, ncol = 2, widths = c(2,0.5))
# ggsave(plot = mysiddensitymap, "sample map larger legend dots 3.pdf",
#        width = 9, height  = 9, device='pdf', dpi=700)