### Mysid Analysis, Take 3
### Including corrections from QERM 514 feedback
### July 25, 2023

### Read In Data

# from file
# data <- data <- read.csv("Er prey analysis for R fixed whale presence.csv")
# from GitHub repo
data <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv?token=GHSAT0AAAAAACFRBFAJSQ6NKD2A3Y6VTLAYZGAJMRA")

### Data Manipulation/Cleaning/Visualization

data$Site <- factor(data$Site, levels = c("Chito Beach", "Bullman Beach", "Seal and Sail", "Sail River", "First Beach", "Koitlah", "Slant Rock", "Skagway Rocks", "Anderson Rocks", "Portage Head", "Duk Point", "North Bodelteh", "South Bodelteh", "Ozette Island"))

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
install.packages("RLRsim")
library(TMB)
library(glmmTMB)
library(RLRsim)

# model with the random effect of Site
model.rand <- glmmTMB(data = data, whalecount ~ MysidCount + StudyMon + (1|Site), 
                  family = truncated_nbinom1, ziformula = ~.)
summary(model.rand)
model.matrix(model.rand)
# returns design matrix

# model without the random effect of Site
model.norand <- glmmTMB(data = data, whalecount ~ MysidCount + StudyMon, 
                  family = truncated_nbinom1, ziformula = ~.)
summary(model.norand)
model.matrix(model.norand)

lrtest(model.rand, model.norand) # doesn't work
# Site makes sense according to structure of data and finding a package to 
# test random effect structure is turning into a pain in the butt so including

