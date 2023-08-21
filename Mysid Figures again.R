### Mysid Figures
### Started August 21, 2023

### Figures

## Prey density map combined years and months

library(ggplot2)
library(ggmap)

# Summarize by site

site.summ <- tows %>%
  group_by(Location) %>%
  summarize(myspertow <- mean(Mysids),
            lat <- mean(Dec.lat),
            long <- mean(Dec.long))
colnames(site.summ) <- c("Location", "myspertow", "lat", "long")
data.frame(site.summ)

# bounds for tow map
long1 <- -124.9
lat1 <- 48.1
long2 <- -124.2
lat2 <- 48.45

# sort tow data
tows <- tows[order(-tows$Mysids),]

# load terrain map
tow_ter <- get_stamenmap(bbox = c(long1, lat1, long2, lat2), zoom=11, maptype = "terrain")

mysids.map.combined <- ggmap(tow_ter) +
  geom_point(aes(x=long, y=lat, size = myspertow),
             data = site.summ,
             alpha = 0.5,
             color = "sienna4") +
  lims(size = c(0,800)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14))
mysids.map.combined

# ggsave(plot = mysiddensitymap, "sample map larger legend dots 3.pdf",
#        width = 9, height  = 9, device='pdf', dpi=700)

## Species catch comp plot

data$pc.HS <- data$HS/data$MysidCount
data$pc.NR <- data$NR/data$MysidCount
data$pc.CI <- data$CI/data$MysidCount
data$pc.ED <- data$ED/data$MysidCount
data$pc.EG <- data$EG/data$MysidCount
data$pc.HP <- data$HP/data$MysidCount
data$pc.TC <- data$TC/data$MysidCount
data$pc.U <- data$U/data$MysidCount
data$totalcheck <- data$pc.HS + data$pc.NR + data$pc.CI + data$pc.ED + 
  data$pc.EG + data$pc.HP + data$pc.TC + data$pc.U

data$Sample[which(data$totalcheck < 1)]

# make data long
long.spp.all <- gather(data, Species, Count, HS:U)

# create year_month column
long.spp.all$ym <- paste(long.spp.all$Year, long.spp.all$Month, sep = "_")
long.spp.all$ym <- factor(long.spp.all$ym, 
                          levels = c("2019_6", "2019_7", "2019_8", "2019_9", 
                                     "2019_10", "2019_11", "2020_6", "2020_7", 
                                     "2020_8", "2020_9"))

# filter by year
spp2019 <- dplyr::filter(long.spp.all, Year == 2019)
spp2020 <- dplyr::filter(long.spp.all, Year == 2020)

# summarize by month and species for each year
spp.summ.2019 <- spp2019 %>%
  group_by(Month, Species) %>%
  summarize(avg)