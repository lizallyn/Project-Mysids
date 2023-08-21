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

# make data long
long.spp.all <- gather(data, Species, Count, HS:U)

# create year_month column
long.spp.all$ym <- paste(long.spp.all$Year, long.spp.all$Month, sep = "_")
long.spp.all$ym <- factor(long.spp.all$ym, 
                          levels = c("2019_6", "2019_7", "2019_8", "2019_9", 
                                     "2019_10", "2019_11", "2020_6", "2020_7", 
                                     "2020_8", "2020_9"))

spp.summ <- long.spp.all %>%
  group_by(Species, ym) %>%
  summarize(perspp <- sum(Count),
            totalmys <- sum(MysidCount))
colnames(spp.summ) <- c("Species", "ym", "perspp", "totalmys")
as.data.frame(spp.summ)
spp.summ$pc <- spp.summ$perspp/spp.summ$totalmys

# order spp by abundance
spp.summ$Species <- as.factor(spp.summ$Species)
spp.summ <- spp.summ %>%
  mutate(Species = recode(Species, HS = "H. sculpta", NR = "N. rayii", CI = "C. ignota", 
                              TC = "T. columbiae", HP = "H. platypoda", ED = "E. davisi", 
                              EG = "E. grimaldii", U = "Unknown"))
spp.summ$Species <- factor(spp.summ$Species, levels = c("H. sculpta", 
                                                        "N. rayii", 
                                                        "C. ignota", 
                                                        "T. columbiae", 
                                                        "H. platypoda", 
                                                        "E. davisi", 
                                                        "E. grimaldii", 
                                                        "Unknown"))

# set the theme
dodge <- position_dodge(width=0.9)
theme.Speciesym <- theme_classic() +
  theme(plot.margin = margin(t=10,r=10,b=10,l=10),
        axis.title = element_blank(), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 12), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 12), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 10, colour = "black", angle=90), 
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.ticks.x = element_blank(),
        legend.position = "right", 
        legend.title = element_text(size = 12, colour = "black"), 
        legend.text = element_text(size = 10, colour = "black", face = "italic"), 
        legend.key.size = unit(1, "line")) # size of color boxes

# plot building
plot.Speciesym <- 
  ggplot(data = spp.summ, aes(x = ym, y = pc, fill = Species)) + 
  geom_col(position = "stack") + 
  labs(x = "Year_Month", y = "catch composition") +
  theme.Speciesym +
  guides(color = guide_legend("Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Species", 
                    labels = c("H. sculpta", "N. rayii", "C. ignota", 
                               "T. columbiae", "H. platypoda", "E. davisi", 
                               "E. grimaldii", "Unknown"),
                    values = c("HS" = "khaki3",
                               "NR" = "darksalmon",
                               "CI" = "skyblue2",
                               "TC" = "darkseagreen2",
                               "HP" = "skyblue4",
                               "ED" = "darkslategrey",
                               "EG" = "darkseagreen4",
                               "U" = "darkgrey"))
plot(plot.Speciesym)
