#### Clean Figures for Prey Manuscript
#### January 2023
#### liz.allyn@makah.com

#### Packages
library(cowplot)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(dplyr)
library(psych)
library(knitr)
library(tidyr)
library(gridExtra)
library(PNWColors)
library(colorspace)

#### Read in Data
setwd("~/Box Sync/Makah Fisheries Management/Er prey/Final R Docs")
sites <- read.csv("Sample site coords for R.csv")
data <- read.csv("All obs for R.csv")
data2 <- read.csv("Er prey analysis for R fixed whale presence.csv")
tows <- read.csv("Tow data for R.csv")

#### Fig 1: Sample Sites Map ####

# packages needed: ggmap, ggrepel

# bounds for sample map
maxlong <- -124.9
minlat <- 48.1
minlong <- -124
maxlat <- 48.45

# bounds for inset map
insleft <- -127
insright <- -121
instop <- 50
insbott <- 45

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

outline.lat <- c(insbott, insbott, instop, instop, insbott)
outline.long <- c(insleft, insright, insright, insleft, insleft)
outline <- data.frame(cbind(outline.lat, outline.long))

inset <- get_stamenmap(bbox=c(insleft, insbott, insright, instop), 
                       zoom=7, maptype="terrain")
insetmap <- ggmap(inset) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), size = 1) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1.5)

base_ter <- get_stamenmap(bbox = c(maxlong,minlat,minlong,maxlat), 
                          zoom=11, maptype="terrain")
map1 <- ggmap(base_ter) +
  geom_point(data = sites, 
             aes(x = Dec.long, y = Dec.lat), 
             color = "white") +
  labs(x = "Longitude", y = "Latitude") +
  geom_text_repel(data = sites,
                  alpha = 0.8,
                  color = "black",
                  aes(x = Dec.long, y = Dec.lat, label = Location),
                  segment.linetype = 0,
                  force_pull = 1,
                  box.padding = 0.1,
                  size = 3,
                  fontface = 2)
map_with_inset <- ggdraw() + 
  draw_plot(map1) + 
  draw_plot(insetmap, x = 0.65, y = 0.55, 
            width = 0.4, height=0.4)
map_with_inset

# ggsave(plot = map_with_inset, filename = "Sample sites map final.pdf",
#        width = 9, height = 5, device='pdf', dpi=700)

#### Fig 2: Prey Density Map ####

# packages needed:

# bounds for tow map
long1 <- -124.9
lat1 <- 48.1
long2 <- -124.2
lat2 <- 48.45

# sort tow data
tows <- tows[order(-tows$Mysids),]
tows$Month <- factor(tows$Month, levels = c("June", "July", "August", "September", "October", "November"))

# filter tow data by year
tows2019 <- dplyr::filter(tows, Year == 2019)
tows2020 <- dplyr::filter(tows, Year == 2020)

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

#### Fig 3: Species Composition Bar Plot ####

# data2 wide to long
long.spp.all <- gather(data2, Species, Count, HS:U)

# create year_month column
long.spp.all$ym <- paste(long.spp.all$Year, long.spp.all$Month, sep = "_")
long.spp.all$ym <- factor(long.spp.all$ym, levels = c("2019_6", "2019_7", "2019_8", "2019_9", "2019_10", "2019_11", "2020_6", "2020_7", "2020_8", "2020_9"))

# summarize by year_month and species
species.counts.ym <- long.spp.all %>%
  group_by(ym, Species) %>%
  dplyr::summarise(tows = length(Sample),
                   Mysids = sum(Count))
species.counts.ym$Species <- factor(species.counts.ym$Species, 
                                    levels = c("HS", "NR", "CI", "TC", "HP", "ED", "EG", "U"))

# create avg mysids per tow column
species.counts.ym$ym <- as.factor(species.counts.ym$ym)
species.counts.ym$pertow <- species.counts.ym$Mysids/species.counts.ym$tows

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
  ggplot(data = species.counts.ym, aes(x = ym, y = pertow, fill = Species)) + 
  geom_col(position = "stack") + 
  labs(x = "Year_Month", y = "Avg. mysids per tow") +
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

# ggsave(plot = plot.Speciesym, 
#        filename = "species comp by month italic legend no outline.pdf", 
#        width = 9, height = 5, device='pdf', dpi=700)

#### Fig 4: Size Distribution Histograms ####

# pull out mysids with a valid length
data.mysids <- dplyr::filter(data, mysid.=="YES")
data.mysids$Monthies <- factor(data.mysids$Monthies, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
data.mysids$length <- as.numeric(data.mysids$length)
num.lengths <- filter(data.mysids, length>1)

# separate years
num.lengths.2019 <- filter(num.lengths, Year == "2019")
num.lengths.2020 <- filter(num.lengths, Year == "2020")

# palette for plot

theme.sizes <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 25), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 25),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "black"), 
        axis.text.y = element_text(size = 15, colour = "black"), 
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

plot.sizes.19 <- 
  ggplot(data = num.lengths.2019, aes(length)) +
  geom_histogram(aes(fill = Monthies),
                 binwidth = 2) +
  scale_fill_manual(name = "Month", 
                    labels = c("June", "July", "August", "September", "October", "November"),
                    values = c("dodgerblue2", "skyblue2", "gold2", "orange2", "indianred2", "violetred2")) + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

plot.sizes.20 <-
  ggplot(data = num.lengths.2020, aes(length)) +
  geom_histogram(aes(fill = Monthies),
                 binwidth = 2,
                 color = "black") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,4500)) +
  scale_fill_manual(values = c("dodgerblue2", "skyblue2", "gold2", "orange2", "indianred2", "violetred2")) +
  labs(x = "Length (mm)", y = "Frequency", fill = "Month", title = "2020") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

legend.sizes <- get_legend(plot.sizes.19)

plot.sizes.19x <- 
  ggplot(data = num.lengths.2019, aes(length)) +
  geom_histogram(aes(fill = Monthies),
                 binwidth = 2,
                 color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,4500)) +
  scale_fill_manual(values = c("dodgerblue2", "skyblue2", "gold2", "orange2", "indianred2", "violetred2")) +
  labs(x = "Length (mm)", y = "Frequency", fill = "Month", title = "2019") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

mysidsizedistribution <- grid.arrange(arrangeGrob(plot.sizes.19x,plot.sizes.20), legend.sizes, ncol = 2, widths = c(2,0.5))

# ggsave(plot = mysidsizedistribution, "mysid size distribution histogram plot white background legend text bigger full months black outlines final.pdf", width = 9, height = 5, device='pdf', dpi=700)

# 

#### Mysid Maturity Calculations ####

# summarize effort
effort.summ1 <- tows %>% 
  group_by(Year, Month, Assigned.ID) %>% 
  dplyr::summarize(samples = length(Assigned.ID))
effort.summ <- effort.summ1 %>% 
  group_by(Year, Month) %>% 
  dplyr::summarise(Samples = sum(samples))
tows.ym <- data.frame(effort.summ)

# summarize gravidity
data.mysids$gravid. <- as.numeric(data.mysids$gravid.)

mysids.all <- data.mysids %>% 
  group_by(Year.Month, gender) %>% 
  summarize(count = n(),
            gravid = sum(gravid.))

totalmys <- mysids.all %>% 
  group_by(Year.Month) %>% 
  summarize(total = sum(count),
            gravid = sum(gravid))

# summarize gender
females <- dplyr::filter(.data = mysids.all, gender == "F")
females.1 <- c(0,0,females$count, 0)
males <- dplyr::filter(.data = mysids.all, gender == "M")
males.1 <- c(males$count[1],0,males$count[2:7], 0)
juv <- dplyr::filter(.data = mysids.all, gender == "J")
juv.1 <- c(0, juv$count)

Samples <- tows.ym$Samples[1:9]

# combine into data frame
totalmys.1 <- cbind(totalmys, females.1, males.1, juv.1, Samples)
totalmys.1$adults <- totalmys.1$males.1 + totalmys.1$females.1
totalmys.1$nongrav <- totalmys.1$females.1 - totalmys.1$gravid

# stats for results paragraph

sum(totalmys.1$adults)/sum(totalmys.1$total) #24.2% mature overall
sum(totalmys.1$females.1)/sum(totalmys.1$adults) # 24.1% female of mature overall
sum(totalmys.1$males.1)/sum(totalmys.1$adults) # 75.9% males of mature overall
sum(totalmys.1$gravid)/sum(totalmys.1$females.1) # 64.7% gravid of females overall
sum(totalmys.1$adults[1:6])/sum(totalmys.1$total[1:6]) # 3.8% mature in 2019
sum(totalmys.1$juv.1[1:6])/sum(totalmys.1$total[1:6]) # 95.8% juvenile in 2019
sum(totalmys.1$adults[7:9])/sum(totalmys.1$total[7:9]) # 71.4% mature in 2020
sum(totalmys.1$females.1[1:6])/sum(totalmys.1$adults[1:6]) # 48.7% females of mature 2019
sum(totalmys.1$females.1[7:9])/sum(totalmys.1$adults[7:9]) # 21.0% females of mature 2020
sum(totalmys.1$males.1[1:6])/sum(totalmys.1$adults[1:6]) # 51.3.7% males of mature 2019
sum(totalmys.1$males.1[7:9])/sum(totalmys.1$adults[7:9]) # 79.0% males of mature 2020
sum(totalmys.1$gravid[1:6])/sum(totalmys.1$females.1[1:6]) # 50.5% gravid of females 2019
sum(totalmys.1$gravid[7:9])/sum(totalmys.1$females.1[7:9]) # 68.8% gravid of females 2020

#### Fig 5: Whales and Mysids Monthly Bar Plot ####

whaledays <- read.csv("whales per day for R.csv") #this includes whales seen on days with no tows

whaledays$Year_Month <- factor(whaledays$Year_Month, 
                               levels = c("2019_6", "2019_7", "2019_8", 
                                          "2019_9", "2019_10", "2019_11", 
                                          "2020_6", "2020_7", "2020_8", "2020_9", 
                                          "2020_11"))

whaday <- whaledays %>% 
  group_by(Year_Month) %>% 
  summarize(whaday = mean(Unique))

whalefood <- long.spp.all %>% 
  group_by(Date, Sample) %>% 
  summarize(mysids.1 = mean(Mysid.count),
            whapd = mean(uniwhal))

whales <- whalefood %>% 
  group_by(Date) %>% 
  summarize(mysids = sum(mysids.1),
            tows = length(Sample),
            whapd = mean(whapd))

months <- data %>% 
  group_by(Year, Month, Date) %>% 
  summarize(count = n())

whales$pertow <- whales$mysids/whales$tows
whales <- cbind(whales, months[,1:2])
whales$ym <- paste(whales$Year, whales$Month, sep = "_")
whales$ym <- factor(whales$ym, 
                    levels = c("2019_6", "2019_7", "2019_8", "2019_9", 
                               "2019_10", "2019_11", "2020_6", "2020_7", 
                               "2020_8", "2020_9", "2020_11"))

whysids.1 <- whales %>% 
  group_by(ym) %>% 
  summarize(mysids = mean(pertow),
            whapd = mean(whapd))

no202011 <- whaday[1:10,]
whadaym <- cbind(no202011, whysids.1[,2])

whadaym$whales <- whadaym$whaday*10
whadaym.long <- gather(whadaym, animal, density, mysids:whales, factor_key=T)

plot.whasnmysbarssolo <- 
  ggplot(data = whadaym.long, aes(x = Year_Month, y = density, fill = animal)) + 
  geom_bar(stat = "identity", position = dodge, width = 0.8) +
  scale_y_continuous(expand = c(0,0), name = "Avg. mysids per tow", sec.axis = sec_axis( trans=~.*.1, name = "Avg. whales per day")) +
  labs(x = "Year_Month") +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -0.5),
        axis.title.y.left = element_text(size = 14, vjust = 2),
        axis.title.y.right = element_text(size = 14, vjust = 2),
        title = element_text(size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black")) +
  guides(color = guide_legend("Animal")) +
  scale_fill_manual(name = "", 
                    labels = c("Mysids", "Whales"), 
                    values = c("mysids" = "darkgoldenrod3", "whales" = "skyblue3"))


plot(plot.whasnmysbarssolo)

# ggsave(plot = plot.whasnmysbarssolo,
#        "whales and mysids double bar chart white plot area.pdf", 
#        width = 10, height = 6, device='pdf', dpi=700)

#### Fig 6: Whales and Mysids Contingency Bar Plot ####

mysandwhales.1 <- long.spp.all %>%
  group_by(Sample, Mysid.density, whale.presence) %>%
  dplyr::summarize(count = n())
mysandwhales <- mysandwhales.1 %>%
  group_by(Mysid.density, whale.presence) %>%
  dplyr::summarize(tows = length(Sample))
mysandwhales$Mysid.density <- factor(mysandwhales$Mysid.density, 
                                     levels = c("None", "Low", "High"))

theme.mysandwhales <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 14), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 14),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 12, colour = "black"), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"))

# plot building
plot.mysandwhales <- 
  ggplot(data = mysandwhales, aes(x = Mysid.density, y = tows, fill = whale.presence)) + 
  geom_bar(stat = "identity", position = dodge, width = 0.75) + 
  labs(y="Number of Samples", x="Mysid Density", fill = "Whale(s) Present?") +
  scale_fill_manual(name = "Whale(s) Present?", 
                    labels = c("No", "Yes"), 
                    values = c("No" = "wheat4",
                               "Yes" = "skyblue3")) +
  scale_y_continuous(expand = c(0,0)) + 
  theme.mysandwhales

plot.mysandwhales

# ggsave(plot = plot.mysandwhales,
#        "mysids and whales fixed column order white background.pdf",
#        width = 11, height = 7, device='pdf', dpi=700)
