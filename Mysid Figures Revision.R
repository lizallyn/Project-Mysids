# Mysid figures revision

#### Charts ####

### Species Composition Bar Plot

data2 <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv")

library(ggplot2)
library(tidyr)
library(dplyr)
library(PNWColors)

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
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 15), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 15), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 12, colour = "black", angle=90), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.ticks.x = element_blank(),
        legend.position = "right", 
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 12, colour = "black", face = "italic"), 
        legend.key.size = unit(1, "line")) # size of color boxes
library(RColorBrewer)
pal <- c(brewer.pal(name = "Set2", n = 7), "darkgray")
pal2 <- c("dodgerblue", "orchid1", "turquoise2", "goldenrod", "orchid4", "skyblue2", "lightseagreen", "darkgray")

# plot building
plot.Speciesym <- 
  ggplot(data = species.counts.ym, aes(x = ym, y = pertow, fill = Species)) + 
  geom_col(position = "stack") + 
  labs(x = "Year_Month", y = "Avg. Mysids per Tow") +
  theme.Speciesym +
  guides(color = guide_legend("Species")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Species", 
                    labels = c("H. sculpta", "N. rayii", "C. ignota", 
                               "T. columbiae", "H. platypoda", "E. davisi", 
                               "E. grimaldii", "Unknown"),
                    values = pal)
plot(plot.Speciesym)

# ggsave(plot = plot.Speciesym,
#        filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/species comp by month bar plot.pdf",
#        width = 9, height = 5, device='pdf', dpi=700)

### Size Distribution Histograms

data <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/All%20obs%20for%20R.csv")

# pull out mysids with a valid length
data.mysids <- dplyr::filter(data, mysid.=="YES")
data.mysids$Monthies <- factor(data.mysids$Monthies, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
data.mysids$length <- as.numeric(data.mysids$length)
num.lengths <- filter(data.mysids, length>1)

data.mysids$count <- rep(1, nrow(data.mysids))

count.tow <- data.mysids %>%
  group_by(Year, Month, Assigned.ID) %>%
  summarize(total = sum(count),
            n.tows = 1)

count.ym <- count.tow %>%
  group_by(Year, Month) %>%
  summarize(avg.count = mean(total, na.rm = T),
            n.tows = sum(n.tows))
count.ym$Y_M <- paste(count.ym$Year, count.ym$Month, sep = "_")

length.ym <- data.mysids %>%
  group_by(Year, Month) %>%
  summarize(avg.length = mean(length, na.rm = T),
            error = sd(length, na.rm = T)/sqrt(sum(count)))
length.ym$Y_M <- paste(length.ym$Year, length.ym$Month, sep = "_")

ym.length.summ <- merge(x = length.ym, y = count.ym, by = "Y_M")
ym.length.summ$Y_M <- factor(ym.length.summ$Y_M, 
                             levels = c("2019_6", "2019_7", "2019_8", "2019_9", 
                             "2019_10", "2019_11", "2020_6", "2020_7", "2020_8"))

# build the plot

theme.sizes <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 15), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 15),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 12, colour = "black", angle = 90), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "white"))

bar.length <- 
  ggplot(data = ym.length.summ) + 
  geom_col(aes(x = Y_M, y = avg.length), fill = "dodgerblue") + 
  geom_errorbar(aes(x = Y_M, ymin = avg.length - error, 
                    ymax = avg.length + error),
                width = 0.2, lwd = 0.5) +
  labs(x = "Year_Month", y = "Average Length (mm)") +
  theme.sizes
bar.length

# ggsave(plot = bar.length,
#        filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/length bar plot.pdf",
#        width = 9, height = 5, device='pdf', dpi=700)

### Biomass regional scatterplot

theme.biomass <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0, size = 15), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black", size = 15),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 12, colour = "black"), 
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5, colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.position = "right", 
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 12, colour = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"))

biomass.plot <- ggplot(data = wm.regionYM) +
  geom_point(aes(x = biomass, y = IDskm, color = Region.2) , 
             size = 2.5) + 
  scale_fill_manual(name = "Region",
                    aesthetics = "color", values = c("dodgerblue", "salmon")) +
  labs(x = "Mysid Biomass (g)", y = "Avg. Unique Whales per km Surveyed") +
  theme.biomass +
  guides(color = guide_legend(override.aes = list(size = 2.5)))
biomass.plot

# ggsave(plot = biomass.plot,
#        filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/biomass scatterplot.pdf",
#        width = 9, height = 5, device='pdf', dpi=700)

#### Maps ####

## Sample sites map with whale survey area outline

sites <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Sample%20site%20coords%20for%20R.csv")

library(ggmap)
library(ggrepel)
library(ggplot2)
library(cowplot)

# bounds for sample map
maxlong <- -124.9
minlat <- 48.1
minlong <- -124.2
maxlat <- 48.45

# bounds for inset map
insleft <- -127
insright <- -121
instop <- 50
insbott <- 45

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

asurv1 <- 48.13
asurv2 <- 48.13
asurv3 <- 48.44
asurv4 <- 48.44
asurv5 <- 48.34
asurv6 <- 48.29

osurv1 <- -124.705
osurv2 <- -124.83
osurv3 <- -124.83
osurv4 <- -124.55
osurv5 <- -124.345
osurv6 <- -124.4

acape <- 48.3861
ocape <- -124.72
aoff <- 48.44
ooff <- -124.83

region.diag.lat <- c(acape, aoff)
region.diag.long <- c(ocape, ooff)
region.diag <- data.frame(cbind(region.diag.lat, region.diag.long))

survlat <- c(asurv1, asurv2, asurv3, asurv4, asurv5, asurv6)
survlong <- c(osurv1, osurv2, osurv3, osurv4, osurv5, osurv6)
survpath <- data.frame(cbind(survlat, survlong))

outline.lat <- c(insbott, insbott, instop, instop, insbott)
outline.long <- c(insleft, insright, insright, insleft, insleft)
outline <- data.frame(cbind(outline.lat, outline.long))

inset <- get_stamenmap(bbox=c(insleft, insbott, insright, instop), 
                       zoom=7, maptype="terrain-background")
base_ter <- get_stamenmap(bbox = c(maxlong, minlat, minlong, maxlat), 
                          zoom=11, maptype="terrain-background")
insetmap <- ggmap(inset) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), lwd = 0.7) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), lwd = 1.5)

map1 <- ggmap(base_ter) +
  geom_point(data = sites, 
             aes(x = Dec.long, y = Dec.lat), 
             color = "gray25") +
  geom_path(data = survpath, aes(x = survlong, y = survlat), lwd = 1, 
            color = "gray40") + 
  geom_path(data = region.diag, aes(x = region.diag.long, y = region.diag.lat), 
            lwd = 1, color = "gray40") +
  labs(x = "Longitude", y = "Latitude") +
  geom_text_repel(data = sites,
                  alpha = 0.8,
                  color = "black",
                  aes(x = Dec.long, y = Dec.lat, label = Location),
                  segment.linetype = 0,
                  force_pull = 1,
                  box.padding = 0.2,
                  size = 3.5,
                  fontface = 1)
map_with_inset <- ggdraw() + 
  draw_plot(map1) + 
  draw_plot(insetmap, x = 0.72, y = 0.12, 
            width = 0.3, height=0.3)
map_with_inset

# # ggsave(plot = map_with_inset,
#        filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/Sample site map revision.pdf",
#        width = 10, height = 8, device='pdf', dpi=700)

### Four panel mysids and whales map

# read in data
ss.whale <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Whale%20at%20Seal%20and%20Sail%20and%20Bullman%202019%202020.csv")
tows <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Tow%20data%20for%20R.csv")
whale <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Whales%20in%20full%20survey%20area%202019%202020.csv")

# whale icon
# install.packages("tidyverse")
library(tidyverse)
library(readr)
library(proj4)
library(magick)
library(ggimage)
library(gridExtra)

ss.whale$image <- ("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/icons8-whale-66.png")
whale$image <- ("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/icons8-whale-66.png")

# sort tow data, month as factor
tows <- tows[order(-tows$Mysids),]
tows$Month <- factor(tows$Month, levels = c("June", "July", "August", "September", "October", "November"))
ss.whale$Month <- as.factor(ss.whale$Month)
whale$Month <- as.factor(whale$Month)

# filter tow data by year
tows2019 <- dplyr::filter(tows, Year == 2019)
tows2020 <- dplyr::filter(tows, Year == 2020)

# filter whale data by year
ss.w2019 <- dplyr::filter(ss.whale, Year == 2019)
ss.w2020 <- dplyr::filter(ss.whale, Year == 2020)

w2019 <- dplyr::filter(whale, Year == 2019)
w2020 <- dplyr::filter(whale, Year == 2020)

# remove whale on cape flattery
w2019 <- w2019[-which(w2019$Start_Dec_Long == -124.7101),]

# bounds for underlying area map
long1 <- -124.8
lat1 <- 48.12
long2 <- -124.35
lat2 <- 48.43

# bounds for ss inset map creation
insleft.ss <- -124.575
instop.ss <- 48.375
insright.ss <- -124.515
insbott.ss <- 48.345

# bounds for outline on map of inset area
maxlong <- -124.575
minlat <- 48.375
minlong <- -124.515
maxlat <- 48.345

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

outline.lat <- c(insbott.ss, insbott.ss, instop.ss, instop.ss, insbott.ss)
outline.long <- c(insleft.ss, insright.ss, insright.ss, insleft.ss, insleft.ss)
outline <- data.frame(cbind(outline.lat, outline.long))

ss_ter <- get_stamenmap(bbox = c(insleft.ss, insbott.ss, insright.ss, instop.ss),
                        zoom=13, maptype = "terrain-background")

# load terrain map
tow_ter <- get_stamenmap(bbox = c(long1, lat1, long2, lat2), zoom=11, 
                         maptype = "terrain-background")

# remove 0 mysid rows so they don't plot!
tows2019 <- tows2019[-which(tows2019$Mysids == 0),]
tows2020 <- tows2020[-which(tows2020$Mysids == 0),]

# 2019 Mysids
map2019 <- ggmap(tow_ter) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.7) +
  labs(x = "Longitude", y = "Latitude", title = "2019 Mysids") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.8)
ss.insetmap2019 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.7) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2019 <- ggdraw() + 
  draw_plot(map2019) + 
  draw_plot(ss.insetmap2019, x = 0.53, y = 0.1, 
            width = 0.4, height=0.4)

# 2020 Mysid map
map2020 <- ggmap(tow_ter) +
  geom_point(data = tows2020,
             alpha = 0.5,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month)) +
  labs(x = "Longitude", y = "Latitude", title = "2020 Mysids") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.8)
ss.insetmap2020 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2020,
             alpha = 0.5) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2020 <- ggdraw() + 
  draw_plot(map2020) + 
  draw_plot(ss.insetmap2020, x = 0.53, y = 0.1, 
            width = 0.4, height=0.4)

# 2019 Whale map
whalemap2019 <- ggmap(tow_ter) +
  geom_image(data = w2019, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                               image = image, color = Month),
             size = 0.05) +
  labs(x = "Longitude", y = "Latitude", title = "2019 Gray Whales") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.8)
ss.whalemap2019 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_image(data = ss.w2019, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                                  image = image, color = Month),
             size = 0.09) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

whalemap_with_inset2019 <- ggdraw() + 
  draw_plot(whalemap2019) + 
  draw_plot(ss.whalemap2019, x = 0.53, y = 0.1, 
            width = 0.4, height=0.4)

# 2020 Whale map
whalemap2020 <- ggmap(tow_ter) +
  geom_image(data = w2020, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                               image = image, color = Month),
             size = 0.05) +
  labs(x = "Longitude", y = "Latitude", title = "2020 Gray Whales") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.8)
ss.whalemap2020 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_image(data = ss.w2020, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                                  image = image, color = Month),
             size = 0.09) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("June" = "mediumblue", 
                                "July" = "dodgerblue2", 
                                "August" ="yellow2", 
                                "September" = "sienna1", 
                                "October" = "red2", 
                                "November" = "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

whalemap_with_inset2020 <- ggdraw() + 
  draw_plot(whalemap2020) + 
  draw_plot(ss.whalemap2020, x = 0.53, y = 0.1, 
            width = 0.4, height=0.4)
whalemap_with_inset2020

# legend

legend.tows <- tows[-which(tows$Mysids == 0),]

maplegend <- ggmap(tow_ter) +
  geom_point(data = legend.tows,
             alpha = 0.4,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "right",
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 13, colour = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(colour = guide_legend(override.aes = list(size=5, alpha = 0.5)))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(maplegend)

# Create the composite
panels <- grid.arrange(map_with_inset2019, 
                       whalemap_with_inset2019,
                       map_with_inset2020,
                       whalemap_with_inset2020, ncol = 2)
fourpanelwithlegend <- grid.arrange(panels, legend, ncol = 2, widths = c(1.5,0.3))
# # ggsave(plot = fourpanelwithlegend, 
#        filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/four panel composite map with legend.pdf",
#        width = 11, height  = 10, device='pdf', dpi=700)
