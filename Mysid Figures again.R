### Mysid Figures
### Started August 21, 2023

### Figures

tows <- read.csv("Tow data for R.csv")
whale <- read.csv("Whales in full survey area 2019 2020.csv")

## Prey density map combined years and months

library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyr)

# Summarize by site

site.summ <- tows %>%
  group_by(Location) %>%
  summarize(myspertow <- mean(Mysids),
            lat <- mean(Dec.lat),
            long <- mean(Dec.long))
colnames(site.summ) <- c("Location", "myspertow", "lat", "long")
data.frame(site.summ)

site.summ.coord <- site.summ[,3:4]

# bounds for tow map
long1 <- -124.9
lat1 <- 48.1
long2 <- -124.2
lat2 <- 48.45

# sort tow data
tows <- tows[order(-tows$Mysids),]
whale$Est_Size_Best <- as.numeric(whale$Est_Size_Best)

# load terrain map
tow_ter <- get_stamenmap(bbox = c(long1, lat1, long2, lat2), zoom=11, maptype = "terrain")

mysids.map.combined <- ggmap(tow_ter) +
  geom_point(data = whale, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                               size = Est_Size_Best),
             color = "orchid3",
             alpha = 0.3) +
  geom_point(aes(x=long, y=lat, size = myspertow),
             data = site.summ,
             alpha = 0.5,
             color = "dodgerblue") +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14))
mysids.map.combined

# ggsave(plot = mysiddensitymap, "sample map larger legend dots 3.pdf",
#        width = 9, height  = 9, device='pdf', dpi=700)

## Other prey map with seal/sail/bullman detail inset

sites <- read.csv("Sample site coords for R.csv")

library(ggrepel)
library(cowplot)

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

#### Fig 2: Prey Density Map

# read in ss whale data clipped
ss.whale <- read.csv("Whale at Seal and Sail and Bullman 2019 2020.csv")

# whale icon
# install.packages("tidyverse")
library(tidyverse)
library(readr)
library(proj4)
library(magick)
library(ggimage)

ss.whale$image <- ("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/icons8-whale-66.png")
whale$image <- ("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/icons8-whale-66.png")

# bounds for underlying area map
long1 <- -124.72
lat1 <- 48.15
long2 <- -124.17
lat2 <- 48.45

# bounds for ss inset map creation
insleft.ss <- -124.575
instop.ss <- 48.375
insright.ss <- -124.51
insbott.ss <- 48.34

# bounds for outline on map of inset area
maxlong <- -124.575
minlat <- 48.375
minlong <- -124.51
maxlat <- 48.34

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

outline.lat <- c(insbott.ss, insbott.ss, instop.ss, instop.ss, insbott.ss)
outline.long <- c(insleft.ss, insright.ss, insright.ss, insleft.ss, insleft.ss)
outline <- data.frame(cbind(outline.lat, outline.long))

ss_ter <- get_stamenmap(bbox = c(insleft.ss, insbott.ss, insright.ss, instop.ss),
                        zoom=13, maptype = "terrain")

# sort tow data
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

# load terrain map
tow_ter <- get_stamenmap(bbox = c(long1, lat1, long2, lat2), zoom=11, 
                         maptype = "terrain", crop = F)

map2019 <- ggmap(tow_ter) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.5) +
  lims(size = c(0,800)) +
  labs(x = "Longitude", y = "Latitude", title = "2019") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.8)
ss.insetmap2019 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_image(data = ss.w2019, aes(x = Start_Dec_Long, y = Start_Dec_Lat, image = image),
             alpha = 1) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.5) +
  lims(size = c(0,800)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2019 <- ggdraw() + 
  draw_plot(map2019) + 
  draw_plot(ss.insetmap2019, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
map_with_inset2019

# now do it for 2020 too

map2020 <- ggmap(tow_ter) +
  geom_point(data = tows2020,
             alpha = 0.5,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month)) +
  labs(x = "Longitude", y = "Latitude", title = "2020") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  lims(size = c(0,2000)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.7)
ss.insetmap2020 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_image(data = ss.w2020, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                                  image = image, color = Month),
             alpha = 0.7) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2020,
             alpha = 0.5) +
  lims(size = c(0,2000)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2020 <- ggdraw() + 
  draw_plot(map2020) + 
  draw_plot(ss.insetmap2020, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
map_with_inset2020

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

mysiddensitymapwithwhalesasicons <- grid.arrange(arrangeGrob(map_with_inset2019,map_with_inset2020), 
                                legend, ncol = 2, widths = c(2,0.5))
ggsave(plot = mysiddensitymapwithwhalesasicons, "sample map with whales in SSB inset no crop icons.pdf",
       width = 9, height  = 9, device='pdf', dpi=700)

### Four panel mysids and whales map

# 2019 Mysids
map2019 <- ggmap(tow_ter) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2019,
             alpha = 0.7) +
  lims(size = c(0,800)) +
  labs(x = "Longitude", y = "Latitude", title = "2019 Mysids") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna1", "red2", "magenta2")) +
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
  lims(size = c(0,800)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna1", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2019 <- ggdraw() + 
  draw_plot(map2019) + 
  draw_plot(ss.insetmap2019, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
map_with_inset2019

# 2020 Mysid map
map2020 <- ggmap(tow_ter) +
  geom_point(data = tows2020,
             alpha = 0.5,
             aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month)) +
  labs(x = "Longitude", y = "Latitude", title = "2020 Mysids") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  lims(size = c(0,2000)) +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), 
            linewidth = 0.7)
ss.insetmap2020 <- ggmap(ss_ter) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), size = 1) +
  geom_point(aes(x=Dec.long, y=Dec.lat, size = Mysids, color = Month),
             data = tows2020,
             alpha = 0.5) +
  lims(size = c(0,2000)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna1", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

map_with_inset2020 <- ggdraw() + 
  draw_plot(map2020) + 
  draw_plot(ss.insetmap2020, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
map_with_inset2020

# 2019 Whale map
whalemap2019 <- ggmap(tow_ter) +
  geom_image(data = w2019, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                                  image = image, color = Month),
             size = 0.03) +
  lims(size = c(0,800)) +
  labs(x = "Longitude", y = "Latitude", title = "2019 Whales") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna1", "red2", "magenta2")) +
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
  lims(size = c(0,800)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna1", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

whalemap_with_inset2019 <- ggdraw() + 
  draw_plot(whalemap2019) + 
  draw_plot(ss.whalemap2019, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
whalemap_with_inset2019

# 2020 Whale map
whalemap2020 <- ggmap(tow_ter) +
  geom_image(data = w2020, aes(x = Start_Dec_Long, y = Start_Dec_Lat, 
                               image = image, color = Month),
             size = 0.03) +
  lims(size = c(0,800)) +
  labs(x = "Longitude", y = "Latitude", title = "2020 Whales") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
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
  lims(size = c(0,800)) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("mediumblue", "dodgerblue2", "yellow2", "sienna2", "red2", "magenta2")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_text(size = 14))

whalemap_with_inset2020 <- ggdraw() + 
  draw_plot(whalemap2020) + 
  draw_plot(ss.whalemap2020, x = 0.45, y = 0.1, 
            width = 0.4, height=0.4)
whalemap_with_inset2020

# Create the composite
panels <- grid.arrange(map_with_inset2019, 
                       whalemap_with_inset2019,
                       map_with_inset2020,
                       whalemap_with_inset2020, ncol = 2)
fourpanelwithlegend <- grid.arrange(panels, legend, ncol = 2, widths = c(2,0.25))
ggsave(path = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Final R Docs/Maps",
       plot = fourpanelwithlegend, 
       filename = "four panel composite map with legend.pdf",
       width = 11, height  = 7, device='pdf', dpi=700)

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
spp.summ$pc[spp.summ$pc == "NaN"] <- 0

# set the theme
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
library(PNWColors)
# "#CDC9C9" snow3 for unknown?
pal <- c(pnw_palette("Sailboat", 8))
plot.Speciesym <- 
  ggplot(data = spp.summ, aes(x = ym, y = pc, fill = Species)) + 
  geom_col(position = "stack") + 
  labs(x = "Year_Month", y = "Catch Composition %") +
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

