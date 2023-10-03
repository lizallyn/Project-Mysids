# Mysid figures revision

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
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), lwd = 0.75) +
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
  draw_plot(insetmap, x = 0.72, y = 0.15, 
            width = 0.3, height=0.3)
map_with_inset

# ggsave(plot = map_with_inset,
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
  labs(x = "Longitude", y = "Latitude", title = "2019 Whales") +
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
  labs(x = "Longitude", y = "Latitude", title = "2020 Whales") +
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
        legend.text = element_text(size = 13, colour = "black")) +
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
ggsave(plot = fourpanelwithlegend, 
       filename = "C:/Users/Elizabeth Allyn/Box/Makah Fisheries Management/Er prey/Liz Needs These Uploaded/Manuscript Docs/Review/Figures/four panel composite map with legend.pdf",
       width = 11, height  = 10, device='pdf', dpi=700)
