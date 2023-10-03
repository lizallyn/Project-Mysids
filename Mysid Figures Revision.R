# Mysid figures revision

sites <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Sample%20site%20coords%20for%20R.csv")
tows <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Tow%20data%20for%20R.csv")
whale <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Whales%20in%20full%20survey%20area%202019%202020.csv")

## Sample sites map with whale survey area outline

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