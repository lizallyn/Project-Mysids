# Map for Scarring paper


library(ggmap)
library(ggrepel)
library(ggplot2)
library(cowplot)

# bounds for sample map
maxlong <- -124.9
minlat <- 47.9
minlong <- -124.2
maxlat <- 48.45

# bounds for inset map
insleft <- -127
insright <- -120
instop <- 50
insbott <- 45

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

region.diag.lat <- c(acape, aoff)
region.diag.long <- c(ocape, ooff)
region.diag <- data.frame(cbind(region.diag.lat, region.diag.long))

survlat <- c(asurv1, asurv2, asurv3, asurv4, asurv5, asurv6)
survlong <- c(osurv1, osurv2, osurv3, osurv4, osurv5, osurv6)
survpath <- data.frame(cbind(survlat, survlong))

outline.lat <- c(insbott, insbott, instop, instop, insbott)
outline.long <- c(insleft, insright, insright, insleft, insleft)
outline <- data.frame(cbind(outline.lat, outline.long))

sekiu.pt <- data.frame(x = -124.2959, y = 48.2689, text = "Sekiu Point")
cape.fl <- data.frame(y = 48.3861, x = -124.72, text = "Cape \n Flattery")
sealionrock <- data.frame(y = 47.98, x = -124.7, text = "Sea Lion \n Rock")

pacific <- data.frame(x = -125.5, y = 47, text = "Pacific \n Ocean")
wash <- data.frame(x = -122, y = 47.5, text = "Washington")
ore <- data.frame(x = -122.5, y = 45.5, text = "Oregon")
bc <- data.frame(x = -122, y = 49, text = "British \n Columbia")

inset <- get_stadiamap(bbox=c(insleft, insbott, insright, instop), 
                       zoom=9, maptype="outdoors")
base_ter <- get_stadiamap(bbox = c(maxlong, minlat, minlong, maxlat), 
                          zoom=13, maptype="outdoors")
insetmap <- ggmap(inset) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), lwd = 0.5) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), lwd = 1.5) + 
  geom_text(data = pacific, aes(x = x, y = y, label = text), size = 3) + 
  geom_text(data = wash, aes(x = x, y = y, label = text), size = 3) + 
  geom_text(data = ore, aes(x = x, y = y, label = text), size = 3) + 
  geom_text(data = bc, aes(x = x, y = y, label = text), size = 3)

map1 <- ggmap(base_ter) +
  labs(x = "Longitude", y = "Latitude") +
  geom_text(data = sekiu.pt, aes(x = x, y = y, label = text)) + 
  geom_text(data = cape.fl, aes(x = x, y = y, label = text)) +
  geom_text(data = sealionrock, aes(x = x, y = y, label = text)) +
  geom_text(aes(x = -124.42, y = 48.4, label = "Strait of Juan de Fuca")) +
  geom_text(aes(x = -124.8, y = 48.28, label = "Pacific \n Ocean"))
map_with_inset <- ggdraw() + 
  draw_plot(map1) + 
  draw_plot(insetmap, x = 0.68, y = 0.1, 
            width = 0.3, height=0.3)
map_with_inset

ggsave(plot = map_with_inset,
       filename = "C:/Users/Elizabeth Allyn/Documents/GitHub/Project-Mysids/Figure Outputs/SurveyMapScarring_smooth.pdf",
       width = 8, height = 10, device='pdf', dpi=700)
