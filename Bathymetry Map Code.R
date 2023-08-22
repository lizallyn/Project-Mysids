#### Bathymetry Map Code
#### Updated Nov 4, 2021
#### revistied August 2023
#### liz.allyn@makah.com

### Packages

library(marmap) #getNOAA.bathy
library(ggmap) #get_map, get_stamenmap, ggmap, geom_
library(RColorBrewer) #brewer.pal

### Define Terms

## bounding coordinates
# format: max long, min lat, min long, max lat

maxlong <- -124.9
minlat <- 48
minlong <- -124.2
maxlat <- 48.5

region <- c(maxlong, minlat, minlong, maxlat)

## Grab NOAA bathymetry
# highest resolution = 1

resolution <- 1

b <- marmap::getNOAA.bathy(lon1 = maxlong, lon2 = minlong, lat1 = minlat, lat2 = maxlat, 
                   resolution = resolution)

b <- fortify.bathy(b) # makes it a bathy-specific data.frame 

### Create base map
# lots of maptype options, terrain, toner most useful
# zoom: 3 = continent, 10 = city, 21 = building. 
#   zoom has limits depending on maptype, can set to auto
# for bathymetry the whole thing gets covered so 
#   it doesn't matter what you choose here

maptype <- "toner-line"
zoom <- 11
base <- get_stamenmap(bbox = c(maxlong,minlat,minlong,maxlat),
                      zoom=zoom,
                      maptype="toner-line")
# this will look like crap but it's ok we're going to cover it

### Add Bathymetry contour lines and fill

# create color scale that makes the land gray

binwidth <- 10
# width of contours in feet (I think)

palette <- "Blues" # palette you want to expand
pal_length <- 9 # num colors in existing palette

h2o_bins <- ceiling(abs(min(b$z))/binwidth) # num h2o contours
land_bins <- ceiling(max(b$z)/binwidth) # num land contours
land_pal <- rep(x = "ivory4", times = land_bins)
# creates a palette with the same shade of gray for each contour
h2o_pal <- rev(colorRampPalette(brewer.pal(pal_length, palette))(h2o_bins))
# creates a palette with a shade of blue for each contour
all_pal <- c(h2o_pal, land_pal) # combine palettes

# set contour color
lines <- "ivory4" # match the land contour fill so they don't show

# create the plot!

site.summ$mlat <- site.summ$lat + 0.011
site.summ$mlong <- site.summ$long - 0.007

b$alpha <- NULL
b$alpha[which(b$z >0)] <- "land"
b$alpha[which(b$z <0)] <- "water"
as.factor(b$alpha)

bathmap <- ggmap(base) +
  geom_contour_filled(data = b, 
                      aes(x=x, y=y, z=z),
                      binwidth = binwidth, 
                      show.legend = F) +
  scale_fill_manual(values = c(all_pal)) +
  geom_contour(data = b, 
               aes(x=x, y=y, z=z),
               binwidth = binwidth,
               color = lines,
               show.legend = F) +
  scale_color_manual(values = c(all_pal)) +
  labs(y = "Latitude", x = "Longitude", fill = "Depth") +
  geom_point(aes(x=mlong, y=mlat, size = myspertow),
             data = site.summ,
             color = "firebrick3")
# show the plot!
bathmap

# With ggOceanMaps
library(ggOceanMaps)

bathy <- raster_bathymetry(stars::st_as_stars(marmap::as.raster(b)), 
                           depths = NULL, verbose = FALSE)

mapcoords <- data.frame(lon = c(maxlong, maxlong, minlong, minlong), lat = c(minlat, maxlat, maxlat, minlat))
mapcoords2 <- data.frame(c(minlong, maxlong, minlat, maxlat))
basemap(mapcoords, shapefiles = list(land = dd_land, bathy = bathy), 
        bathy.style = "rcb") +
  geom_point(aes(x=long, y=lat),
             data = ggOceanMaps::transform_coord(site.summ.coord),
             alpha = 0.5,
             color = "sienna4")
