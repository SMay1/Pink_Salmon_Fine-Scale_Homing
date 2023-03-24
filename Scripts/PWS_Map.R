#Script to make map for publication.
#Code originally supplied by L. Turner and later modified for this study.

library("rnaturalearthdata")
#remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(rnaturalearth)
library(tidyverse)
library(rgeos)
library(sf)
library(ggpubr)
library(cowplot)
library(ggspatial)

# ------------------------------------------------------------------------------
#detach("package:raster", unload = T)

# Map of Alaska
nc = st_read(dsn = "../Data/AlaskaCoast_columbiabay.shp")
# Data for lat long locations
sf.map <- read.csv("../Data/Creek_Coords.csv", na.string = "") 
head(sf.map)
names(sf.map)
# Create a dataframe for mapping sites
# Convert to meters to deal with the weird -180 180 
sf.sta <-
  data.frame(
    Creek = sf.map$Creek,
    lat = sf.map$Lat,
    lon = sf.map$Long
  ) %>%
  st_as_sf(.,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant") %>%
  st_transform(., 3469)


#create bounding coordinates
bbox <- st_bbox(sf.sta) + c(-20000, -10000, 20000, 10000) 

# Transform shapefile
PWS <- nc %>% #Alaska map data
  st_transform(., 3469) %>% #set the projection
   st_crop(., bbox) %>% #clip to the SEAK region
  st_cast(., "POLYGON")

# Transform shapefile
AK <- nc %>% #Alaska map data
  st_transform(., 3469) %>% #set the projection
  st_cast(., "POLYGON")

# Check bbox
bbox
# Map --------------------------------------------------------------------------

colorz<-viridis::plasma(5,end = 0.8)

################################################################################

# Create map only showing the radius around the island -------------------------

ak.map <- ggplot(AK) +
  geom_sf(fill = "white",size=0.5) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),panel.border = element_blank(),
        panel.background = element_rect(fill="transparent",colour="transparent"),
        plot.background = element_rect(fill="transparent",colour="transparent")) +
   geom_rect(
    aes(
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[2],
      ymax = bbox[4]
    ),
    color = "red",
    size = 2,
    fill = NA)+
  coord_sf(expand = F)+
  annotate(geom="text",label="AK",
           x = 100000, y = 1300000,size=4.5,color="black")+
  labs(y="",x="")

ak.map

PWS.map <- ggplot(PWS) +
  geom_sf(fill = "lightgrey", alpha = .8) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = alpha("azure", .5), colour = "azure")) + 
  theme(panel.grid = element_blank(),legend.position = "top")+
  #guides(fill=guide_legend(nrow=2,byrow=T))+
  geom_sf(data = sf.sta, aes(fill = Creek), size = 4, shape = 21) +
  scale_fill_discrete(type = colorz)+
  coord_sf(expand = F)+
  ggspatial::annotation_scale(location = 'bl')+
  annotate(geom="text",label="Montague\nIsland",
           x = 210400, y = 700000,size=3,color="black")+
  annotate(geom="text",label="Knight\nIsland",
           x = 183000, y = 722000,size=3,color="black")+
  annotate(geom="text",label="Chenega\nIsland",
           x = 166000, y = 718000,size=3,color="black")+
  annotate(geom="text",label="Prince William Sound",
           x = 207000, y = 725000,size=4,color="darkblue")+
  labs(x="",y="")

#PWS.map

library(shadowtext)
PWS.map <- ggplot(PWS) +
  geom_sf(fill = "lightgrey", alpha = .8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = alpha("azure", .5), colour = "azure"),
        legend.position = "top") +
  geom_sf(data = sf.sta, aes(fill = Creek), size = 4, shape = 21) +
  scale_fill_discrete(type = colorz) +
  coord_sf(expand = F) +
  ggspatial::annotation_scale(location = 'bl') +
  geom_shadowtext(aes(x = 210400, y = 700000, label = "Montague\nIsland"),
                  size = 3, fontface = "plain", color = "black", 
                  bg.color = "white", halo.size = 1, halo.colour = "white") +
  geom_shadowtext(aes(x = 183000, y = 722000, label = "Knight\nIsland"),
                  size = 3, fontface = "plain", color = "black", 
                  bg.color = "white", halo.size = 1, halo.colour = "white") +
  geom_shadowtext(aes(x = 164000, y = 718000, label = "Chenega\nIsland"),
                  size = 3, fontface = "plain", color = "black", 
                  bg.color = "white", halo.size = 1, halo.colour = "white") +
  annotate(geom="text",label="Prince William Sound",
           x = 207000, y = 725000,size=4,color="darkblue")+
  labs(x="", y="")

PWS.map

ggdraw() +
  draw_plot(PWS.map)+
  draw_plot(ak.map, x = 0.74, y = 0.53, width = 0.25, height = 0.25)


