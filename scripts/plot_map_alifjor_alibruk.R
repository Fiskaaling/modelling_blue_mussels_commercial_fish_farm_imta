# kort av aliøkinum og alifjørðinum alibrúkinum

crs_deg <- 4326
crs_m <- 5316

ringar <- read_rds("data/processed/ringar.rds")

#dyb 
dyb_allar <- read_rds("data/dyb10_allar.rds") %>% 
  st_transform(crs_deg)

#oyggjar
oyggjar_allar <- read_rds("data/oyggjar_allar.rds") %>%
  st_transform(crs_deg) 

#streymmáting
streymmating <- read_csv("data/geodata/streymmating.csv", col_types = cols())


zoomin <- data.frame(lon = c(-7.42, -7.30),
                     lat = c(62.065, 62.095))
zoomin_sf <- zoomin %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

#dyb cropped to Sørvág
dyb <- dyb_allar %>% 
  st_transform(crs_deg) %>% 
  sf::st_crop(zoomin_sf)

#oyggjar
oyggjar <- oyggjar_allar %>%
  st_transform(crs_deg) %>% 
  sf::st_crop(zoomin_sf)


#litir frá 3D image
blatt1 <- "#5C6EA0"
blatt2 <- "#7E89B7"
blatt3 <- "#9CB3DC"
blatt4 <- "#E3F5FF"
reytt <- "#873C5C"
reytt2 <- "#a05c6e"
cages <- "white"
measurement <- "red"
complementary0 <- "#807149"
complementary <- "#a08e5c"
complementary1 <- "#b3a47c"


# colours
Winter_Wizard <- "#9DDBFF"
Diamond <- "#C3F4FE"
Ivory <- "#FEFDED"
Pearl <-  "#E8E1C9"
Turquoise_Green <- "#AAD1AC"
Tea_Green <- "#CBE7BE"

land <- Pearl
land_line <- Pearl
ocean <- blatt1
bathy <- blatt2


kort_foroyar <- ggplot() +
  geom_sf(data = dyb_allar, colour = bathy, size = 0.2) +
  geom_sf(data = oyggjar_allar, 
          fill = land, colour = land_line, size = 0.2) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm")) +
  geom_sf(data = zoomin_sf, fill = NA, colour = "darkred", size = 1.5) +
  geom_sf_text(data = zoomin_sf, aes(label = "b"), nudge_y = 0.05, nudge_x = -0.05, colour = "darkred", size = 6)+
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 8) +
  theme(panel.background = element_rect(fill = ocean, colour = ocean),
        panel.grid.major = element_line(color = NA, linetype = 2),
        #axis.ticks = element_line(colour = "grey20"),
        panel.ontop = FALSE,
        #axis.text = element_text(colour = "grey20"),
        axis.title = element_blank(),
        axis.text = element_blank()
  )

kort_foroyar


kort_fjord <- ggplot() +
  geom_sf(data = dyb, colour = bathy, size = 0.2) +
  geom_sf(data = oyggjar, 
          fill = land, colour = land_line, size = 0.2) +
  geom_point(data = streymmating, 
             aes(x = lng, y = lat, colour = "Current Measurement"),
             size = 1.5) +
  #geom_sf(data = aliokiA83, fill = NA, aes(colour = "alioki")) +
  #geom_sf(data = ncg, fill = NA, aes(colour = "alieind")) +
  #geom_sf(data = grd_rot, fill = NA, aes(colour = "Frame")) +
  geom_sf(data = ringar, fill = NA, aes(colour = "Fish Cages"), size = 0.4) +
  scale_colour_manual(values = c(measurement, cages), name = "") +
  guides(color = guide_legend(override.aes = list(shape = 15, 
                                                  linetype = 0, size = 2))) +
  ggspatial::annotation_scale(height = unit(0.2, "cm"), location = "br")+
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks = seq(62.06, 62.09, by = 0.01)) +
  theme_minimal(base_size = 8) +
  theme(axis.text = element_text(colour = "grey20"), 
        axis.title = element_blank(),
        axis.ticks = element_line(colour = "grey20"),
        legend.key.size = unit(0.5, 'lines'),
        legend.text = element_text(size = 8),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = blatt1, colour = blatt1),
        #panel.border = element_rect(colour = "darkred", fill = NA),
        legend.position = c(0.8,0.9))

kort_fjord

ggpubr::ggarrange(kort_foroyar, kort_fjord, widths = c(0.27,0.73))



ggsave("images/kort_fjord.pdf", plot = kort_fjord, units = "mm",
       width = 81, height = 31.5, bg = 'transparent')

ggsave("images/kort_fjord.png", plot = kort_fjord, units = "mm",
       width = 120, bg = 'transparent')
ggsave("images/kort_foroyar.png", plot = kort_foroyar, units = "mm",
       width = 80, bg = 'transparent')


