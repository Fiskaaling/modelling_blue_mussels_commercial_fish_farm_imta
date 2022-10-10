#import geodata Sørvág specific
library(tidyverse)
library(sf)
#library(concaveman)

crs_deg <- 4326
crs_m <- 5316

alioki <- c("A83")

zoomin <- data.frame(lon = c(-7.8, -6.15),
                     lat = c(61.35, 62.45))
zoomin_sf <- zoomin %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

#importera gdb fílir við geodata

#alipolyoki
aliokiA83 <- read_rds("data/alioki.rds") %>%
  filter(area_permit_name %in% alioki) %>% 
  st_transform(crs_deg) %>% 
  select(shape) %>% 
  mutate(name = "A83",
         type = "aliøki") %>% 
  st_transform(crs_m)

write_rds(aliokiA83, "data/alioki_sorvag.rds")

# this file contains the gps coordinates of the corner positions of the frame
alieind <- read_csv("data/geodata/Fish_farm_corner_positions.csv", col_types = cols()) %>% 
  arrange(row_no) %>% 
  st_as_sf(coords = c("lat", "lng")) %>% 
  st_set_crs(crs_deg) %>%
  mutate(name = "laksa alieind",
         type = "alieind") %>% 
  group_by(name, type) %>% 
  summarise() %>% 
  st_cast("MULTIPOINT") %>% 
  st_convex_hull() %>% 
  #concaveman() %>% 
  st_transform(crs_m)

write_rds(alieind, "data/alieind.rds")

#dyb cropped 
dyb <- read_rds("data/dyb10.rds") %>% 
  st_transform(crs_deg) %>% 
  sf::st_crop(zoomin_sf)

write_rds(dyb, "data/dyb10_allar.rds")

#oyggjar
oyggjar <- read_rds("data/oyggjar.rds") %>%
  st_transform(crs_deg) %>% 
  sf::st_crop(zoomin_sf)

write_rds(oyggjar, "data/oyggjar_allar.rds")

#add rds fílir to .gitignore
df <- data.frame(gitignore = c("data/alioki_sorvag.rds",
                               "data/alieind.rds",
                               "data/dyb10_allar.rds",
                               "data/oyggjar_allar.rds"))

walk(df$gitignore, usethis::use_git_ignore)
