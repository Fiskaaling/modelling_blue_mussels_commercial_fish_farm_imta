# ramma, ringar, og kræklinga alibrúk verða upprættaði her!

library(tidyverse)
library(sf)

# all objects created after here will be deleted
freeze <- ls()

# this file contains the corner positions of the farming unit on site
# data variable called "alieind"
alieind <- read_rds("data/alieind.rds")

# ummáli á aliringunum
ringar_ummal <- 160

# mát á rammunum
grid_spacing = 70

# gradir sum ramman skal roterast í mun til alieindina
rotang = 7

# funktión til at rotera
rot = function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

# functión til at transformera og rotera
tran = function(geo, ang, center) {
  (geo - center) * rot(ang * pi / 180) + center
}

# rotation um punkt xc,yc
xrot <- function(x, y, rotang, xc, yc){
  (x - xc) * cos(rotang * pi / 180) - (y - yc) * sin(rotang * pi / 180) + xc
}

yrot <- function(x, y, rotang, xc, yc){
  (x - xc) * sin(rotang * pi / 180) + (y - yc) * cos(rotang * pi / 180) + yc
}

ncg = st_geometry(alieind)
cntrd = st_centroid(ncg)

# rotating sf object doesn't work on linux at the moment...??...
grd <- sf::st_make_grid(tran(ncg, -rotang, cntrd),
                        cellsize = c(grid_spacing, grid_spacing),
                        offset = st_bbox(ncg)[c("xmin", "ymin")] + c(10,30),
                        what = "polygons",
                        square = TRUE,
                        n = c(5,2))

# started to rewrite code to makee it linux friendly...
# ncg_rot <- ncg %>% 
#   st_cast("POINT") %>% 
#   st_coordinates() %>% 
#   data.frame() %>% 
#   mutate(lat = xrot(X, Y, rotang, cntrd[[1]][1], cntrd[[1]][2]),
#          lng = yrot(X, Y, rotang, cntrd[[1]][1], cntrd[[1]][2])) %>% 
#   select(lat, lng) %>% 
#   st_as_sf(coords = c("lat", "lng"), crs = 5316) %>% 
#   summarise() %>% 
#   st_cast("MULTIPOINT") %>% 
#   st_cast("POLYGON")
# 
# grd <- sf::st_make_grid(ncg_rot,
#                         cellsize = c(grid_spacing, grid_spacing),
#                         offset = st_bbox(ncg)[c("xmin", "ymin")] + c(10,30),
#                         what = "polygons",
#                         square = TRUE,
#                         n = c(5,2))

bb <- st_bbox(st_union(grd))

# rotera alirammu
grd_rot <- tran(grd, rotang, cntrd) %>% 
  st_set_crs(crs_m) %>% 
  st_sf()

# write the corrected corner positions

cornerwrite <- TRUE

if(cornerwrite){
  
  corners <- grd_rot %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_convex_hull() %>% 
    st_simplify(dTolerance = 0.00001) %>% 
    st_cast("MULTIPOINT") %>% 
    st_transform(4326)%>% 
    st_coordinates() %>% 
    data.frame() %>% 
    rename(lat = X,
           lng = Y) %>% 
    select(lat, lng)
  
  write_csv(corners, "data/geodata/frame_corner_positions.csv")
  
  rm(corners)
}


# tekna alibrúk

plotta <- FALSE

if(plotta == TRUE) {
  
  ggplot() +
    geom_sf(data = alieind, aes(color = "logged corner positions")) +
    #geom_sf(data = grd %>% st_set_crs(crs_m) %>% st_sf()) +
    geom_sf(data = grd_rot, aes(color = "rotated frame"),
            fill = NA, size = 1) +
    scale_color_manual(values = c("black", "darkred")) +
    #geom_sf(data = grid %>% st_as_sfc(), fill = NA) +
    #geom_text(aes(label = info1, x = grid$xmin+(boundarygrid*8), y = grid$ymax), 
    #          hjust = 0, vjust = 1.1, size = 3) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 6), 
          legend.position = "top")
}

# alibrúk eftir innaru langsíðu
# bb er boundary box á alirammuni teknað omanfyri

flyt <- 0
breidd <- 20

alibruk1 <- st_polygon(list(rbind(c(bb[[1]], bb[[2]]-flyt),
                                  c(bb[[3]], bb[[2]]-flyt),
                                  c(bb[[3]], bb[[2]]-flyt-breidd),
                                  c(bb[[1]], bb[[2]]-flyt-breidd),
                                  c(bb[[1]], bb[[2]]-flyt)
)
)) %>% 
  st_sfc(crs = crs_m)


# alibrúk undir alirammuni
enlarge <- 0

alibruk2 <- st_polygon(list(rbind(c(bb[[1]]-enlarge, bb[[2]]-enlarge),
                                  c(bb[[1]]-enlarge, bb[[4]]+enlarge),
                                  c(bb[[3]]+enlarge, bb[[4]]+enlarge),
                                  c(bb[[3]]+enlarge, bb[[2]]-enlarge),
                                  c(bb[[1]]-enlarge, bb[[2]]-enlarge)
)
)) %>% 
  st_sfc(crs = crs_m)

# rotera alibrúk
alibruk1_rot <- tran(alibruk1, rotang, cntrd) %>% 
  st_set_crs(crs_m) %>% 
  st_sf()

alibruk2_rot <- tran(alibruk2, rotang, cntrd) %>% 
  st_set_crs(crs_m) %>% 
  st_sf()

dir.create("data/processed", showWarnings = TRUE)

write_rds(grd_rot, "data/processed/ramma.rds")
write_rds(alibruk1_rot, "data/processed/alibruk1.rds")
write_rds(alibruk2_rot, "data/processed/alibruk2.rds")
write_rds(alibruk1, "data/processed/alibruk1_unrotate.rds")
write_rds(alibruk2, "data/processed/alibruk2_unrotated.rds")

# ringar verða teknaðir útfrá roteraðum griddi
ringar <- st_centroid(grd_rot) %>% st_buffer(dist = ringar_ummal/(2*pi))
write_rds(ringar, paste0("data/processed/ringar.rds"))

# delete objects generated in this script!
rm(list = setdiff(ls(), c(freeze))) 
