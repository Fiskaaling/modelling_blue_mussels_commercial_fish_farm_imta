# spjaðing av bitlum, parallel koyring

# fyrst verður kannað um neyðugu r pakkar eru installeraðir
source("scripts/required_r_packages.R", encoding = "utf-8")

# hvat skal koyrast?
# particlar í gjøgnum kræklingaalingabrúk?
kraklingar <- TRUE

# particle deposition frá laksaaling?
laksur <- TRUE
images <- TRUE
depositionplot_per_m <- 1

# skulu geodátur heintast av gitlab?
# bert neyðugt fyrstu ferð
geodata <- FALSE

# hvar liggja distribution files
folderpath <- ""
filetypes <- "csv"

# skal tú bara testa? um ikki so set testdrops = NA
testdrops <- NA

# hvussu nógvar cores
ncores <- 40

# neyðugar upplýsingar ----------------------------------------------------

# hvør/hvørjar søkkiferðir skal koyrast, vel "01", "15", "32" ella "75" (string)
ferdir <- c("01", "15", "32", "75")
byti <- c(0.05, 0.1, 0.2, 0.65)

# hvussu nógvar bins eru? (numeric) 
allar <- c(2399, 2399, 2399, 2399)

# hvussu nógvir metrar eru millum hvørja bin í spjaðingsmodellinum?
bin_m <- 0.01

# hvussu nógvir metrar svarar bin0 til í spjaðingsmodellinum?
binstart_m <- 5 + bin_m

# hvussu stórt skal hvørt grid verða inni í ringunum (m)?
# bitlar verða sleptir úr miðuni á hvørjari grid box
ringargrid <- 4

# hvussu stórur skal buffarin veðra rundan um ringarnar (m) ?
# hetta verður nýtt til boundary boxina, ið verður uppsett
boundarybuffer <- 150

# hvussu stórt skal hvørt grid verða inni í boundary (m)?
# hetta verður nýtt til uppteljing av bitlunum í økinum
boundarygrid <- 1

# hvussu nógvar gradir eru alieindirnar roteraðar?
# hetta veit man frá tá ramman varð teknað í "scripts/geosetup.R"
rotang <- 7

# hvussu eita "directions" í dátugrunninum?
x <- "u"
y <- "v"

# upplýsingar um kræklingaalingabrúkini

## kræklingaalibrúk nr. 1 (við síðurnar av alibrúkinum)
krak_navn1 <- "1"
from_meters1 <- 0
to_meters1 <- 10

## kræklingaalibrúk nr. 2 (undir alibrúkinum)
krak_navn2 <- "2"
from_meters2 <- 18
to_meters2 <- 28

# Speedfactor
speedfactor <- seq(0.1,4,0.1)
#speedfactor <- c(1.0)
speedfactor_text <- sprintf("%.2f", speedfactor)

# R packages --------------------------------------------------------------

library(tidyverse)
library(sf)
library(parallel)
library(foreach)
library(doParallel)

# create directories ------------------------------------------------------

dir.create("data/processed", showWarnings = FALSE)

# source scripts ----------------------------------------------------------

if(geodata == TRUE){
  source("scripts/get_geodata.R", encoding = "utf-8")
  source("scripts/initial_import_geodata.R", encoding = "utf-8")
}

# her verða ringar og møgulig kraklingaaliøki sett upp!
# hetta riggar ikki á ubuntu í løtuni :( [notat::2022-07-12]
#source("scripts/geosetup.R")

# source functions --------------------------------------------------------

source("scripts/functions_post_processing.R")


# koyring fyri hvørja søkkiferð -------------------------------------------

for(v in 1:length(ferdir)) {
  
  ferd <- ferdir[v]
  maxbin <- allar[v]
  binlength <- bin_m
  
  
  # create directories ------------------------------------------------------
  
  dir.create(paste0("data/processed/ferd", ferd), showWarnings = FALSE)
  dir.create(paste0("data/processed/summaries"), showWarnings = FALSE)
  
  if (kraklingar == TRUE){
    dir.create(paste0("data/processed/ferd", ferd, "/particles"), showWarnings = FALSE)
  }
  
  
  # uppset gridd í aliringunum ----------------------------------------------
  
  uppset_grid_ringar(ringargrid)
  
  # import data -------------------------------------------------------------
  
  # kræklingaaliøki 1
  krak1 <- read_rds("data/processed/alibruk1.rds")
  
  # kræklingaaliøki 1
  krak2 <- read_rds("data/processed/alibruk2.rds")
  
  # aliringar
  ringar <- read_rds("data/processed/ringar.rds")
  
  ringgrid <- read_rds(paste0("data/processed/ring_grid_", ringargrid, ".rds")) %>% 
    select(geometry)
  
  dfgrid <- read_rds(paste0("data/processed/dfgridring_", ringargrid, ".rds"))
  
  
  # boundary box verður sett upp her ----------------------------------------
  
  # boundary box um ringarnar
  grid <- st_bbox(ringgrid)
  grid[3] <- round(grid$xmax + boundarybuffer, 0)
  grid[4] <- round(grid$ymax + boundarybuffer, 0)
  grid[1] <- round(grid$xmin - boundarybuffer, 0)
  grid[2] <- round(grid$ymin - boundarybuffer, 0)
  xlength <- grid[[3]]-grid[[1]]
  ylength <- grid[[4]]-grid[[2]]
  
  
  # syrgja fyri at boundary box er kvadradisk
  if(xlength > ylength){
    grid[4] <- grid$ymax + (xlength - ylength)/2
    grid[2] <- grid$ymin - (xlength - ylength)/2
    ylength <- grid[[4]]-grid[[2]]
  } 
  
  if(ylength > xlength){
    grid[3] <- grid$xmax + (ylength - xlength)/2
    grid[1] <- grid$xmin - (ylength - xlength)/2
    xlength <- grid[[3]]-grid[[1]]
  }
  
  # hvussu nógv grids eru á hvørjari síðu?
  grids <- xlength/boundarygrid
  
  
  # Plot við ringunum og tí uppsettu boundary boxini ------------------------
  
  
  info1 <- paste0("søkkiferð = ", as.numeric(ferd)/10, " cm/s \n", 
                  "grid stødd í ringunum = ", ringargrid, " x ", ringargrid, " m2 \n",
                  "boundary box = ", grids*boundarygrid, " x ", grids*boundarygrid, " m2 \n",
                  "grid stødd í boundary box = ", boundarygrid, " x ", boundarygrid, " m2 \n")
  
  ggplot() +
    geom_sf(data = ringar) +
    geom_sf(data = ringgrid) +
    geom_sf(data = krak1, fill = NA) +
    geom_sf(data = krak2, fill = NA) +
    geom_sf(data = grid %>% st_as_sfc(), fill = NA) +
    geom_text(aes(label = info1, x = grid$xmin+(boundarygrid*8), y = grid$ymax), 
              hjust = 0, vjust = 1.1, size = 3) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 6))
  
  ggsave(paste0("data/processed/ferd",ferd, "/ferd_", ferd, ".png"), width = 5, height = 5)
  
  
  # bitlar inni í kræklingaaliøki -------------------------------------------
  
  if(kraklingar == TRUE) {
    
    
    # import data -------------------------------------------------------------
    
    if(filetypes == "csv") {
      df <- read_csv(paste0(folderpath, "out_0.0", ferd, ".csv"), col_types = cols(col_double()))
    } else {
      df <- read_rds(paste0(folderpath, "out00", ferd, ".rds"))
    }    
    
    # Rotate ------------------------------------------------------------------
    
    # Alt verður roterað, gradir = rotang um 0,0
    
    # rotera koordinatar á sleppingini
    dfgrid_rot <- dfgrid %>%
      mutate(x1 = xrot(x = x, y = y, rotang = rotang, xc = 0, yc = 0),
             y1 = yrot(x = x, y = y, rotang = rotang, xc = 0, yc = 0))
    
    # Rotera kræklingaalibrúkini
    
    krak1_rot <- krak1 %>% 
      st_cast("POINT") %>% 
      mutate(!!x := unlist(map(geometry,1)),
             !!y := unlist(map(geometry,2))) %>% 
      data.frame() %>% 
      select(-geometry) %>% 
      mutate(x1 = xrot(x = u, y = v, rotang = rotang, xc = 0, yc = 0),
             y1 = yrot(x = u, y = v, rotang = rotang, xc = 0, yc = 0))
    
    krak2_rot <- krak2 %>% 
      st_cast("POINT") %>% 
      mutate(!!x := unlist(map(geometry,1)),
             !!y := unlist(map(geometry,2))) %>% 
      data.frame() %>% 
      select(-geometry) %>% 
      mutate(x1 = xrot(x = u, y = v, rotang = rotang, xc = 0, yc = 0),
             y1 = yrot(x = u, y = v, rotang = rotang, xc = 0, yc = 0))
    
    # Rotera partiklar
    
    #df_rot <- dfrotation(df, maxbin, rotang)
    
    doParallel::registerDoParallel(max(detectCores()-20, detectCores()-1))
    
    df_rot <- foreach (i=0:maxbin, .combine = cbind
    ) %dopar% {
      dfrotation_parallel(df = df, i = i, rotang = rotang)
    }
    df_rot <- bind_cols(df[1], df_rot)
    
    stopImplicitCluster()
    
    rm(df)
    gc()
    
    # parallelisering av functiónini particles_kraklingaaling
    
    # kræklingaliøki nr. 1, liggur sum standard á 0 - 10 m dýpi
    # prepare data
    
    bintometer <- data.frame(bin = 0:maxbin,
                             m = 0:maxbin*binlength)
    
    binfrom = round((from_meters1 - binstart_m)/binlength)
    binto = round((to_meters1 - binstart_m)/binlength)
    
    # summerize over specified bins
    if(binto >= 0) {
      if(binto == 0) {
        binfrom <- 0
        bins_to_summarise <- c(paste0(0, x), paste0(0, y))
      } else { 
        if(binfrom < 0) {binfrom <- 0}
        if(binto > maxbin) {binto <- maxbin}
        if(binfrom == 0) {binto <- binto - 1}
        bins_to_summarise <- c(paste0(binfrom:binto, x), paste0(binfrom:binto, y))
      }
      
      
      #filter dataframe
      data <- df_rot %>% 
        select(starttid, all_of(bins_to_summarise))
      
      # Detect the number of available cores and create cluster
      cl <- parallel::makeCluster(min(detectCores()-1, ncores))
      # Activate cluster for foreach library
      doParallel::registerDoParallel(cl)
      
      foreach (i=1:min(nrow(dfgrid),testdrops, na.rm = TRUE), 
               .packages = c("dplyr", "readr")
      ) %dopar% {
        
        particles_kraklingaaling(data = data, 
                                 ringdrop = i, 
                                 ferd = ferd,
                                 krak_alioki_rotated = krak1_rot,
                                 krak_navn = krak_navn1,
                                 dfgrid_rotated = dfgrid_rot,
                                 maxbin = maxbin,
                                 x = x,
                                 y = y,
                                 binfrom = binfrom,
                                 binto = binto,
                                 binlength = binlength,
                                 sensitivity = speedfactor)
      }
    }
    
    # kræklingaliøki nr. 2, liggur sum standard á 18 - 28 m dýpi
    # prepare data
    binfrom = round((from_meters2 - binstart_m)/binlength)
    binto = round((to_meters2 - binstart_m)/binlength)
    
    # summerize over specified bins
    if(binfrom < 0) {binfrom <- 0}
    if(binto > maxbin) {binto <- maxbin}
    if(binfrom == 0) {binto <- binto - 1}
    bins_to_summarise <- c(paste0(binfrom:binto, x), paste0(binfrom:binto, y))
    
    if(binfrom < maxbin) {
      
      #filter dataframe
      data <- df_rot %>% 
        select(starttid, all_of(bins_to_summarise))
      
      foreach (i=1:min(nrow(dfgrid),testdrops, na.rm = TRUE), 
               .packages = c("dplyr", "readr")
      ) %dopar% {
        
        particles_kraklingaaling(data = data,
                                 ringdrop = i,
                                 ferd = ferd,
                                 krak_alioki_rotated = krak2_rot,
                                 krak_navn = krak_navn2,
                                 dfgrid_rotated = dfgrid_rot,
                                 maxbin = maxbin,
                                 x = x,
                                 y = y,
                                 binfrom = binfrom,
                                 binto = binto,
                                 binlength = binlength,
                                 sensitivity = speedfactor)
        
      }
      
    }
    
    parallel::stopCluster(cl)
    
    # summary av partiklum, ið fara í gjøgnum kræklinaalingabrúkini, í einari ávísari dýbd
    
    # kræklingaliøki nr. 1
    summary_kraklinga_particles(ferd = ferd,
                                krak_navn = krak_navn1,
                                number_ringdrops = min(nrow(dfgrid),testdrops, na.rm = TRUE),
                                number_particles_in_drop = nrow(df),
                                deep = to_meters1 - max(from_meters1, binstart_m),
                                binlength = binlength,
                                sensitivity = speedfactor_text
    )
    
    if(binfrom < maxbin) {
      
      # kræklingaliøki nr. 2
      summary_kraklinga_particles(ferd = ferd,
                                  krak_navn = krak_navn2,
                                  number_ringdrops = min(nrow(dfgrid),testdrops, na.rm = TRUE),
                                  number_particles_in_drop = nrow(df),
                                  deep = to_meters2 - max(from_meters2, binstart_m),
                                  binlength = binlength,
                                  sensitivity = speedfactor_text
                                  
      )
    }
  }
  
  # particle deposition frá laksaaling --------------------------------------
  
  if(laksur == TRUE) {
    
    depositionplot_per_m <- max(depositionplot_per_m, bin_m)
    
    # import data -------------------------------------------------------------
    
    if(filetypes == "csv") {
      df <- read_csv(paste0(folderpath, "out_0.0", ferd, ".csv"))
    } else {
      df <- read_rds(paste0(folderpath, "out00", ferd, ".rds"))
    }  
    
    binsmeters <- data.frame(bin = c(0:maxbin)) %>% 
      mutate(meters = binstart_m+bin_m*bin) %>% 
      filter(meters %in% c(seq(ceiling(binstart_m), max(meters), depositionplot_per_m))) %>% 
      .$bin
    
    # Detect the number of available cores and create cluster
    cl <- parallel::makeCluster(min(ncores, length(binsmeters)))
    
    # Activate cluster for foreach library
    doParallel::registerDoParallel(cl)
    
    # parallelisering av functiónini particles_deposition
    foreach(i = 1:length(binsmeters),
            .packages = c("tidyverse", "sf")) %dopar% {
              
              particle_deposition(binno = binsmeters[i],
                                  data = df,
                                  dfgrid = dfgrid,
                                  ferd = ferd,
                                  boundarybox = grid,
                                  boundarygrid = boundarygrid,
                                  x = x,
                                  y = y)
            }
    
    # Stop cluster to free up resources
    parallel::stopCluster(cl)
    
  }
  
}


# plot distributions in depth ---------------------------------------------


if(images == TRUE){
  
  maxbin <- max(allar)
  
  depositionplot_per_m <- max(depositionplot_per_m, bin_m)
  
  binsmeters <- data.frame(bin = c(0:maxbin)) %>% 
    mutate(meters = binstart_m+bin_m*bin) %>% 
    filter(meters %in% c(binstart_m, seq(ceiling(binstart_m), max(meters), depositionplot_per_m))) %>% 
    .$bin
  
  
  # stitch all the bins together
  stitch_bins(speed = ferdir, binstoplot = binsmeters)
  
  # stitch all the summary bins together
  stitch_summary(speed = ferdir, binstoplot = binsmeters)
  
  # samla particle depositions fyri hvørja ferð við tí uppgivna býtinum
  pdms_speeds_proportions(ferdir = ferdir,
                          byti = byti)
  
  
  df_speeds_combined <- read_rds(paste0("data/processed/summaries/bins_particle_deposition_speeds_",
                                        paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                                        ".rds")) %>% 
    filter(bin %in% binsmeters)
  
  dir.create("images/density_plots", showWarnings = FALSE, recursive = TRUE)
  dir.create("images/gifs", showWarnings = FALSE)
  
  ringar <- read_rds("data/processed/ringar.rds")
  krak1 <- read_rds("data/processed/alibruk1.rds")
  krak2 <- read_rds("data/processed/alibruk2.rds")
  
  cl <- parallel::makeCluster(min(detectCores()-80, length(binsmeters)))
  doParallel::registerDoParallel(cl)
  
  # parallelisering av functiónini plot_pdms
  
  foreach(i = 1:length(binsmeters),
          .packages = c("tidyverse", "sf")) %dopar% {
            
            plot_pdms(binno = binsmeters[i],
                      data = df_speeds_combined %>% filter(bin == binsmeters[i]),
                      bin_m = bin_m,
                      binstart_m = binstart_m,
                      scale_min = min(df_speeds_combined$dens_byti),
                      scale_max = max(df_speeds_combined$dens_byti),
                      breaks = 15,
                      show_kde = FALSE,
                      sf_aliringar = ringar,
                      sf_krak1 = krak1,
                      from_meters1 = from_meters1,
                      to_meters1 = to_meters1,
                      sf_krak2 = krak2,
                      from_meters2 = from_meters2,
                      to_meters2 = to_meters2,
                      ferdir = ferdir,
                      byti = byti)
          }
  
  # Stop cluster to free up resources
  parallel::stopCluster(cl)
  
  # create gif
  
  ## list file names
  imgs <- gtools::mixedsort(list.files(path = "images/density_plots/",
                                       pattern = paste0(".*",
                                                        paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                                                        ".*.png"), 
                                       full.names = TRUE))
  
  ## create animation
  gifski::gifski(png_files = imgs, 
                 gif_file = paste0("images/gifs/particle_deposition_speeds_combined_",
                                   paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                                   ".gif"), 
                 delay = 0.4, width = 500, height = 500)
  
}

#rm(list = ls())
gc()
