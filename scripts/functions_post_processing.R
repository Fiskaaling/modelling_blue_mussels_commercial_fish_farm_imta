# functions

# These are the functions needed for the post processing of the particle
# tracking model. For calculating how many particles enter the different
# blue mussels farms and for the particle distributions at a certain depth.
# the comments and variable names are in a mixture of Faroese and English ;)



# R packages --------------------------------------------------------------

library(tidyverse)
library(sf)
library(purrr)
options(dplyr.summarise.inform = FALSE)


# geospatial - rotation functions -----------------------------------------

# rotation um punkt xc,yc
xrot <- function(x, y, rotang, xc, yc){
  (x - xc) * cos(rotang * pi / 180) - (y - yc) * sin(rotang * pi / 180) + xc
}

yrot <- function(x, y, rotang, xc, yc){
  (x - xc) * sin(rotang * pi / 180) + (y - yc) * cos(rotang * pi / 180) + yc
}


# rotate u and v in same dataframe

dfrotation <- function(df, maxbin, rotang){
  
  out <- df %>% select(starttid)
  
  for (i in 0:maxbin){
    
    columns <- c(paste0(i, x), paste0(i, y))
    
    dfrot <- df %>% 
      select(starttid, all_of(columns)) %>% 
      rename(x0 = columns[1],
             y0 = columns[2]) %>% 
      mutate(!! columns[1] := xrot(x0, y0, rotang, 0, 0),
             !! columns[2] := yrot(x0, y0, rotang, 0, 0)) %>% 
      select(-x0, -y0)
    
    out <- out %>% left_join(dfrot, by = "starttid")
    
  }
  i <- i+1
  
  out
}

dfrotation_parallel <- function(df, i, rotang) {
  
  columns <- c(paste0(i, x), paste0(i, y))
  
  dfrot <- df %>% 
    select(all_of(columns)) %>% 
    rename(x0 = columns[1],
           y0 = columns[2]) %>% 
    mutate(!! columns[1] := xrot(x0, y0, rotang, 0, 0),
           !! columns[2] := yrot(x0, y0, rotang, 0, 0)) %>% 
    select(-x0, -y0)
  
}


# geospatial - set up propping points in fish cages -----------------------

# uppseting av griddi inni í aliringunum
uppset_grid_ringar <- function(size) {
  
  #import aliringar
  ringar <- read_rds("data/processed/ringar.rds")
  
  # grid inn í ringarnar
  
  grid_spacing_ringar <- size
  
  ring_grid <- st_make_grid(ringar,
                            cellsize = c(grid_spacing_ringar, grid_spacing_ringar),
                            what = "polygons",
                            square = TRUE) %>% 
    st_intersection(ringar) %>% 
    st_sf() %>% 
    mutate(area = st_area(geometry)) %>% 
    filter(as.numeric(area) == (grid_spacing_ringar * grid_spacing_ringar))
  
  # centeroid í griddinum í ringunum verður funnið
  # partiklar verða sleptir í hvørjum centeroid
  
  ringgrid_cent <- ring_grid %>% 
    st_centroid()
  
  dfgrid <- ringgrid_cent %>% 
    st_cast("POINT") %>% 
    mutate(x := unlist(map(geometry,1)),
           y := unlist(map(geometry,2))) %>% 
    data.frame() %>% 
    select(-geometry, -area) %>% 
    rownames_to_column() %>% 
    mutate(rowname = as.numeric(rowname))
  
  write_rds(ring_grid, paste0("data/processed/ring_grid_", grid_spacing_ringar, ".rds"))
  write_rds(dfgrid, paste0("data/processed/dfgridring_", grid_spacing_ringar, ".rds"))
  
  rm(ringar, ring_grid, ringgrid_cent, dfgrid)
}



# stitching data together -------------------------------------------------

# stitch bin files files together

stitch_bins <- function(speed, binstoplot){
  
  for(v in 1:length(speed)) {
    
    ferd <- speed[v]
    
    for(i in 1:length(binstoplot)) {
      
      j <- binstoplot[i]
      
      if (i == 1) {
        
        t <- read_rds(paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_bin_", j, ".rds")) %>% 
          mutate(bin = j)
        
      } else {
        
        t1 <- read_rds(paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_bin_", j, ".rds")) %>% 
          mutate(bin = j)
        
        t <- t %>% 
          bind_rows(t1)
      }
      
    }
    write_rds(x = t, paste0("data/processed/summaries/ferd_", ferd, "_bin_all.rds"))
  }
}


# stitch summary files files together

stitch_summary <- function(speed, binstoplot) {
  
  for(v in 1:length(speed)){
    
    ferd <- speed[v]
    
    for(i in 1:length(binstoplot)) {
      
      j <- binstoplot[i]
      
      if (i == 1) {
        
        t <- read_rds(paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_summary_bin_", j, ".rds")) 
        
      } else {
        
        t1 <- read_rds(paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_summary_bin_", j, ".rds")) 
        
        t <- t %>% 
          bind_rows(t1)
      }
      
    }
    
    write_csv(x = t, paste0("data/processed/summaries/ferd_", ferd, "_bin_summaries.csv"))
  }
}

# functión til at samla allar søkkiferðirnar við tí uppgivna býtinum

pdms_speeds_proportions <- function(ferdir,
                                    byti) {
  
  df_summaries <- data.frame(v = ferdir,
                             alpha = byti) %>% 
    mutate(filepath = paste0("data/processed/summaries/ferd_", v, "_bin_summaries.csv"),
           is_file = file.exists(filepath)) %>% 
    filter(is_file == TRUE) %>% 
    group_by(filepath) %>% 
    mutate(contents = map(filepath, read_csv, col_types = cols(speed = col_character()))) %>% 
    unnest(cols = c(contents)) %>% 
    mutate(percent_boundary = p_bound_total/p_total) %>% 
    ungroup()
  
  df_speeds <- data.frame(v = ferdir,
                          alpha = byti) %>% 
    mutate(filepath = paste0("data/processed/summaries/ferd_", v, "_bin_all.rds"),
           is_file = file.exists(filepath)) %>% 
    filter(is_file == TRUE) %>% 
    group_by(filepath) %>% 
    mutate(contents = map(filepath, read_rds)) %>% 
    unnest(cols = c(contents)) %>% 
    ungroup() %>% 
    left_join(df_summaries %>% select(v, alpha, bin, p_total, percent_boundary), 
              by = c("v", "alpha", "bin")) %>%
    ungroup() %>% 
    mutate(count = (n * alpha)) %>% 
    group_by(bin, x, y, count, p_total) %>% 
    summarise(dens_byti = sum(count)/p_total) %>% 
    distinct(bin, x,y,count, dens_byti) %>% 
    ungroup()
  
  write_rds(x = df_speeds, paste0("data/processed/summaries/bins_particle_deposition_speeds_",
                                  paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                                  ".rds"))
  
}



# Particles through blue mussel farm --------------------------------------

# arbeiði í wide form í staðin...

particles_kraklingaaling <- function(data, 
                                     ringdrop, 
                                     ferd, 
                                     krak_alioki_rotated, 
                                     krak_navn, 
                                     dfgrid_rotated,
                                     maxbin,
                                     x, 
                                     y,
                                     binfrom,
                                     binto,
                                     binlength,
                                     sensitivity) {
  
  # listi í environment sum ikki skal slettast tá functiónin hevur koyrt
  freeze <- ls()
  
  bins <- paste0("bin", binfrom:binto)
  
  # koordinatar á sleppingini
  gridno <- dfgrid_rotated %>% filter(rowname == ringdrop)
  
  for(i in sensitivity){
    
    dfx <- data %>% 
      select(matches(!! x)) %>% 
      data.matrix()
    
    dfy <- data %>% 
      select(matches(!! y)) %>% 
      data.matrix()
    
    # sensitivity analysis
    
    dfx <- i * dfx
    dfy <- i * dfx
    
    # flutt á positión k
    dfx <- dfx + gridno$x1
    dfy <- dfy + gridno$y1
    
    # kanna um x og y eru inni í kræklingaalibrúkinum
    
    dfx <- (dfx > min(krak_alioki_rotated$x1)) & (dfx < max(krak_alioki_rotated$x1))
    dfy <- (dfy > min(krak_alioki_rotated$y1)) & (dfy < max(krak_alioki_rotated$y1))
    
    out <- dfx == TRUE & dfy == TRUE
    colnames(out) <- bins
    
    out <- out %>% 
      data.frame() %>% 
      #select(all_of(bins_to_summarise)) %>%
      #data.matrix() %>% 
      rowSums(na.rm = TRUE) %>% 
      data.frame(bins_no = .) %>% 
      group_by(bins_no) %>% 
      summarise(count = n()) %>%
      mutate(mm = bins_no * binlength * 1000) %>% 
      select(mm, count) %>% 
      ungroup() %>% 
      arrange(mm)
    
    i_text <- sprintf("%.2f", i)
    
    # write to disk
    write_rds(x = out, paste0("data/processed/ferd", ferd,
                              "/particles/ferd_", ferd, 
                              "_krak_", krak_navn, 
                              "_sens_", i_text,
                              "_ringdrop_", ringdrop, ".rds"))
    
  }
  
  #sletta variablar, uttan freeze
  rm(list = setdiff(ls(), freeze))
  gc()
}


# functión til telja hvussu nógvir partiklar fara í gjøgnum hvussu nógvar bins

summary_kraklinga_particles <- function(ferd,
                                        krak_navn,
                                        number_ringdrops,
                                        number_particles_in_drop,
                                        deep,
                                        binlength,
                                        sensitivity){
  
  totalparticles <- number_particles_in_drop*number_ringdrops
  
  for (j in sensitivity) {
    
    # start dataframe
    p1 <- data.frame(mm = as.character(seq(0,deep*1000, binlength*1000)),
                     count = as.numeric(0))
    
    #create progress bar
    pb <- txtProgressBar(min = 0, max = number_ringdrops, style = 3)
    
    for(i in 1:number_ringdrops){
      
      # filepath  
      ##fílarnar vera gjørdar í func_particle_deposition.R
      
      path <- paste0("data/processed/ferd", ferd, 
                     "/particles/ferd_", ferd, 
                     "_krak_", krak_navn, 
                     "_sens_", j,
                     "_ringdrop_", i, ".rds")
      
      dfdrop <- read_rds(path) %>% 
        data.frame() %>% 
        mutate(count = as.numeric(count),
               mm = as.character(mm))
      
      # readfile and summarise and add to p1  
      p1 <- p1 %>% 
        full_join(dfdrop, by = "mm") %>% 
        mutate(count = rowSums(across(count.x:count.y), na.rm = TRUE)) %>% 
        ungroup() %>%    
        select(mm, count) %>% 
        arrange(mm)
      
      # update progress bar
      setTxtProgressBar(pb, i)
      
    }
    
    # save p1 as csv
    
    write_csv(p1, paste0("data/processed/ferd", ferd, 
                         "/ferd_", ferd, 
                         "_krak_", krak_navn, 
                         "_sens_", j,
                         "_summary.csv"))
    
    
    #close progress bar
    close(pb)
    
  }
  
  
}



# Particle deposition - all cages -----------------------------------------

# functión til at seta pdm út á aliringar

particle_deposition <- function(binno,
                                data,
                                dfgrid,
                                ferd,
                                boundarybox,
                                boundarygrid,
                                x,
                                y) {
  
  # listi í environment sum ikki skal slettast tá functiónin hevur koyrt
  freeze <- ls()
  
  # filter data
  
  columns <- c(paste0(binno, x), paste0(binno, y))
  
  dffilt <- data %>%
    select(starttid, all_of(columns)) %>% 
    rename(x = columns[1],
           y = columns[2]) %>% 
    mutate(bin = binno) %>%
    ungroup()
  
  
  # grid verður sett upp í boyndary box, við teimum ymisku støddunum
  
  xbreaks <- seq(boundarybox[[1]],boundarybox[[3]], boundarygrid)
  xlabels <- xbreaks + boundarygrid/2
  xlabels <- xlabels[-length(xbreaks)]
  ybreaks <- seq(boundarybox[[2]],boundarybox[[4]], boundarygrid)
  ylabels <- ybreaks + boundarygrid/2
  ylabels <- ylabels[-length(ybreaks)]
  
  binsx <- data.frame(x = c(1:length(xlabels)),
                      xbreaks = xlabels)
  binsy <- data.frame(y = c(1:length(ylabels)),
                      ybreaks = ylabels)
  
  # total number of particles in bin put on ring_grid
  
  ptotal <- df %>% nrow() %>% as.numeric() * max(dfgrid$rowname)
  
  # talva til at fylla út, við tal av partiklum
  
  totals_summary <- data.frame(bin = binno,
                               speed = ferd,
                               p_total = ptotal,
                               grid_m = boundarygrid,
                               p_bound_total = 0,
                               p_bin_total = 0)
  
  
  # partiklar verða sleptir sambært uppsetta griddinum inni í ringunum
  
  for(k in 1:nrow(dfgrid)){
    
    # koordinatar á sleppingini
    gridno <- dfgrid %>% filter(rowname == k)
    
    # freefall verða fluttir á positión k
    dfmoved <- dffilt %>% 
      mutate(x = x + gridno$x,
             y = y + gridno$y,
             rowname = gridno$rowname)
    
    p_boundary <- dfmoved %>% 
      filter(between(x, boundarybox[[1]], boundarybox[[3]]) & between(y, boundarybox[[2]], boundarybox[[4]])) %>% 
      nrow() %>% 
      as.numeric()
    
    #prøva at einkla hvørt lag
    
    # freq <-  as.data.frame(table(findInterval(dfmoved$u, xbreaks),findInterval(dfmoved$v, ybreaks)))
    # 
    # xbin = cut(dfmoved$u, breaks = xbreaks, include.lowest = TRUE, labels = xlabels)
    # ybin = cut(dfmoved$v, breaks = ybreaks, include.lowest = TRUE, labels = ylabels)
    
    
    
    
    # particlar verða binnaðir sambært boundary box griddi og upptaldir
    
    binned <- dfmoved %>% 
      mutate(xbin = cut(x, breaks = xbreaks, include.lowest = TRUE, labels = xlabels),
             ybin = cut(y, breaks = ybreaks, include.lowest = TRUE, labels = ylabels)) %>% 
      filter(!is.na(xbin)) %>% 
      filter(!is.na(ybin)) %>% 
      select(xbin, ybin) %>% 
      group_by(xbin, ybin) %>% 
      tally() %>% 
      ungroup() %>% 
      mutate(x = as.numeric(xbin), # - 1 + grid[[1]] + gridsize/2,
             y = as.numeric(ybin), # - 1 + grid[[2]] + gridsize/2,
             n = as.numeric(n)) %>% 
      left_join(binsx, by = c("x")) %>% 
      left_join(binsy, by = c("y")) %>% 
      select(xbreaks, ybreaks, n) %>% 
      rename(x = xbreaks,
             y = ybreaks)
    
    # ein uppteljing av tí "binnaða" datanum
    
    p_binned_total <- binned %>%
      filter(between(x, 
                     round(boundarybox[[1]], 0) + boundarygrid/2, 
                     round(boundarybox[[3]], 0) - boundarygrid/2) & 
               between(y, 
                       round(boundarybox[[2]], 0) + boundarygrid/2,
                       round(boundarybox[[4]], 0) - boundarygrid/2)) %>% 
      summarise(p = sum(n)) %>% 
      .$p %>% 
      as.numeric()
    
    # uppdatera summary talvu
    totals_summary <- totals_summary %>% 
      mutate(p_bound_total = if_else(bin == binno, p_bound_total + p_boundary, p_bound_total),
             p_bin_total = if_else(bin == binno, p_bin_total + p_binned_total, p_bin_total))
    
    
    # data verður útskrivað
    
    
    if(k == 1){
      # skriva binned data
      write_rds(x = binned, paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_bin_", binno, ".rds"))
      rm(binned)
      
      # skriva summary talvu
      # write_rds(x = totals_summary, paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_summary_bin_", binno, ".rds"))
      
    } else {
      # skriva binned data, sum n
      binned0 <- read_rds(paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_bin_", binno, ".rds"))
      
      binned <- binned0 %>% 
        full_join(binned, by = c("x", "y")) %>% 
        mutate(n.x = if_else(is.na(n.x), 0, n.x),
               n.y = if_else(is.na(n.y), 0, n.y),
               n = n.x + n.y) %>% 
        select(x, y, n)
      
      write_rds(x = binned, paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_bin_", binno, ".rds"))
      
      rm(binned0, binned)
      
    }
    
    # skriva summary talvu
    write_rds(x = totals_summary, paste0("data/processed/ferd", ferd, "/ferd_", ferd, "_summary_bin_", binno, ".rds"))
    
  }
  
  #sletta variablar, uttan freeze
  rm(list = setdiff(ls(), freeze))
  
}


# Plot function for deposition distribution with kde ----------------------

# function to plot contour plot of the particle deposition for each bin
library(tidyverse)


plot_pdms <- function(binno,
                      data,
                      bin_m,
                      binstart_m,
                      scale_min,
                      scale_max,
                      breaks,
                      show_kde = FALSE,
                      sf_aliringar,
                      sf_krak1,
                      from_meters1,
                      to_meters1,
                      sf_krak2,
                      from_meters2,
                      to_meters2,
                      ferdir,
                      byti){
  
  # Number of breaks in contour scale
  breaks <- breaks
  
  # breaks
  prob <- c(0.995, 0.95, 0.90, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
  problabels <- c(0.95)
  
  dffilt <- data %>% 
    mutate(binmeters = bin * bin_m + binstart_m)
  
  # depth of current bin
  depth <- dffilt %>% distinct(binmeters)
  
  if(show_kde) {
    df2 <- with(dffilt, dffilt[rep(1:nrow(dffilt), count),])
    df3 <- df2 %>% sample_n(1000)
    
    #dens  <- MASS::kde2d(df3$x, df3$y, n = 30)
    #kd <- data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z))
    
    kd <- df2 %>% 
      sample_n(500000 ) %>% 
      group_by(binmeters) %>% 
      nest() %>% 
      mutate(kdmodel = map(data, ~MASS::kde2d(x = .$x, y= .$y, n = 50)),
             kddf = map(kdmodel, broom::tidy),
             kddf = map(kddf, data.frame)
      ) %>% 
      select(binmeters, kddf) %>%
      unnest(cols = kddf)
    
    
    kdprob <- function(kd, prob) {
      kd %>% 
        group_by(binmeters) %>% 
        nest() %>% 
        mutate(percent = list(prob),
               prob = list(1-prob),
               dx = map_dbl(data, ~diff(unique(.$x))[1]),
               dy = map_dbl(data, ~diff(unique(.$y))[1]),
               sz = map(data, ~sort(.$z)),
               c1 = map(sz, ~{cumsum(.x)*dx*dy}),
               lev = pmap(list(x = c1, y = sz, xout = prob), approx),
               lev = map(lev, pluck("y"))) %>% 
        select(binmeters, percent, lev) %>% 
        unnest(cols = c(percent, lev))
    }
    
    #problines <- kdprob(kd, prob)
    problineslabels <- kdprob(kd, problabels)
  }
  
  # Establish the min and max of scale 
  grandmin <- scale_min
  grandmax <- scale_max
  
  # Define the number of breaks. In this case breaks + 1
  mybreaks <- seq(0, grandmax, length.out = breaks + 1)
  
  # Function to return the desired number of colors
  
  #mycolours <- viridis::viridis(length(mybreaks)) #doesn't work on linux :(
  mycolours <- c("#440154FF", "#481A6CFF", "#472F7DFF", "#414487FF",
                 "#39568CFF", "#31688EFF", "#2A788EFF", "#23888EFF",
                 "#1F988BFF", "#22A884FF", "#35B779FF", "#54C568FF",
                 "#7AD151FF", "#A5DB36FF", "#D2E21BFF", "#FDE725FF")
  #alphacol <- viridis::viridis(length(mybreaks), alpha = 0.95)
  alphacol <- c("#440154F2", "#481A6CF2", "#472F7DF2", "#414487F2",
                "#39568CF2", "#31688EF2", "#2A788EF2", "#23888EF2",
                "#1F988BF2", "#22A884F2", "#35B779F2", "#54C568F2",
                "#7AD151F2", "#A5DB36F2", "#D2E21BF2", "#FDE725F2")
  mycolours1 <- alphacol
  mycolours1[1] <- mycolours[1]
  mycolours1 <- mycolours
  
  #filter out data
  dffilt <- dffilt #%>% 
  #filter(dens_byti_levels > grandmin)
  
  plot <- ggplot() +
    geom_contour_filled(data = dffilt, aes(x = x, y = y, z = (dens_byti)),
                        show.legend = TRUE,
                        breaks = (mybreaks)
    ) +
    {if(show_kde)
      geom_contour(data = kd, aes(x = x, y = y, z = z), 
                   breaks = problineslabels$lev, 
                   colour = "white", 
                   linetype = 2,
                   size = 0.3, 
                   show.legend = FALSE) 
    } +
    scale_fill_manual(values=mycolours1,
                      drop=FALSE,
                      guide = guide_colorsteps(ticks = FALSE,
                                               direction = "vertical",
                                               frame.colour = "white"
                      )
    ) +
    {if(show_kde) 
      metR::geom_text_contour(data = kd, 
                              aes(y=y, x=x, z=z,
                                  label = factor(..level.., 
                                                 labels = paste(problineslabels$percent*100, "%"))),
                              breaks = problineslabels$lev, 
                              colour = "black", 
                              size = 2, 
                              stroke = 0.1)
    }+
    
    geom_text(aes(x = 180009, y = 885922, label = "Cages"),
              colour = "white", angle = -7, size = 2) +
    geom_sf(data = sf_aliringar, fill = NA, color = "white", size = 0.2) +
    {if(unique(dffilt$binmeters) >= from_meters1 & unique(dffilt$binmeters) <= to_meters1) 
      geom_sf(data = sf_krak1, fill = NA, colour = "pink", 
              size = 0.2, linetype = 1)} +
    {if(depth$binmeters >= from_meters1 & depth$binmeters <= to_meters1)
      geom_text(aes(x = 180009, y = 885820, label = "Surface Mussel Farm"),
                colour = "pink", angle = -7, size = 2)} +
    {if(depth$binmeters >= from_meters2 & depth$binmeters <= to_meters2)
      geom_sf(data = sf_krak2, fill = NA, color = "pink",
              size = 0.2, linetype = 1)} +
    {if(depth$binmeters >= from_meters2 & depth$binmeters <= to_meters2)
      geom_text(aes(x = 180009, y = 885840, label = "Submerged Mussel Farm"),
                colour = "pink", angle = -7, size = 2)} +
    geom_text(aes(x = 179743, y = 885746, label = paste0(depth, " m")),
              colour = "white", hjust = 0, vjust = 0, size = 3) +
    coord_sf(xlim = c(179693 + 30, 180325 - 30), ylim = c(885606 + 120 , 886238 - 120), expand = FALSE, crs = 5316)+
    theme_void(base_size = 8) +
    #facet_grid(binmeters~.)+
    theme(legend.position = c(0.95,0.5),
          legend.text = element_blank(),
          #legend.text = element_text(hjust = 1, size = 5),
          legend.key.width = unit(0.4, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.title = element_blank(),
          #legend.background = element_rect(fill = alpha("white", 0.1), colour = NA),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  plot
  
  
  ggsave(plot = plot, filename = paste0("images/density_plots/speeds_combined_pdm_",
                                        paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                                        "_bin_", paste(binno, collapse = "_"), ".png"),
         units = "mm",
         width = 81, height = 81)
  
  ggsave(plot = plot, 
         filename = paste0("images/density_plots/speeds_combined_pdm_",
                           paste0(ferdir, "at", str_remove_all(byti, "\\."), collapse = "_"),
                           "_bin_", paste(binno, collapse = "_"), ".pdf"),
         units = "mm",
         width = 81, height = 81)
  
}




