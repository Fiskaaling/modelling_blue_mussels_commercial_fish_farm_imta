# import the mussel particle summaries

library(tidyverse)

# function to move files into summaries folder
move_summaries <- function(ferdir, krak_navn, speedfactor, foldertomoveto) {
  
  paths <- data.frame(filename = character(),
                      ferd = character(),
                      kraknavn = character(),
                      speedfactor = character())
  
  for(k in krak_navn) {
    
    for(i in ferdir){
      path <- data.frame(filename = paste0("data/processed/ferd", i, 
                                           "/ferd_", i,
                                           "_krak_", k,
                                           "_sens_", speedfactor, "_summary.csv"),
                         newfile = paste0(foldertomoveto,
                                          "ferd_", i,
                                          "_krak_", k,
                                          "_sens_", speedfactor, "_summary.csv")
      )
      
      paths <- paths %>% 
        rbind(path)
      
    }
  }
  
  file.copy(from = paths$filename, to = paths$new, overwrite = TRUE)
  
}


kraksamla <- function(folderpath, ferdir, krak_navn, speedfactor){
  
  dir.create(path = "data/processed/sensitivities", showWarnings = FALSE)
  
  paths <- data.frame(filename = character(),
                      ferd = character(),
                      kraknavn = character(),
                      speedfactor = character())
  
  for(k in krak_navn){
    
    for(i in ferdir){
      path <- data.frame(filename = paste0(folderpath,
                                           "ferd_", i,
                                           "_krak_", k,
                                           "_sens_", speedfactor, "_summary.csv"),
                         ferd = i,
                         kraknavn = k,
                         speedfactor = speedfactor)
      paths <- paths %>% 
        rbind(path)
      
    }
    
    data <- paths %>% 
      mutate(isfile = file.exists(filename))
    
    manglar <- data %>% 
      filter(isfile == FALSE)
    
    if(nrow(manglar) >0){
      cat("\n")
      cat(paste0("eingin útrokning er gjørd fyri søkkiferðirnar: "))
      cat(paste(manglar$ferd, collapse = ", "))
      cat("\n")
      cat("\n")
    }
    
    if(nrow(manglar) < nrow(data)) {
      data <- data %>% 
        filter(isfile == TRUE) %>% 
        mutate(data = map(filename, read_csv, col_types = cols(), col_names = c("mm", "count"), skip = 1)) %>% 
        unnest(cols = data) %>% 
        select(-filename, -isfile)
      
      write_csv(data, paste0("data/processed/sensitivities/krak_particle_summary_", speedfactor, ".csv"))
      
    } else {
      cat("\n")
      cat(paste0("eingin útrokning er gjørd fyri kræklingaaliøki: ", krak_navn))
      cat("\n")
      cat("\n")
    }
    
  }
  
}


# at zero

sensitivity_at_zero <- function(ferdir, krak_navn, totalparticles, kraklength = NULL){
  
  if(is.null(kraklength)){
    kraklength <- c(4990,10010)
  }
  
  krakdf <- data.frame(kraknavn = c(krak_navn,krak_navn),
                       mm = c(kraklength,0,0),
                       count = c(0, totalparticles, totalparticles, 0))
  
  data <- data.frame(ferd = ferdir,
                     speedfactor = as.character("0.00"))
  
  data <- merge(krakdf, data) %>% 
    select(ferd, kraknavn, speedfactor, mm, count)
  
  write_csv(data, paste0("data/processed/sensitivities/krak_particle_summary_0.00.csv"))
  
}



krakingested <- function(ferdir,
                         speedfactor,
                         FRv
) {
  
  # data frame møguligum ferðum, býti, speedfactor og FRv
  
  options <- data.frame(expand.grid(ferdir, speedfactor, FRv)) %>%
    rename(ferd = Var1, 
           speedfactor = Var2,
           FRv = Var3)
  
  # innles summary data
  #data frá kraksamla()
  
  paths <- data.frame(filename = paste0("data/processed/sensitivities/krak_particle_summary_", speedfactor,".csv"))
  #paths <- paste0("data/processed/summaries/krak_particle_summary_", speedfactor,".csv")
  
  #initiate dataframe
  df <- data.frame(kraknavn = character(),
                   ferd = character(),
                   Sv = numeric(),
                   speedfactor = numeric(),
                   FRv = numeric(),
                   N_dropped = numeric(),
                   N_kraklingaoki = numeric(),
                   N_ingested = numeric(),
                   frac_ingested = numeric())
  
  for(i in 1:nrow(paths)){
    
    data <- paths %>% 
      slice(i) %>% 
      mutate(isfile = file.exists(filename)) %>% 
      filter(isfile == TRUE) %>% 
      mutate(data = map(filename, read_csv,
                        col_types = cols(ferd = col_character(),
                                         kraknavn = col_character(),
                                         speedfactor = col_character()
                        )
      )
      ) %>% 
      unnest(cols = data) %>% 
      select(-filename, -isfile) %>% 
      mutate(z = mm/1000,
             Sv = as.numeric(ferd)/1000) %>% 
      left_join(options, by = c("ferd", "speedfactor")) %>% 
      mutate(n_ingested = count * (1-exp(-FRv*z/Sv))) %>% 
      group_by(kraknavn, ferd, Sv, speedfactor, FRv) %>% 
      summarise(N_dropped = sum(count),
                N_kraklingaoki = sum(count[z !=0]),
                N_ingested = sum(n_ingested),
                frac_ingested = sum(n_ingested)/N_dropped) %>% 
      ungroup() %>% 
      mutate(speedfactor = as.numeric(speedfactor))
    
    df <- df %>% rbind(data)
    
    gc()
    
  }
  
  df
  
}

krakassimilated_individual <- function(AE, dataramma){
  
  data <- dataramma %>% 
    mutate(AE = AE) %>% 
    group_by(kraknavn, speedfactor, FRv, ferd) %>% 
    mutate(frac_assimilated = AE * frac_ingested)
  
}

krakassimilated <- function(bytidf, AE, dataramma){
  
  optionsAE <- data.frame(expand.grid(unique(bytidf$bytinavn), AE)) %>% 
    rename(bytinavn = Var1,
           AE = Var2)
  
  bytioptions <- bytidf %>% 
    left_join(optionsAE, by = "bytinavn")
  
  
  data <- dataramma %>% 
    left_join(bytioptions, by = "ferd") %>% 
    group_by(kraknavn, speedfactor, FRv, bytinavn, AE, N_dropped) %>% 
    mutate(frac_assimilated = AE*sum(alpha_Sv*frac_ingested),
           N_kraklingaoki = sum(alpha_Sv*N_kraklingaoki),
           byti = paste0(alpha_Sv, collapse = ", "),
           ferdir = paste0(ferd, collapse = ", ")) %>% 
    distinct(kraknavn, speedfactor, FRv, ferdir, byti, bytinavn, AE, N_dropped, N_kraklingaoki, frac_assimilated)
  
}
