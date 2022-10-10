# Modelling waste assimilation by blue mussels within the spatial constraints of a commercial fish farm: implications for multitrophic aquaculture

scripts for: *particle tracking + waste assimilation by blue mussels*

## Particle tracking

The particle tracking model was written in python, run the file **01_freefall.py**

-   the conda environment file is located here: **env/freefall_environment.yml**
-   *data needed to run the script*:
    -   current measurement data

### Post-processing of the modelled particle distibution

The post-processing of the results from the particle tracking model was written in R, run the file **02_ptm_post_processing.R**

-   *data needed to run the script*:
    -   results from the particle tracking model above
    -   corner positions of the fish farm
    -   coastline data (for plotting)
    -   bathymetry data (for plotting)

## Waste assimilation by blue mussels

The waste assimilation by blue mussels was likewise written in R, run the Rmd file **03_uptake_calculations.Rmd**

-   *data needed to run the script*:
    -   results from the post-processing above

## Data availability

> **Note** *Data not included in this repository*  
> - current measurement  
> - corner positions of the fish farm  
> - coastline data  
> - bathymetry data  
> - production data

data is available upon reasonable request from Fiskaaling, contact birgitta@fiskaaling.fo
