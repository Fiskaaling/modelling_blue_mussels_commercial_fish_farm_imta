# r packages required

# obs! odbc driver til postgres er neyðugur!!

packages <- c("stringi", "Rcpp", #these are installed first, because of a bug in the tidyverse installation on UNIX system
              "units", #installed because of bug in sf installation on UNIX system
              "tidyverse",
              "viridis",
              "DBI",
              #"odbc", #only needed if data is stored in database
              "sf",
              "foreach",
              "doParallel",
              "purrr",
              "usethis"#,
              #"gifski" #problematic on linux
              )

# install missing r packages

missing <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(missing)>0){install.packages(missing)}
