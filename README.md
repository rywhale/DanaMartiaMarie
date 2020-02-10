
## To install dependencies:
```
install.packages(
  c(
    "shiny",
    "shinythemes",
    "dplyr",
    "lubridate",
    "leaflet",
    "leaflet.esri",
    "sf",
    "ggplot2",
    "tidyr",
    "stringr",
    "glue",
    "tidyhydat",
    "purrr",
    "snakecase"
  )
)

# Real time data from WSC DataMart
remotes::install_github("rywhale", "rtdd")

# Most recent HYDAT
tidyhydat::download_hydat()

```

## To run:
```
shiny::runGitHub("DanaMartiaMarie", "rywhale")
```