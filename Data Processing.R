library(tidyverse)
library(sf)
library(tigris)
library(tmap)
tmap_mode("view")

## Function to calculate distinct LSAD codes per state & count
getList <- function(x) {
  x <- x %>%
    as.data.frame() %>%
    select(STATEFP, LSAD) %>%
    group_by(STATEFP, LSAD) %>%
    add_count() %>%
    distinct() %>%
    arrange(desc(LSAD)) %>%
    inner_join(lsad, by = "LSAD")
}

## LSAD lookup table
lsad <- read.csv("Data/lsad-lookup.csv")
lsad <- lsad %>%
  select(1,3) %>%
  setNames(c("LSAD", "LSAD-Description"))

## List of state IDs
stateLS <- c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23,
             24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 
             45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56)

# 1. Places ---------------------------------------------------------------

## Get all US places into one file
us_places <- do.call(rbind, lapply(stateLS, places))
us_places_list <- getList(us_places)

## Drop CDPs
us_places_clean <- us_places %>%
  filter(LSAD != 57)
st_write(us_places_clean, "Data/us_incorporated_places.gpkg")

