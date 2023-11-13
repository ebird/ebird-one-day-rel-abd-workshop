library(auk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

### EBD data ---

# load checklist data and look at it

# exercise

# load observation data and look at it

# Shared checklists ---

# look at grouped data

# create unique checklists and inspect

# Taxonomic rollup ---

# use auk to roll up

# compare before and after for same checklist

# Zero-filling ---

# use auk to load checklist and observations and zero-fill

# convert Xs to NAs

# Filter data ---

# filter data

### ERD data ---

library(arrow)
library(auk)
library(dplyr)
library(ebirdst)
library(ggplot2)
library(sf)
library(terra)

# read in checklist data and inspect

# look at predictors

# read in observations and inspect

# Zero-filling ---

# create a single species set of detections

# join checklists and detections

# zero-fill

# map
map_proj <- "+proj=laea +lon_0=146.95 +lat_0=-19.15 +datum=WGS84 +units=m +no_defs"
ne_land <- read_sf("data/gis-data.gpkg", "ne_land") %>%
  st_transform(crs = map_proj) %>%
  st_geometry()
ne_country_lines <- read_sf("data/gis-data.gpkg", "ne_country_lines") %>%
  st_transform(crs = map_proj) %>%
  st_geometry()
ne_state_lines <- read_sf("data/gis-data.gpkg", "ne_state_lines") %>%
  st_transform(crs = map_proj) %>%
  st_geometry()
target_state <- read_sf("data/gis-data.gpkg", "regions") %>%
  filter(state_code == "AU-QLD") %>%
  st_transform(crs = map_proj) %>%
  st_geometry()

# prepare ebird data for mapping
bird_sf <- bird_zf %>%
  # convert to spatial points
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = map_proj)

# map
par(mar = c(0.25, 0.25, 0.25, 0.25))
# set up plot area
plot(st_geometry(target_state), col = NA, border = NA)
# contextual gis data
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)
plot(target_state, col = "#e6e6e6", border = NA, add = TRUE)
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
# ebird observations
# all
plot(bird_sf,
     pch = 19, cex = 0.1, col = scales::alpha("#555555", 4),
     add = TRUE)
# detection
plot(filter(bird_sf, obs_detected == 1),
     pch = 19, cex = 0.3, col = scales::alpha("#4daf4a", 1),
     add = TRUE)
# legend
legend("bottomright", bty = "n",
       col = c("#555555", "#4daf4a"),
       legend = c("eBird checklists", "Bird sightings"),
       pch = 19)
box()
par(new = TRUE, mar = c(0, 0, 3, 0))
title("Shining Bronze-Cuckoo eBird Observations\nNovember-January 2007-2022")

# Prediction Grid ---

# read in prediction grid

# load raster template

# rasterize forest cover

# plot
plot(trim(forest_cover), axes = FALSE)

# Grid sampling ---

# look at number of checklists by day

# look at mean detection rate

# grid sample and look at amount of data and prevalence
