library(arrow)
library(auk)
library(dplyr)
library(ebirdst)
library(fields)
library(forcats)
library(ggplot2)
library(mccf1)
library(ranger)
library(scam)
library(sf)
library(terra)

set.seed(1)

# Data preparation ---

# load observations

# load checklists, zero-fill

# filter checklists

# load study region and spatially filter
study_region <- read_sf("data/ibra61_reg.gpkg") %>%
  filter((REG_CODE %in% c("SEQ", "BBS")) & STATE == "QLD") %>%
  st_union() %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

# Test-train split ---

# split and inspect

# Case-controlled grid sampling ---

# grid sample

# look at data volume and prevalence

# Hurdle model ---

# select training data

# Step 1: Encounter rate ---

# calculate detection frequency

# fit encounter rate model

# predict encounter rate and fit calibration model

# calculate threshold

# Step 2: Count ---

# subset training data for count

# predict encounter rate to count model training data

# fit count model

# Assessment ---

# get test data and make predictions

# calculate encounter rate PPMs

# calculate count PPMs

# Predictor Importance ---

# extract variable importance

# plot
ggplot(head(pi, 10)) +
  aes(x = fct_reorder(predictor, importance), y = importance) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 2, colour = "#555555") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL,
       y = "Predictor Importance (Gini Index)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#cccccc", linewidth = 0.5))

# Partial dependence ---

# function to calculate partial dependence for a single predictor
calculate_pd <- function(predictor, model, data,
                         x_res = 25, n = 1000) {
  # create prediction grid using quantiles
  x_grid <- quantile(data[[predictor]],
                     probs = seq(from = 0, to = 1, length = x_res),
                     na.rm = TRUE)
  # remove duplicates
  x_grid <- x_grid[!duplicated(signif(x_grid, 8))]
  x_grid <- unname(unique(x_grid))
  grid <- data.frame(predictor = predictor, x = x_grid)
  names(grid) <- c("predictor", predictor)

  # subsample training data
  n <- min(n, nrow(data))
  data <- data[sample(seq.int(nrow(data)), size = n, replace = FALSE), ]

  # drop focal predictor from data
  data <- data[names(data) != predictor]
  grid <- merge(grid, data, all = TRUE)

  # predict
  p <- predict(model, data = grid)

  # summarize
  pd <- grid[, c("predictor", predictor)]
  names(pd) <- c("predictor", "x")
  pd$encounter_rate <- p$predictions[, 2]
  pd <- dplyr::group_by(pd, predictor, x) %>%
    dplyr::summarise(encounter_rate = mean(encounter_rate, na.rm = TRUE),
                     .groups = "drop")

  return(pd)
}

# calculate for top variables

# plot top 10 predictors for encounter rate model
ggplot(head(pi_er, 10)) +
  aes(x = fct_reorder(predictor, importance), y = importance) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 2, colour = "#555555") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL,
       y = "Predictor Importance (Gini Index)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#cccccc", linewidth = 0.5))

# plot top 10 predictors for count model
ggplot(head(pi_count, 10)) +
  aes(x = fct_reorder(predictor, importance), y = importance) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 2, colour = "#555555") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL,
       y = "Predictor Importance (Gini Index)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#cccccc", linewidth = 0.5))

# Prediction ---

# read in prediction grid

# filter

# rasterize

# subset to study region

# calculate partials for maximizing prediction values

# solar noon diff

# plot
ggplot(pd_time) +
  aes(x = solar_noon_diff, y = encounter_rate) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(-12, 13, by = 3)) +
  labs(x = "Difference from solar noon",
       y = "Encounter",
       title = "Partial dependence") +
  theme_bw()

# effort_hrs

# plot
ggplot(pd_hrs) +
  aes(x = effort_hrs, y = encounter_rate) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  labs(x = "Effort Hours",
       y = "Encounter",
       title = "Partial dependence") +
  theme_bw()

# effort distance

# plot
ggplot(pd_km) +
  aes(x = effort_distance_km, y = encounter_rate) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x = "Effort Distance (km)",
       y = "Encounter",
       title = "Partial dependence") +
  theme_bw()

# rate

# plot
ggplot(pd_km) +
  aes(x = effort_speed_kmph, y = encounter_rate) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 5.5 / 3.5, col = "red") +
  #scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  xlim(c(0, 5)) +
  labs(x = "Effort Speed (km/h))",
       y = "Encounter",
       title = "Partial dependence") +
  theme_bw()

# assign prediction effort values

# predict presence/absence, encounter rate, calibrate, predict count

# convert to raster

# load and project gis data
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
study_region_proj <- st_transform(study_region, crs = map_proj) %>%
  st_geometry()

# project the raster data
r_pred_proj <- crop(r_pred, st_transform(study_region, crs(r_pred))) %>%
  project(map_proj, method = "near")

# map!

# in range abundance
r_plot <- r_pred_proj[["abundance"]] * r_pred_proj[["in_range"]]

par(mar = c(0.25, 0.25, 0.25, 0.25))
# set up plot area
plot(study_region_proj, col = NA, border = NA)
plot(ne_land, col = "#cfcfcf", border = "#888888", lwd = 0.5, add = TRUE)

# define quantile breaks, excluding zeros
brks <- ifel(r_plot > 0, r_plot, NA) %>%
  global(fun = quantile,
         probs = seq(0, 1, 0.1), na.rm = TRUE) %>%
  as.numeric() %>%
  unique()
# label the bottom, middle, and top value
lbls <- round(c(min(brks), median(brks), max(brks)), 2)
# ebird status and trends color palette
pal <- ebirdst_palettes(n = length(brks) - 1, type = "weekly")
plot(r_plot,
     col = c("#e6e6e6", pal), breaks = c(0, brks),
     maxpixels = ncell(r_plot),
     legend = FALSE, axes = FALSE, bty = "n",
     add = TRUE)

# borders
plot(ne_state_lines, col = "#ffffff", lwd = 0.75, add = TRUE)
plot(ne_country_lines, col = "#ffffff", lwd = 1.5, add = TRUE)
plot(study_region_proj, border = "#000000", col = NA, lwd = 1, add = TRUE)
box()

# legend
image.plot(zlim = c(0, 1), legend.only = TRUE,
           col = pal, breaks = seq(0, 1, length.out = length(brks)),
           smallplot = c(0.88, 0.90, 0.2, 0.8),
           horizontal = FALSE,
           axis.args = list(at = c(0, 0.5, 1), labels = lbls,
                            fg = "black", col.axis = "black",
                            cex.axis = 0.75, lwd.ticks = 0.5),
           legend.args = list(text = "Relative Abundance",
                              side = 2, col = "black",
                              cex = 1, line = 0))
