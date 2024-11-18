# Load necessary libraries
library(tidyverse)
library(sf)
library(geodata)
library(tmap)

# Get spatial data for Rwanda's administrative boundaries using geodata package
sf_Rwanda <- geodata::gadm(country = "RW", level = 1, path = tempdir()) %>%
  st_as_sf()

# Load the confirmed cases data
confirmed.cases <- read.csv("D:\\datasets\\COUNTY_RW_DATA.csv", header = TRUE)

# Convert to data frame (select first 47 rows)
# count_df <- data.frame(confirmed.cases[1:47,])
count_df <- data.frame(confirmed.cases)

# Join the spatial data with confirmed cases data
sf_Rwanda_joined <- sf_Rwanda %>% left_join(count_df, by = "NAME_1")

view(sf_Rwanda_joined)
# # Create the heatmap using tmap
# tm_shape(sf_Rwanda_joined) +
#   tm_polygons(
#     "Count",
#     palette = "Blues",
#     title = "Confirmed Cases",
#     legend.show = TRUE
#   ) +
#   tm_layout(
#     title = "COVID-19 Confirmed Cases by Region in Rwanda",
#     legend.position = c("right", "bottom")
#   )


ggplot() +
  geom_sf(data = sf_Rwanda_joined, aes(fill = Count)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(fill = "Confirmed Cases", title = "COVID-19 Confirmed Cases by Provinces in Rwanda")
