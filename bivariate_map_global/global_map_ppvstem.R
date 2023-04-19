# Load the necessary libraries
library(raster)
library(sf)
library(tidyverse)
library(paletteer)
library(extrafont)
library(ragg)
library(ggtext)
library(gtools)
library(cowplot)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


# Read in the exported data
precipitation_raster <- raster("Total_Precipitation_1980_2020era5land.tif")
temperature_raster <- raster("Mean_Temperature_1980_2020era5land.tif")

# Convert raster data to data frames
precipitation_df <- as.data.frame(as(precipitation_raster, "SpatialPixelsDataFrame"))
temperature_df <- as.data.frame(as(temperature_raster, "SpatialPixelsDataFrame"))
rm(precipitation_raster,temperature_raster)
# Merge the data frames
bivariate_df <- merge(precipitation_df, temperature_df) %>% 
  mutate(precipitation_tertile = quantcut(total_precipitation_sum, q = 3, labels = FALSE),
         temperature_tertile = quantcut(temperature_2m, q = 3, labels = FALSE),
         key = case_when(
           precipitation_tertile == 1 & temperature_tertile == 1 ~ 1,
           precipitation_tertile == 2 & temperature_tertile == 1 ~ 2,
           precipitation_tertile == 3 & temperature_tertile == 1 ~ 3,
           precipitation_tertile == 1 & temperature_tertile == 2 ~ 4,
           precipitation_tertile == 2 & temperature_tertile == 2 ~ 5,
           precipitation_tertile == 3 & temperature_tertile == 2 ~ 6,
           precipitation_tertile == 1 & temperature_tertile == 3 ~ 7,
           precipitation_tertile == 2 & temperature_tertile == 3 ~ 8,
           precipitation_tertile == 3 & temperature_tertile == 3 ~ 9),
         colour = case_when(
           key == 1 ~ "#f3f3f3", key == 2 ~ "#b4d3e1", key == 3 ~ "#509dc2",
           key == 4 ~ "#f3e6b3", key == 5 ~ "#b3b3b3", key == 6 ~ "#376387",
           key == 7 ~ "#f3b300", key == 8 ~ "#b36600", key == 9 ~ "#000000"))

# Generate data frame for the key
keydata <- bivariate_df %>%
  filter(!is.na(colour)) %>%
  group_by(precipitation_tertile, temperature_tertile) %>%
  summarise(RGB = unique(colour))

# Create a plot for the key
key <- ggplot(keydata) +
  geom_tile(aes(x = precipitation_tertile, y = temperature_tertile, fill = RGB)) +
  scale_fill_identity() +
  labs(x = expression("More precipitation" %->% ""),
       y = expression("Higher temperature" %->% "")) +
  theme_custom() +
  theme(
    axis.title = element_text(size = 12),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  coord_fixed()

# Create a bivariate map
bivariate_map <- ggplot(bivariate_df, aes(x = x, y = y, fill = colour, colour = colour)) +
  geom_tile() +
  scale_fill_identity() +
  scale_colour_identity() +
  theme_custom() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = rel(3))) +
  labs(title = "Precipitation and Temperature (1980-2020)",
       subtitle = "Bivariate map of total precipitation and mean temperature",
       caption = "Data from ERA5-Land \nPlot by @fcojara506")

# Save the bivariate map and key as a single image
agg_tiff("BivariateMap_Precipitation_Temperature.tif", units = "in", width = 20, height = 10, res = 400)
agg_tiff("BivariateMap_Precipitation_Temperature.png", units = "in", width = 20, height = 10, res = 50)
ggdraw() +
  draw_plot(bivariate_map, 0, 0, 1, 1) +
  draw_plot(key, 0.05, 0.05, 0.2, 0.2)
dev.off()
dev.off()
