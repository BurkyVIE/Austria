library(tidyverse)
library(rnaturalearth)
library(sf)
library(raster)

# Importiere Grenze Austria
austria <- ne_countries(scale = 10, country = "Austria", returnclass = "sf") %>%
  st_transform(., 3416)

# Erzeuge Netz
netz <- st_make_grid(austria, cellsize = 1250, what = "centers")

# Netz nur innerhalb Grenzen
netz <- st_intersection(netz, austria)

# Oesterreichsiche Landeshauptstaedte
lhstd <- tribble(~Name, ~Lat, ~Long,
                      "Bregenz", 47.505, 9.749167,
                      "Eisenstadt", 47.845556, 16.518889,
                      "Graz", 47.066667, 15.433333,
                      "Innsbruck", 47.268333, 11.393333,
                      "Klagenfurt", 46.616667, 14.3,
                      "Linz", 48.3, 14.283333,
                      "Salzburg", 47.8, 13.033333,
                      "St. Pölten", 48.2, 15.616667,
                      "Wien", 48.208333, 16.366667) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(., 3416) %>%
  st_cast(., "POINT")

# Berechne naechste LHStd fuer Netz
dist <- st_distance(lhstd, netz) %>% apply(., 2, min) %>% `/`(1000)

# Ergaenze Distanzen im Netz
netz <- st_sf(geometry = netz, Distanz = dist)

ggplot() +
  geom_sf(data = netz, mapping = aes(color = Distanz)) +
  geom_sf(data = austria, fill = NA, color = "grey25", size = 2) +
  geom_sf(data = lhstd, color = "grey25", size = 2) +
  geom_sf_label(data = lhstd, mapping = aes(label = Name), nudge_y = c(-1, 1, -1, -1, 1, -1, -1, 1, 1) * 10000, alpha = .75, size = 3.25) +
  scale_color_distiller(name = "Distance [km]", palette = "YlOrRd", direction = 1) +
  labs(title = "Closest Austrian state capital map") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm")) -> p

windows(12, 7)
plot(p)
rm(austria, netz, lhstd, dist, p)
