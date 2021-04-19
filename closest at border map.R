library(tidyverse)
library(rnaturalearth)
library(sf)
library(raster)

# Importiere Grenze Austria
austria <- ne_countries(scale = 10, country = "Austria", returnclass = "sf") %>%
  st_transform(., 3416)

# Erzeuge Netz
netz <- st_make_grid(austria, cellsize = 1000, what = "centers") # cellsize 1250

# Netz nur innerhalb Grenzen
netz <- st_intersection(netz, austria)

# transform Austria from polygon shape to line
austria <- st_cast(austria, "MULTILINESTRING")

# Berechne naechste LHStd fuer Netz
dist <- st_distance(austria, netz) %>% as.vector() %>% `/`(1000)

# Ergaenze Distanzen im Netz
netz <- st_sf(geometry = netz, Distanz = dist)

# Maximale Distanz zur Grenze
maxi <- netz %>% filter(Distanz == max(Distanz))

ggplot() +
  geom_sf(data = netz, mapping = aes(color = Distanz)) +
  geom_sf(data = austria, fill = NA, color = "grey25", size = 2) +
  # geom_sf(data = maxi, color = "grey25", size = 2) +
  scale_color_distiller(name = "Distance [km]", palette = "YlGnBu", direction = -1) +
  # scale_color_fermenter(name = "Distance [km]", palette = "YlGnBu", direction = -1, breaks = (0:8)*15) +
  labs(title = "Closest Austrian border map") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm")) -> p

windows(12, 7)
plot(p)
rm(austria, netz, dist, maxi, p)
