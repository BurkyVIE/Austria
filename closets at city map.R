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

# Oesterreichsiche Staedte mit ueber 50.000 Einwohnern
at_staedte <- tribble(~Name, ~Lat, ~Long,
                      "Wien", 48.208333, 16.366667,
                      "Graz", 47.066667, 15.433333,
                      "Linz", 48.3, 14.283333,
                      "Salzburg", 47.8, 13.033333,
                      "Innsbruck", 47.268333, 11.393333,
                      "Klagenfurt", 46.616667, 14.3,
                      "Villach", 46.616667, 13.85,
                      "Wels", 48.15, 14.016667,
                      "St. Pölten", 48.2, 15.616667) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(., 3416) %>%
  st_cast(., "POINT") %>%
  mutate(Name = factor(Name, ordered = TRUE))

# Ergaenze naechtse Stadt im Netz
netz <- st_sf(geometry = netz, Naechste = at_staedte$Name[st_nearest_feature(netz, at_staedte)])

# df <- st_coordinates(netz) %>% as_tibble() %>% bind_cols(Naechste = netz %>% pull(Naechste)) # so wäre geom_tile möglich, Achtung fill statt color

ggplot() +
  geom_sf(data = netz, mapping = aes(color = Naechste)) +
  geom_sf(data = austria, fill = NA, color = "grey25", size = 2) +
  geom_sf(data = at_staedte, color = "grey85", size = 3) +
  geom_sf(data = at_staedte, color = "grey25", size = 2) +
  scale_color_manual(name = NULL, values = colorspace::qualitative_hcl(9)) +
  # scale_color_brewer(name = NULL, palette = "Spectral") +
  # scale_color_viridis_d(name = NULL) +
  guides(color = guide_legend(override.aes = list(size = 5, shape = 15))) +
  labs(title = "Closest Austrian city map",
       subtitle = "cities with a population > 50,000") +
  theme_void() +
  theme(legend.position = "bottom") -> p

windows(12, 7)
plot(p)
rm(austria, netz, at_staedte, p)