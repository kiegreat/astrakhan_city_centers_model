
library(tidyverse)
library(sf) # to work with geospatial data
library(ggmap) # to create basemaps
library(RColorBrewer) # to work with color aesthetics
library(extrafont) # to use custom fonts

loadfonts(device = "win")

style_string <- '&style=element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road.arterial%7Celement:labels%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels%7Cvisibility:off&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Cvisibility:off&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e'
basemap <- get_googlemap(center = c(lon = 48.033574, lat = 46.347869), zoom = 12, inject = style_string, maptype = 'roadmap')

maps_theme <-   theme(
  axis.ticks = element_blank(), 
  axis.text = element_blank(), 
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

plots_theme <-   theme(
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  title = element_text(family = 'Roboto Medium'), 
  plot.subtitle = element_text(family = 'Roboto Light'), 
  plot.caption = element_text(family = 'Roboto Light'), 
  legend.title = element_text(family = 'Roboto'),
  text = element_text(family = 'Roboto')
)

df_coords <- readRDS('data/df_coords.rds')


map1 <- ggmap(basemap) +
  stat_density_2d(data = df_coords, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon', h = NULL, adjust = c(0.2, 0.2)) +
  geom_point(data = df_coords, aes(x = lon, y = lat), alpha = 0.5, shape = 20, col = 'black') +
  maps_theme +
  theme(legend.position = 'none') +
  scale_fill_gradient(low = 'red', high = 'yellow')

map1

places <- read_sf("data/osm/gis_osm_places_a_free_1.shp")
city <- places %>% filter(name == 'Астрахань') # %>% st_set_crs(4326) %>% st_transform(crs = 32638)

water <- read_sf("data/osm/gis_osm_water_a_free_1.shp") %>% st_intersection(city, water)
city <- st_difference(city %>% st_union(), water %>% st_union()) %>% st_as_sf()

cells <- st_make_grid(city, cellsize = .002, square = FALSE) # first, cellsize was 0.004
class(cells)

ggmap(basemap) +
  geom_sf(data = city, inherit.aes = F)

ggmap(basemap) +
  geom_sf(data = cells, inherit.aes = F)

cells2 <- cells %>% st_as_sf() 
cells2 <- cells2 %>% mutate(id = c(1:nrow(cells2)))

df_coords_sf <- st_as_sf(df_coords, coords = c("lon", "lat")) %>% st_set_crs(value = st_crs(cells2))

df <- st_join(x = cells2, y = df_coords_sf)
df2 <- df %>% na.omit() %>% group_by(id) %>% summarise(n = n())

ggmap(basemap) +
  geom_sf(data = df2, aes(fill = n), inherit.aes = F)


df3 <- st_intersects(df2, df2)

df3[[1]]
df3[[2]]

df_list <- data.frame()

for(i in c(1:length(df3))){
  df_temp <- data.frame(id = i, neigh = df3[[i]])
  df_list <- rbind(df_list, df_temp)
}

df4 <- df2 %>% mutate(list_id = c(1:nrow(df2)))

df_neighbores <- inner_join(df_list, df4, by = c('neigh' = 'list_id'))

dfn2 <- df_neighbores %>% 
  group_by(id.x) %>% 
  summarise(
    sum_n = sum(n),
    k = 7
  ) %>% 
  mutate(
    trend = sum_n / k
  )

ggplot(dfn2, aes(x = trend)) +
  geom_histogram(bins = 50)

dfn3 <- df_neighbores %>% 
  filter(id.x == neigh) %>% 
  left_join(dfn2, by = 'id.x') %>% 
  mutate(nrm = n - trend)

ggplot(dfn3, aes(x = nrm)) +
  geom_histogram(bins = 50)

ggplot(dfn3, aes(x = nrm)) +
  geom_boxplot()

std2 <- sd(dfn3$nrm) * 2
std3 <- sd(dfn3$nrm) * 3

ggplot(dfn3, aes(x = nrm)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = c(median(dfn3$nrm), median(dfn3$nrm) + std2, median(dfn3$nrm) + std3), col = 'red', linetype = 'dashed')

thresh_center <- median(dfn3$nrm) + sd(dfn3$nrm) * 3
thresh_subcenter <- median(dfn3$nrm) + sd(dfn3$nrm) * 2

dfn4 <- dfn3 %>% 
  mutate(
    is_center = ifelse(nrm >= thresh_center, 1, 0),
    is_subcenter = ifelse(nrm >= thresh_subcenter, 1, 0)
  ) %>% 
  st_as_sf()

ggmap(basemap) +
  geom_sf(data = dfn4, aes(fill = n), inherit.aes = F, alpha = 0.3) +
  geom_sf(data = dfn4 %>% filter(is_subcenter == 1), fill = 'blue', inherit.aes = F, alpha = 0.6) +
  geom_sf(data = dfn4 %>% filter(is_center == 1), fill = 'red', inherit.aes = F)

sum(dfn4$is_center)

# Всего 3 ядра (при cellsize = 0.004)
# 6 ядер первого и 3 ядра второго порядка (при cellsize = 0.002)

# Нужно отрисовывать вручную кварталы на АркГИСе? :(
# Расширить перечень объектов?











