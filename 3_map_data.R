
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

# 1. Kernel density ----

map1 <- ggmap(basemap) +
  stat_density_2d(data = df_coords, aes(x = lon, y = lat, fill = ..level..), geom = 'polygon', h = NULL, adjust = c(0.2, 0.2)) +
  geom_point(data = df_coords, aes(x = lon, y = lat), alpha = 0.1, col = 'black') +
  maps_theme +
  theme(legend.position = 'none') +
  scale_fill_gradient(low = 'red', high = 'yellow') +
  labs(
    x = '',
    y = '',
    title = 'Пространственная структура Астрахани',
    subtitle = 'на основе тепловой карты концентрации объектов обслуживания',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / Данные - 2ГИС'
  ) +
  coord_sf(crs = st_crs(4326)) +
  scale_y_continuous(limits = c(46.28, 46.43), expand = c(0, 0))

map1

ggsave(filename = 'map1.png', plot = map1, device = 'png', path = 'viz', dpi = 400, width = 12, height = 7)

rm(map1); gc()



# 2. Hexagonal grid ----

# Remove water part from city shape
city <- read_sf("data/osm/gis_osm_places_a_free_1.shp") %>% filter(name == 'Астрахань')
water <- read_sf("data/osm/gis_osm_water_a_free_1.shp") %>% st_intersection(city, water)
city <- st_difference(city %>% st_union(), water %>% st_union()) %>% st_as_sf()
rm(water)

# Create hexagonal grid
set.seed(42)
cells <- st_make_grid(city, cellsize = .004, square = FALSE) %>% st_as_sf()
cells <- cells %>% mutate(id = c(1:nrow(cells)))

df_coords <- st_as_sf(df_coords, coords = c("lon", "lat")) %>% st_set_crs(value = st_crs(cells))
df_intersection <- st_join(x = cells, y = df_coords) %>% na.omit() %>% group_by(id) %>% summarise(n = n())
df_setdiff <- cells %>% filter(!(id %in% df_intersection$id)) %>% mutate(n = 0) %>% select(id, n, x) # define cells without service objects
df_union <- rbind(df_intersection, df_setdiff)

# For each row get its neighbors
df_graph <- st_intersects(df_union, df_union)

df_list <- data.frame()

for(i in c(1:length(df_graph))) {
  df_temp <- data.frame(second_id = i, neighbour = df_graph[[i]])
  df_list <- rbind(df_list, df_temp)
}

rm(df_temp, i)

# How many neighbours each cell has
df_k <- df_list %>% group_by(second_id) %>% filter(second_id != neighbour) %>% summarise(k = n_distinct(neighbour))

df_union <- df_union %>% mutate(second_id = c(1:nrow(df_union)))
df_neighbores <- inner_join(df_list, df_union %>% select(second_id, n), by = c('neighbour' = 'second_id'))

df_trend <- df_neighbores %>% 
  inner_join(df_k %>% select(second_id, k), by = 'second_id') %>% 
  group_by(second_id) %>% 
  summarise(
    k = mean(k),
    sum_n = sum(n)
  ) %>% 
  mutate(
    trend = sum_n / k
  )

df_nrm <- df_neighbores %>% 
  filter(second_id == neighbour) %>% 
  left_join(df_trend, by = 'second_id') %>% 
  mutate(nrm = n - trend)

rm(df_intersection, df_graph, df_neighbores, df_list, df_trend)

border_1 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 0.67
border_2 <- median(df_nrm$nrm) + sd(df_nrm$nrm)
border_3 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 2
border_4 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 3

ggplot(df_nrm, aes(x = nrm)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = c(border_1, border_2, border_3, border_4), col = 'red', linetype = 'dashed')

df_map2 <- df_nrm %>% 
  mutate(
    cell_type = case_when(
      nrm < border_3 ~ 0,
      # nrm >= border_1 & nrm < border_2 ~ 1,
      # nrm >= border_2 & nrm < border_3 ~ 2,
      nrm >= border_3 & nrm < border_4 ~ 3,
      TRUE ~ 4
    )
  ) %>% 
  st_as_sf()

map2 <- ggmap(basemap) +
  geom_sf(data = df_map2 %>% filter(cell_type %in% c(3,4)), fill = 'red', alpha = 0.5, inherit.aes = F) +
  geom_sf(data = df_coords, inherit.aes = F, alpha = 0.05, size = 1) +
  scale_fill_brewer(palette = 'Reds') +
  maps_theme +
  theme(legend.position = 'none') +
  labs(
    x = '',
    y = '',
    title = 'Пространственная структура Астрахани',
    subtitle = 'на основе неравномерно-районированной модели (гексагональная сетка)',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / Данные - 2ГИС'
  ) +
  coord_sf(crs = st_crs(4326)) +
  scale_y_continuous(limits = c(46.28, 46.43), expand = c(0, 0))

map2

ggsave(filename = 'map2.png', plot = map2, device = 'png', path = 'viz', dpi = 400, width = 12, height = 7)

rm(map2, df_nrm, city, cells); gc()
rm(border_1, border_2, border_3, border_4)



# 3. Expert grid ----

city_grid <- read_sf('data/astrakhan_grid.shp')
city_grid <- city_grid %>% select(-Id) %>% mutate(id = c(1:nrow(city_grid)))

df_coords <- readRDS('data/df_coords.rds')
df_coords <- st_as_sf(df_coords, coords = c("lon", "lat")) %>% st_set_crs(value = st_crs(city_grid))

df_intersection <- st_join(x = city_grid, y = df_coords) %>% na.omit() %>% group_by(id) %>% summarise(n = n())
df_setdiff <- city_grid %>% filter(!(id %in% df_intersection$id)) %>% mutate(n = 0) %>% select(id, n, geometry) # define cells without service objects
df_union <- rbind(df_intersection, df_setdiff)

# For each row get its neighbors
df_graph <- st_intersects(df_union, df_union)

df_list <- data.frame()

for(i in c(1:length(df_graph))) {
  df_temp <- data.frame(second_id = i, neighbour = df_graph[[i]])
  df_list <- rbind(df_list, df_temp)
}

rm(df_temp, i)

# How many neighbours each cell has
df_k <- df_list %>% group_by(second_id) %>% filter(second_id != neighbour) %>% summarise(k = n_distinct(neighbour))

df_union <- df_union %>% mutate(second_id = c(1:nrow(df_union)))
df_neighbores <- inner_join(df_list, df_union %>% select(second_id, n), by = c('neighbour' = 'second_id'))

df_trend <- df_neighbores %>% 
  inner_join(df_k %>% select(second_id, k), by = 'second_id') %>% 
  group_by(second_id) %>% 
  summarise(
    k = mean(k),
    sum_n = sum(n)
  ) %>% 
  mutate(
    trend = sum_n / k
  )

df_nrm <- df_neighbores %>% 
  filter(second_id == neighbour) %>% 
  inner_join(df_trend, by = 'second_id') %>% 
  mutate(nrm = n - trend)

rm(df_intersection, df_graph, df_neighbores, df_list, df_trend, df_union, df_setdiff)

border_1 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 0.67
border_2 <- median(df_nrm$nrm) + sd(df_nrm$nrm)
border_3 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 2
border_4 <- median(df_nrm$nrm) + sd(df_nrm$nrm) * 3

ggplot(df_nrm, aes(x = nrm)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = c(border_1, border_2, border_3, border_4), col = 'red', linetype = 'dashed')

df_map3 <- df_nrm %>% 
  mutate(
    cell_type = case_when(
      nrm < border_3 ~ 0,
      nrm >= border_1 & nrm < border_2 ~ 4,
      nrm >= border_2 & nrm < border_3 ~ 3,
      nrm >= border_3 & nrm < border_4 ~ 2,
      TRUE ~ 1
    )
  ) %>% 
  st_as_sf()

map3 <- ggmap(basemap) +
  geom_sf(data = df_map3 %>% filter(cell_type %in% c(0,3,4)), fill = 'white', inherit.aes = F, alpha = 0.1, lwd = 0.25) +
  geom_sf(data = df_map3 %>% filter(cell_type %in% c(1,2)), aes(fill = factor(cell_type)), inherit.aes = F) +
  geom_sf(data = df_coords, inherit.aes = F, alpha = 0.05, size = 1) +
  scale_fill_manual(values = c('#fc9272', '#fddfd1'), labels = c("I порядка", "II порядка")) +
  maps_theme +
  theme(legend.position = 'bottom') +
  labs(
    x = '',
    y = '',
    title = 'Пространственная структура Астрахани',
    subtitle = 'на основе неравномерно-районированной модели (квартальная сетка)',
    caption = 'Автор - Кирилл Гудков / Инструмент - R / Данные - 2ГИС',
    fill = 'Ядра'
  ) +
  coord_sf(crs = st_crs(4326)) +
  scale_x_continuous(limits = c(47.985, 48.11), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.31, 46.39), expand = c(0, 0))

map3

ggsave(filename = 'map3.png', plot = map3, device = 'png', path = 'viz', dpi = 400, width = 12, height = 7)

rm(map3, df_nrm, city_grid, df_k); gc()
rm(border_1, border_2, border_3, border_4)



# 4. Combine ----

df_xy <- readRDS('data/df_coords.rds')

map4 <- ggmap(basemap) +
  stat_density_2d(data = df_xy, aes(x = lon, y = lat, fill = ..level..), geom = 'polygon', h = NULL, adjust = c(0.3, 0.3), alpha = 0.5) +
  geom_sf(data = df_map2 %>% filter(cell_type %in% c(3,4)), alpha = 0.3, inherit.aes = F, lwd = 0.4, fill = NA, color = '#FF006E') +
  geom_sf(data = df_map3 %>% filter(cell_type %in% c(1,2)), alpha = 0.3, inherit.aes = F, lwd = 0.4, fill = NA, color = '#3A86FF') +
  scale_fill_gradient(low = 'red', high = 'yellow') +
  maps_theme +
    labs(
      x = '',
      y = '',
      title = 'Пространственная структура Астрахани',
      subtitle = 'сравнение: тепловая карта vs гексагональная сетка vs квартальная сетка',
      caption = 'Автор - Кирилл Гудков / Инструмент - R / Данные - 2ГИС'
    ) +
  coord_sf(crs = st_crs(4326)) +
  scale_y_continuous(limits = c(46.28, 46.43), expand = c(0, 0)) +
  scale_x_continuous(limits = c(47.985, 48.11), expand = c(0, 0)) +
  scale_y_continuous(limits = c(46.31, 46.39), expand = c(0, 0)) +
  theme(legend.position = 'none')

map4

ggsave(filename = 'map4.png', plot = map4, device = 'png', path = 'viz', dpi = 400, width = 12, height = 7)



# 5. Save data for cartodb ----

df_hex <- df_map2 %>% filter(cell_type %in% c(3,4)) %>% select(nrm) %>% rename(geomerty = x)
df_borough <- df_map3 %>% filter(cell_type %in% c(1,2)) %>% select(nrm)

st_write(obj = df_hex, dsn = 'data/shapefiles/city_centers_hexagons.shp', driver = 'ESRI Shapefile')
st_write(obj = df_borough, dsn = 'data/shapefiles/city_centers_boroughs.shp', driver = 'ESRI Shapefile')
write_csv(df_xy, 'data/df_city_centers_objects.csv')
