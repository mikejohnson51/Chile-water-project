

library(tidyverse)
library(sf)


library(USAboundaries)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)

library(rworldmap)
library(AOI)

library(osmdata)
library(elevatr)

library(gghighlight)
library(ggrepel)





chile = aoi_get(country = 'Chile')
plot(chile$geometry)

cities = read_csv('data/worldcities.csv') %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  filter(country == 'Chile', population >= 100000) %>% 
  st_transform(5070)

so_amer = countries110 %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 5070) %>%
  filter(admin %in% c('Bolivia', 'Peru','Chile', 'Argentina'))

so_amer_c = st_combine(so_amer) %>% 
  st_cast('MULTILINESTRING')

# Create boundy box for So Amer region
bb = chile %>% 
  st_transform(4326) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()
plot(bb)

rivers = read_sf('data/waterways.shp') %>% 
  select(osm_id, name, type) %>% 
  filter(type == 'river')  %>% 
  na.omit() %>% 
  st_transform(5070) 


ggplot() +
  geom_sf(data = so_amer, fill = 'cornsilk3') +
  geom_sf(data = chile, fill = 'darkorange', alpha = .5) +
  geom_sf(data = cities, col = "red", size = 2) +
  geom_text_repel(data = cities, aes(geometry = geometry, label = city), stat = 'sf_coordinates') +
  theme_gray() +
  theme( panel.background = element_rect(fill = '#BFD5E3'),
         panel.grid.major = element_line(size =0)) +
  labs(title = 'Chile',
       subtitle = 'South America') + 
  ylim(c(-55,-15))


country_touch = st_filter(so_amer, chile, .predicate = st_intersects)


