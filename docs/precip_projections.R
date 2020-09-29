
library(tidyverse)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)

library(sp)
library(AOI)
library(rgdal)
library(raster)
library(climateR)
library(cft)

param_meta$terraclim

chile = aoi_get(country = 'Chile') %>% 
  st_transform(5070) 

bb = chile %>% 
  st_transform(5070) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

params = c('prcp')

st = getTerraClim(bb, params, startDate = '2017-08-01') %>%
  stack() %>%
  setNames(params)


so_amer = countries110 %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 5070) %>%
  filter(admin %in% c('Bolivia', 'Peru','Chile', 'Argentina'))

so_amer_c = st_combine(so_amer) %>% 
  st_cast('MULTILINESTRING')

chile_ggplot = ggplot() +
  geom_sf(data = so_amer, fill = 'cornsilk3') +
  geom_sf(data = chile) +
  theme_gray() +
  labs(title = 'Chile',
       subtitle = 'South America')
plot(st)
plot(st, add = TRUE)
plot(chile_ggplot, add= TRUE)
plot(st, add = TRUE)
