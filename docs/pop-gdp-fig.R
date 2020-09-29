
library(tidyverse)
library(readr)
library(stringr)
library(ggrepel)
library(ggthemes)
library(scales)


# Population projections graph

total_pop = read_csv('data/total-pop.csv') %>% 
  select(country = Location, pop = PopTotal, pop_density = PopDensity, year = Time, Variant) %>% 
  filter(country == 'Chile', year <= 2050) %>% 
  filter(!Variant %in% c('Constant fertility', 'Instant replacement', 'Zero migration', 
                         'Constant mortality', 'No change', 'Momentum', 'Median PI', 'Upper 80 PI', 'Lower 80 PI', 'Upper 95 PI', 'Lower 95 PI'))
past_pop = total_pop %>% 
  filter(Variant == 'Medium', year <= 2050) %>% 
  mutate(pop = pop *1000)

med = total_pop %>% 
  filter(Variant == 'Medium') %>% 
  mutate(pop = pop *1000)


low = total_pop %>% 
  filter(Variant == 'Low')%>% 
  mutate(pop = pop *1000)


high = total_pop %>% 
  filter(Variant == 'High')%>% 
  mutate(pop = pop *1000)


current_pop = past_pop %>% filter(year == 2020)

medium_pop = med %>% filter(year == 2050)
# plot population projections

pop_projection = past_pop %>% ggplot(aes(x = year, y = pop)) +
  geom_line(aes(y = pop), col = 'darkcyan', size = 1.25) +
  geom_line(data = low, aes(y = pop), col = 'darkcyan', size = 1.25) +
  geom_line(data = high, aes(y = pop), col = 'darkcyan', size = 1.25) +
  geom_point(data = current_pop, aes(x = year, y = pop), color = 'red', size = 3) +
  geom_label_repel(data = current_pop, aes(label = pop), hjust = 1.3, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   label.size = 1,
                   segment.color = 'grey50', size = 5) +
  geom_point(data = medium_pop, aes(x = year, y = pop), color = 'red', size = 3) +
  geom_label_repel(data = medium_pop, aes(label = pop), hjust = 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   label.size = 1,
                   segment.color = 'grey50', size = 5) +
  scale_y_continuous(name = 'Population', labels = comma) +
  labs(title = "Projected population growth (1950 - 2050)",
       x = "Year",
       y = "Population",
       subtitle = 'Data Source: United Nations',
       color = "") +
  theme(plot.title=element_text(size=20,face="bold", vjust=1.5, lineheight=1.6),
        axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"), axis.title.x = element_text(color="black", vjust= 1), axis.title.y = element_text(color="black", vjust= 1),
        legend.text = element_text(size = 12, face = 'b')) +
  theme(aspect.ratio = 0.5)

  
library(scales)
pop_col = pop_long %>% select(pop)



# GDP per capita Chile, Brazil, Arg, Peru
gdp_capita = read_csv('data/gdp-capita.csv') %>% 
  filter(country %in% c('Chile', 'Brazil', 'Argentina', 'Peru'))

chile = gdp_capita %>% 
  filter(country == 'Chile') %>% 
  select(!country) %>% 
  pivot_longer(cols = everything(), names_to = 'year', values_to = 'gdp_chile') %>% 
  mutate(country = 'Chile')

bra = gdp_capita %>% 
  filter(country == 'Brazil') %>% 
  select(!country) %>% 
  pivot_longer(cols = everything(), names_to = 'year', values_to = 'gdp_bra') %>% 
  mutate(country = 'Brazil')


arg = gdp_capita %>% 
  filter(country == 'Argentina') %>% 
  select(!country) %>% 
  pivot_longer(cols = everything(), names_to = 'year', values_to = 'gdp_arg') %>% 
  mutate(country = 'Argentina')


peru = gdp_capita %>% 
  filter(country == 'Peru') %>% 
  select(!country) %>% 
  pivot_longer(cols = everything(), names_to = 'year', values_to = 'gdp_peru') %>% 
  mutate(country = 'Peru')



so_amer = inner_join(bra, chile, by = 'year') %>% 
  inner_join(arg, by = 'year') %>% 
  inner_join(peru, by = 'year')
  #select(year, gdp_chile = gdp.y, gdp_bra = gdp.x, gdp_arg = gdp.x.x, gdp_peru = gdp.y.y)

current_gdp_cap = so_amer %>% 
  filter(year %in% '2019')

so_amer %>% ggplot(aes(x = year, y = gdp_chile, group = 1)) +
  geom_line(aes(y = gdp_chile, col = country.y), size = 1.25) +
  geom_line(aes(y = gdp_bra, col = country.x), size = 1.25) +
  geom_line(aes(y = gdp_arg, col = country.x.x), size = 1.25) +
  geom_line(aes(y = gdp_peru, col = country.y.y), size = 1.25) +
  scale_x_discrete(breaks = seq(1960, 2020, by = 10)) +
  geom_point(data = current_gdp_cap, aes(x = year, y = gdp_chile), color = 'red', size = 3) +
  geom_label_repel(data = current_gdp_cap, aes(label = gdp_chile), hjust = 1.3, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   label.size = 1,
                   segment.color = 'grey50', size = 5) 
  labs(title = "GDP per capita",
       x = "Year",
       y = "GDP per capita",
       subtitle = 'Data Source: Gapminder',
       color = "") +
  theme(plot.title=element_text(size=20,face="bold", vjust=1.5, lineheight=1.6),
        axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"), axis.title.x = element_text(color="black", vjust= 1), axis.title.y = element_text(color="black", vjust= 1),
        legend.text = element_text(size = 12, face = 'b'))

# GDP Projection Chile
gdp = read_csv('data/chile-gdp.csv')

gdp = gdp %>% pivot_longer(cols = everything(), names_to = 'year', values_to = 'gdp') 

highlight = gdp %>% 
  filter(year %in% c('2002', '2006', '2020', '2024'))

gdp_plot = gdp %>% ggplot(aes(x = year, y = gdp, group = 1)) +
  geom_line(aes(y = gdp), col = 'darkgreen', size = 1.25) +
  scale_x_discrete(breaks = seq(1950, 2024, by = 5)) +
  geom_point(data = highlight, aes(x = year, y = gdp), color = 'red', size = 3) +
  geom_label_repel(data = highlight, aes(label = gdp), hjust = 1.3, 
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   label.size = 1,
                   segment.color = 'grey50', size = 5) +
  labs(title = "GDP in Chile (1980 - 2024)",
       x = 'Year',
       y = "GDP (billions in USD)",
       subtitle = 'Data Source: International Monetary Fund') +
  theme_economist() +
  theme(plot.title=element_text(size=20,face="bold", vjust=1.5, lineheight=1.6),
        axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"), axis.title.x = element_text(color="black", vjust= 1),
        axis.title.y = element_text(color="black", vjust= 1)) +
    theme(aspect.ratio = 1)


